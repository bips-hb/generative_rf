# Load libraries
library(data.table)
library(ranger)
library(doMC)
registerDoMC(8)

# Autoencoder-based generative model
fae_gen <- function(
  x, 
  m,
  num_trees = 100, 
  min_node_size = 5, 
  parallel = TRUE,
  ...) {
  # Prelimz
  x <- as.data.frame(x)
  n <- nrow(x)
  idx_char <- sapply(x, is.character)
  if (any(idx_char)) {
    x[, idx_char] <- as.data.frame(
      lapply(x[, idx_char, drop = FALSE], as.factor)
    )
  }
  idx_logical <- sapply(x, is.logical)
  if (any(idx_logical)) {
    x[, idx_logical] <- as.data.frame(
      lapply(x[, idx_logical, drop = FALSE], as.factor)
    )
  }
  factor_cols <- sapply(x, is.factor)
  # Precompute global bounds
  bnds_cnt <- bnds_cat <- NULL
  if (any(!factor_cols)) {
    non_fctrs <- colnames(x)[!factor_cols]
    x_cnt <- as.matrix(x[, !factor_cols, drop = FALSE])
    d_cnt <- ncol(x_cnt)
    bnds_cnt <- data.table(
      'nodeID' = NA_integer_,
      'variable' = rep(colnames(x_cnt), times = 2),
      'value' = c(apply(x_cnt, 2, min), apply(x_cnt, 2, max)),
      'bound' = rep(c('lo', 'hi'), each = d_cnt)
    )
  }
  if (any(factor_cols)) {
    fctrs <- colnames(x)[factor_cols]
    x_cat <- x[, factor_cols, drop = FALSE]
    d_cat <- ncol(x_cat)
    bnds_cat <- data.table(
      'nodeID' = NA_integer_,
      'variable' = rep(colnames(x_cat), times = 2),
      'value' = c(rep(0, times = d_cat), 
                  sapply(seq_along(x_cat), function(j) length(levels(x_cat[, j])))),
      'bound' = rep(c('lo', 'hi'), each = d_cat)
    )
  }
  bounds <- rbind(bnds_cnt, bnds_cat)
  # Tree growing function (each tree gets its own synthetic data)
  grow_tree <- function(b) {
    # Draw data, fit model
    x_real <- x[sample(n, replace = TRUE), , drop = FALSE]
    x_synth <- as.data.frame(lapply(x, function(xj) {
      sample(xj, n, replace = TRUE)
    }))
    dat <- rbind(data.table(y = 1, x_real),
                 data.table(y = 0, x_synth))
    f <- ranger(y ~ ., dat, classification = TRUE, num.trees = 1,
                replace = FALSE, min.node.size = min_node_size, 
                respect.unordered.factors = TRUE, ...)
    return(f)
  }
  if (isTRUE(parallel)) {
    rf <- foreach(bb = 1:num_trees) %dopar% grow_tree(bb)
  } else {
    rf <- foreach(bb = 1:num_trees) %do% grow_tree(bb)
  }
  # Extract node IDs and tree info
  nodeIDs <- do.call('cbind', lapply(rf, function(f) {
    predict(f, x, type = 'terminalNodes')$predictions
  }))
  tree_df <- do.call('rbind', lapply(1:num_trees, function(b) {
    as.data.table(treeInfo(rf[[b]]))[, tree := b]
  }))
  # Leaf parameters
  theta_fn <- function(b) {
    tree <- tree_df[tree == b]
    leaves <- unique(nodeIDs[, b])
    path_fn <- function(l) {
      node <- l
      path <- tree[nodeID == l]
      path[, bound := NA_character_]
      while(node > 0L) {
        tmp <- tree[leftChild == node | rightChild == node, ]
        tmp[, bound := ifelse(leftChild == node, 'hi', 'lo')]
        path <- rbind(tmp, path)
        node <- tmp$nodeID
      }
      path <- na.omit(path[, .(nodeID, splitvarName, splitval, bound)])
      colnames(path)[2:3] <- c('variable', 'value')
      path <- rbind(bounds, path)
      # Reduce
      inf <- path[bound == 'lo', max(value), by = variable]
      sup <- path[bound == 'hi', min(value), by = variable]
      out <- merge(inf, sup, by = 'variable')
      colnames(out)[2:3] <- c('min', 'max')
      out[, leaf := l][, tree := b]
      out <- out[, .(tree, leaf, variable, min, max)]
      return(out)
    }
    out <- foreach(ll = leaves, .combine = rbind) %do% path_fn(ll)
    return(out)
  }
  theta_df <- foreach(bb = 1:num_trees, .combine = rbind) %dopar% theta_fn(bb)
  # Post-process factor variables
  if (any(factor_cols)) {
    for (j in fctrs) {
      rng <- 1:length(levels(x[[j]]))
      for (b in 1:num_trees) {
        lvls <- rf[[b]]$forest$covariate.levels[[j]]
        theta_df[tree == b & variable == j, 
                 values := paste(lvls[rng >= min & rng <= max], collapse = ', '), 
                 by = leaf]
      }
      theta_df[variable == j, min := NA_real_]
      theta_df[variable == j, max := NA_real_]
    }
  } else {
    theta_df[, values := NA_character_]
  }
  # Sample function
  cands_cat <- cands_cnt <- NULL
  sample_fn <- function(i) {
    # Shuffle trees
    trees <- sample(num_trees)
    # Start the Markov chain
    b <- 1L
    tree_id <- trees[b]
    cvg <- table(nodeIDs[, tree_id]) / n
    l <- as.numeric(sample(names(cvg), 1, prob = cvg))
    theta <- theta_df[tree == tree_id & leaf == l, .(variable, min, max, values)]
    # Add links
    while(b < num_trees) {
      b <- b + 1L
      tree_id <- trees[b]
      theta_tmp <- theta_df[tree == tree_id]
      # Remove leaves with no overlap
      if (any(factor_cols)) {
        cands_cat <- lapply(fctrs, function(j) {
          v <- strsplit(theta[variable == j, values], ', ')[[1]]
          v_tmp <- strsplit(theta_tmp[variable == j, values], ', ')
          keep <- sapply(seq_along(v_tmp), function(ll) {
            any(v %in% v_tmp[[ll]])
          })
          theta_tmp[variable == j, leaf][keep]
        })
      }
      if (any(!factor_cols)) {
        cands_cnt <- lapply(non_fctrs, function(j) {
          lo <- theta[variable == j, min]
          hi <- theta[variable == j, max]
          theta_tmp[variable == j & min < hi & max > lo, leaf]
        })
      }
      cands <- Reduce(intersect, c(cands_cat, cands_cnt))
      # Sample from remaining candidates
      samples <- nodeIDs[nodeIDs[, tree_id] %in% cands, tree_id]
      cvg <- table(samples) / length(samples)
      l <- as.numeric(sample(names(cvg), 1, prob = cvg))
      # Merge, reduce
      theta <- rbind(
        theta, 
        theta_df[tree == tree_id & leaf == l, .(variable, min, max, values)]
      )
    }
    # Synthesize
    out_cat <- out_cnt <- NULL
    if (any(factor_cols)) {
      theta_cat <- theta[variable %in% fctrs]
      theta_cat[, values := Reduce(intersect, strsplit(values, ', ')), by = variable]
      theta_cat <- unique(theta_cat)
      # Sample uniformly for categorical
      out_cat <- lapply(fctrs, function(j) {
        v <- strsplit(theta_cat[variable == j, values], ', ')[[1]]
        sample(v, size = 1)
      })
      names(out_cat) <- fctrs
    }
    if (any(!factor_cols)) {
      inf <- theta[variable %in% non_fctrs, max(min), by = variable]
      sup <- theta[variable %in% non_fctrs, min(max), by = variable]
      theta_cnt <- merge(inf, sup, by = 'variable')
      colnames(theta_cnt)[2:3] <- c('min', 'max')
      # Take midpoint for continuous
      out_cnt <- lapply(non_fctrs, function(j) {
        theta_cnt[variable == j, (min + max) / 2]
      })
      names(out_cnt) <- non_fctrs
    }
    xi <- as.data.frame(c(out_cat, out_cnt))
    return(xi)
  }
  out <- foreach(ii = 1:m, .combine = rbind) %dopar% sample_fn(ii)
  return(out)
}

    
    
    


