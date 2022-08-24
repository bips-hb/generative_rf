# Load libraries
library(data.table)
library(ranger)
library(matrixStats)
library(truncnorm)
library(doMC)
registerDoMC(8)

# Adversarial RF function
adversarial_rf <- function(
    x, 
    delta = 0,
    oob = FALSE, 
    num_trees = 10, 
    min_node_size = 10, 
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
  psi_cnt <- psi_cat <- NULL
  ### PART I: TRAIN FOREST ###
  cells <- list(1:n)
  h <- 0
  cells_final <- list()
  h_final <- double()
  # Tree growing function
  tree_grow <- function(b) {
    x_tmp <- x[sample(n, replace = TRUE), ]
    while (length(cells) > 0) {
      a <- cells[[1]]
      if (length(a) < min_node_size | h[1] >= 1 - delta) {
        cells_final[[length(cells_final) + 1]] <- a
        h_final[length(h_final) + 1] <- h[1]
        cells <- cells[-1]
        h <- h[-1]
      } else {
        n_a <- length(a)
        x_a <- x_tmp[a, ]
        # Synthesize
        x_synth <- as.data.frame(lapply(x_a, function(x) {
          sample(x, nrow(x_a), replace = TRUE)
        }))
        dat <- rbind(data.table(y = 1, x_a),
                     data.table(y = 0, x_synth))
        # Fit
        f <- ranger(y ~ ., dat, classification = TRUE, num.trees = 1, 
                    max.depth = 1, replace = FALSE, sample.fraction = 1)
        # Evaluate, append
        dat[, node_id := predict(f, dat, type='terminalNodes')$predictions]
        if (dat[, sum(node_id)] == 0) {
          h[1] <- 1
        } else {
          p1 <- dat[node_id == 1, sum(y) / .N]
          p2 <- dat[node_id == 2, sum(y) / .N]
          h <- c(h[-1], 4 * p1 * (1 - p1), 4 * p2 * (1 - p2))
          cells <- c(cells[-1], split(a, dat[y == 1, node_id]))
          l_idx <- length(h)
          if (p1 == 0) {
            h <- h[-(l_idx - 1)]
            cells <- cells[-(l_idx - 1)]
          } else if (p2 == 0) {
            h <- h[-l_idx]
            cells <- cells[-l_idx]
          }
        }
      }
    }
    # Leaf parameters
    n_leaves <- length(cells_final)
    n_per_leaf <- sapply(cells_final, length)
    leaves <- data.table('tree' = b, 
                         'leaf' = 1:n_leaves, 
                         'cvg' = n_per_leaf / n, 
                         'loss' = 1 - h_final)
    # Calculate mu and sigma for continuous features in each leaf
    if (any(!factor_cols)) {
      vars <- colnames(x)[!factor_cols]
      psi_cnt <- foreach(leaf = 1:n_leaves, .combine = rbind) %do% {
        x_leaf <- as.matrix(x_tmp[cells_final[[leaf]], !factor_cols, drop = FALSE])
        data.table('leaf' = leaf, 'variable' = vars, 'type' = 'continuous',
                   'mu' = colMeans(x_leaf), 'sigma' = colSds(x_leaf),
                   'max' = apply(x_leaf, 2, max), 'min' = apply(x_leaf, 2, min),
                   'value' = NA, 'prob' = NA)
      }
    } 
    # Calculate class probabilities for categorical features in each leaf
    if (any(factor_cols)) {
      psi_cat <- foreach(leaf = 1:n_leaves, .combine = rbind) %:%
        foreach (j = which(factor_cols), .combine = rbind) %do% {
          xj_leaf <- x_tmp[cells_final[[leaf]], j]
          values <- levels(xj_leaf)
          probs <- as.numeric(table(xj_leaf) / n_per_leaf[leaf])
          data.table('leaf' = leaf, 'variable' = colnames(x)[j], type = 'categorical',
                     'mu' = NA, 'sigma' = NA, 'max' = NA, 'min' = NA,
                     'value' = values, 'prob' = probs)
      }
    } 
    psi <- rbind(psi_cnt, psi_cat)
    out <- merge(leaves, psi, by = 'leaf')
    return(out)
  }
  # Merge trees, export results
  f <- foreach(bb = 1:num_trees, .combine = rbind) %dopar% tree_grow(bb)
  return(f)
}

forge <- function(adv_rf, n_out, trunc = FALSE) {
  df_cnt <- df_cat <- NULL
  # Sample target leaves
  leaves <- unique(adv_rf[, .(tree, leaf, cvg)])
  leaf_idx <- sample(1:nrow(leaves), n_out, replace = TRUE, 
                     prob = leaves$cvg / max(leaves$tree))
  leaves_out <- leaves[leaf_idx, .(tree, leaf)]
  # Sample continuous features
  if (any(adv_rf[, type == 'continuous'])) {
    d_cnt <- adv_rf[type == 'continuous', length(unique(variable))]
    df_cnt <- merge(leaves_out, adv_rf[type == 'continuous', ], 
                    by = c('tree', 'leaf'), allow.cartesian = TRUE)
    n_tmp <- nrow(df_cnt)
    if (trunc == TRUE) {
      df_cnt[, synth := rtruncnorm(n_tmp, a = min, b = max, mean = mu, sd = sigma)]
    } else {
      df_cnt[, synth := rnorm(n_tmp, mean = mu, sd = sigma)]
    }
    df_cnt[is.na(synth), synth := mu]
    df_cnt <- df_cnt[, .(variable, synth)]
    df_cnt[, id := rep(1:n_out, each = d_cnt)]
    df_cnt <- dcast(df_cnt, id ~ variable, value.var = 'synth')[, id := NULL]
  } 
  # Sample categorical features
  if (any(adv_rf[, type == 'categorical'])) {
    n_cat <- nrow(unique(adv_rf[type == 'categorical', .(variable, value)]))
    df_cat <- merge(leaves_out, adv_rf[type == 'categorical', ],
                    by = c('tree', 'leaf'), allow.cartesian = TRUE)
    df_cat[, synth := sample(value, size = .N / length(value), prob = prob), 
           by = .(variable, tree, leaf)]
    df_cat <- df_cat[, .(variable, synth)]
    df_cat[, id := rep(1:n_out, each = n_cat)]
    df_cat <- unique(df_cat)
    df_cat <- dcast(df_cat, id ~ variable, value.var = 'synth')[, id := NULL]
  }
  out <- cbind(df_cnt, df_cat)
  return(out)
}






