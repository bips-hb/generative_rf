#' Adversarial random forest
#' 
#' Implements an adversarial RF to learn distribution parameters.
#' 
#' @param x Input data. 
#' @param delta Tolerance parameter. Algorithm converges when OOB accuracy is
#'   < 0.5 plus \code{delta}.
#' @param num_trees Number of trees to grow. 
#' @param min_node_size Minimum size for terminal nodes.
#' @param parallel Compute in parallel? Must register backend beforehand.
#' @param ... Extra parameters to be passed to \code{ranger}.
#'
#' @details
#' 
#' 
#' @return
#' 
#' 
#' @import ranger 
#' @importFrom foreach foreach %dopar%
#' @importFrom data.table data.table melt setDT
#' @importFrom truncnorm dtruncnorm rtruncnorm
#' @importFrom matrixStats colSds
#'

adversarial_rf <- function(
    x, 
    delta = 0,
    num_trees = 10, 
    min_node_size = 5, 
    parallel = TRUE,
    ...) {
  # Identify factors, if any
  x_real <- as.data.frame(x)
  n <- nrow(x_real)
  idx_char <- sapply(x_real, is.character)
  if (any(idx_char)) {
    x_real[, idx_char] <- as.data.frame(
      lapply(x_real[, idx_char, drop = FALSE], as.factor)
    )
  }
  idx_logical <- sapply(x_real, is.logical)
  if (any(idx_logical)) {
    x_real[, idx_logical] <- as.data.frame(
      lapply(x_real[, idx_logical, drop = FALSE], as.factor)
    )
  }
  factor_cols <- sapply(x_real, is.factor)
  # Sample from marginals to get naive synthetic data
  x_synth <- as.data.frame(lapply(x_real, function(x) {
    sample(x, length(x), replace = TRUE)
  }))
  # Merge real and synthetic data
  dat <- rbind(data.frame(y = 1L, x_real),
               data.frame(y = 0L, x_synth))
  # Train unsupervised random forest
  rf0 <- ranger(y ~ ., dat, keep.inbag = TRUE, classification = TRUE, 
                num.trees = num_trees, min.node.size = min_node_size, 
                respect.unordered.factors = TRUE, ...)
  # Optionally recurse
  iters <- 1L
  acc <- 1 - rf0$prediction.error
  if (acc > 0.5 + delta) {
    converged <- FALSE
    while (!isTRUE(converged)) {
      nodeIDs <- predict(rf0, x_real, type = 'terminalNodes')$predictions
      # Create synthetic data
      x_synth <- foreach(i = 1:n, .combine = rbind) %dopar% {
        tree <- sample(1:num_trees, 1)
        leaf <- sample(nodeIDs[, tree], 1)
        leaf_idx <- nodeIDs[, tree] == leaf
        as.data.frame(lapply(x_real[leaf_idx, ], function(x) sample(x, 1)))
      }
      # Merge real and synthetic data
      dat <- rbind(data.frame(y = 1, x_real),
                   data.frame(y = 0, x_synth))
      # Train unsupervised random forest
      rf1 <- ranger(y ~ ., dat, keep.inbag = TRUE, classification = TRUE, 
                    num.trees = num_trees, min.node.size = min_node_size, 
                    respect.unordered.factors = TRUE, ...)
      # Evaluate
      acc <- 1 - rf1$prediction.error
      if (acc <= 0.5 + delta) {
        converged <- TRUE
      } else {
        rf0 <- rf1
        iters <- iters + 1L
      }
    }
  }
  # Now move forward with rf0
  return(rf0)
}

# Density estimator
forde <- function(arf, x, alpha = 0.01) {
  # Identify factors, if any
  x <- as.data.frame(x)
  n <- nrow(x)
  d <- ncol(x)
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
  
  # Extract leaf bounds
  bnds_cnt <- bnds_cat <- NULL
  if (any(!factor_cols)) {
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
  
  # Get terminal nodes for all observations
  pred <- predict(arf, x, type = 'terminalNodes')$predictions
  
  # Complete leaf list
  leaves <- foreach(b = 1:num_trees, .combine = rbind) %do% {
    out <- as.data.table(treeInfo(arf, b))[, tree := b]
    out <- out[!is.na(prediction), .(tree, nodeID)]
    colnames(out)[2] <- 'leaf'
    out[, cvg := sum(pred[, b] == leaf) / n, by = leaf]
    out <- out[cvg > 0]
    return(out)
  }
  psi_cnt <- psi_cat <- NULL
  
  # Compute parameters for each leaf
  psi_fn <- function(i) {
    # Localize
    b <- leaves[i, tree]
    l <- leaves[i, leaf]
    idx <- pred[, b] == l
    # Trace the path for each leaf to get extrema
    tree <- as.data.table(treeInfo(arf, b))
    path <- tree[nodeID == l]
    path[, bound := NA_character_]
    node <- l
    while(node > 0) {
      tmp <- tree[leftChild == node | rightChild == node, ]
      tmp[, bound := ifelse(leftChild == node, 'hi', 'lo')]
      path <- rbind(tmp, path)
      node <- tmp$nodeID
    }
    path <- na.omit(path[, .(nodeID, splitvarName, splitval, bound)])
    colnames(path)[2:3] <- c('variable', 'value')
    path <- rbind(path, bounds)
    inf <- path[bound == 'lo', max(value), by = variable]
    sup <- path[bound == 'hi', min(value), by = variable]
    psi1 <- merge(inf, sup, by = 'variable')
    colnames(psi1)[c(2, 3)] <- c('min', 'max')
    # Calculate mean and std dev for continuous features
    if (any(!factor_cols)) {
      vars <- colnames(x)[!factor_cols]
      x_leaf <- as.matrix(x[idx, !factor_cols, drop = FALSE])
      psi2 <- data.table(
        'variable' = vars, 
        'mu' = colMeans(x_leaf), 
        'sigma' = colSds(x_leaf),
        'value' = NA_character_, 'prob' = NA_real_
      )
      psi_cnt <- merge(psi1, psi2, by = 'variable')
    } 
    # Calculate class probabilities for categorical features
    if (any(factor_cols)) {
      psi_cat <- foreach(j = which(factor_cols), .combine = rbind) %do% {
        k <- length(levels(x[[j]]))
        xj_leaf <- x[idx, j]
        if (psi1[variable == colnames(x)[j], max - min] <= 1) {
          alpha <- 0
        }
        pr <- (table(xj_leaf) + alpha) / (length(xj_leaf) + alpha * k)
        data.table(
          'variable' = colnames(x)[j], 
          'min' = NA_real_, 'max' = NA_real_,
          'mu' = NA_real_, 'sigma' = NA_real_,
          'value' = levels(x[[j]]), 
          'prob' = as.numeric(pr)
        )
      }
    } 
    # Put it all together, export
    psi <- rbind(psi_cnt, psi_cat)
    psi[, tree := b][, leaf := l]
    psi <- psi[, .(tree, leaf, variable, min, max, mu, sigma, value, prob)]
    return(psi)
  }
  # Loop over leaves in parallel
  psi <- foreach(ii = 1:nrow(leaves), .combine = rbind) %dopar% psi_fn(ii)
  # Remove fake bounds
  if (any(!factor_cols)) {
    for (j in colnames(x)[!factor_cols]) {
      min_j <- bounds[variable == j & bound == 'lo', value]
      max_j <- bounds[variable == j & bound == 'hi', value]
      psi[variable == j & min == min_j, min := -Inf]
      psi[variable == j & max == max_j, max := Inf]
    }
  }
  
  # BS hack for zero-variance points (this will be resolved with min.bucket)
  psi[is.na(prob) & is.na(sigma), sigma := 0.01]
  psi[sigma == 0, sigma := 0.01]
  
  # Now compute densities
  loglik <- foreach(i = 1:n, .combine = c) %dopar% {
    tree_lik <- foreach(b = 1:num_trees, .combine = c) %do% {
      psi_l <- psi[tree == b & leaf == pred[i, b]]
      j_lik <- sapply(1:d, function(j) {
        psi_j <- psi_l[variable == colnames(x)[j]]
        if (j %in% which(!factor_cols)) {
          ll_j <- log(dtruncnorm(x[i, j], a = psi_j$min, b = psi_j$max, 
                               mean = psi_j$mu, sd = psi_j$sigma))
        } else {
          ll_j <- psi_j[value == x[i, j], log(prob)]
        }
        return(ll_j)
      })
      out <- sum(j_lik) * leaves[tree == b & leaf == pred[i, b], cvg]
      return(out)
    }
    out <- mean(tree_lik)
    return(out)
  }
  
  # Export
  out <- list('psi' = psi, 'loglik' = loglik)
  return(out)
  
}



























