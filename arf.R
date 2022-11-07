#' Adversarial random forest
#' 
#' Implements an adversarial RF to learn independence-inducing splits.
#' 
#' @param x Input data. 
#' @param delta Tolerance parameter. Algorithm converges when OOB accuracy is
#'   < 0.5 plus \code{delta}.
#' @param num_trees Number of trees to grow. 
#' @param min_node_size Minimum size for terminal nodes.
#' @param parallel Compute in parallel? Must register backend beforehand.
#' @param ... Extra parameters to be passed to \code{ranger}.
#'
#' 
#' @import ranger 
#' @import data.table
#' @importFrom foreach foreach %do% %dopar%
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


#' Forests for density estimation
#' 
#' Uses a pre-trained ARF model to estimate leaf and distribution parameters.
#' 
#' @param arf Pre-trained adversarial random forest.
#' @param x_trn Training data for estimating parameters.
#' @param x_tst Optional test data. If supplied, the function computes 
#'   log-likelihoods on test data (measured in nats).
#' @param alpha Smoothing parameter for categorical data. Ensures that levels 
#'   have zero probability only if splits command it.
#'
#' @import ranger 
#' @import data.table
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom truncnorm dtruncnorm 
#' @importFrom matrixStats colSds
#' 

forde <- function(arf, x_trn, x_tst = NULL, alpha = 0.01) {
  # Prelimz
  x <- as.data.frame(x_trn)
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
  num_trees <- arf$num.trees
  # Compute leaf bounds
  bnds <- foreach(b = 1:num_trees, .combine = rbind) %do% {
    tree_df <- as.data.table(treeInfo(arf, b))
    num_nodes <- nrow(tree_df)
    lb <- matrix(-Inf, nrow = num_nodes, ncol = d)
    ub <- matrix(Inf, nrow = num_nodes, ncol = d)
    colnames(lb) <- colnames(ub) <- colnames(x)
    for (i in 1:num_nodes) {
      left_child <- tree_df[i, leftChild + 1L]
      right_child <- tree_df[i, rightChild + 1L]
      splitvar_name <- tree_df[i, splitvarName]
      splitval <- tree_df[i, splitval]
      if (!is.na(left_child)) {
        ub[left_child, ] <- ub[right_child, ] <- ub[i, ]
        lb[left_child, ] <- lb[right_child, ] <- lb[i, ]
        ub[left_child, splitvar_name] <- splitval
        lb[right_child, splitvar_name] <- splitval
      }
    }
    b_leaves <- tree_df[terminal == TRUE, nodeID + 1L]
    cbind(b, b_leaves, lb[b_leaves, ], ub[b_leaves, ])
  }
  colnames(bnds)[1:2] <- c('tree', 'leaf')
  lo <- as.data.table(bnds[, 1:(2 + d)])
  hi <- as.data.table(bnds[, c(1:2, (2 + d + 1):(2 + 2 * d))])
  lo <- melt(lo, id.vars = c('tree', 'leaf'), value.name = 'min')
  hi <- melt(hi, id.vars = c('tree', 'leaf'), value.name = 'max')
  bnds <- merge(lo, hi, by = c('tree', 'leaf', 'variable'))
  # Get terminal nodes for all observations
  pred <- predict(arf, x, type = 'terminalNodes')$predictions + 1L
  # Enumerate leaves
  leaves <- unique(bnds[, .(tree, leaf)])
  leaves[, cvg := sum(pred[, tree] == leaf) / n, by = .(tree, leaf)]
  bnds <- merge(bnds, leaves[cvg > 0], by = c('tree', 'leaf'))
  
  # Compute parameters for each leaf
  psi_cnt <- psi_cat <- NULL
  psi_fn <- function(i) {
    # Localize
    b <- leaves[i, tree]
    l <- leaves[i, leaf]
    bnds_bl <- bnds[tree == b & leaf == l]
    idx <- pred[, b] == l
    # Calculate mean and std dev for continuous features
    if (any(!factor_cols)) {
      vars <- colnames(x)[!factor_cols]
      x_leaf <- as.matrix(x[idx, !factor_cols, drop = FALSE])
      psi_cnt <- data.table(
        'variable' = vars, 
        'mu' = colMeans(x_leaf), 
        'sigma' = colSds(x_leaf),
        'value' = NA_character_, 'prob' = NA_real_
      )
    } 
    # Calculate class probabilities for categorical features
    if (any(factor_cols)) {
      psi_cat <- foreach(j = which(factor_cols), .combine = rbind) %do% {
        k <- length(levels(x[[j]]))
        xj_leaf <- x[idx, j]
        if (bnds_bl[variable == colnames(x)[j], max - min] <= 1) {
          alpha <- 0
        }
        pr <- (table(xj_leaf) + alpha) / (length(xj_leaf) + alpha * k)
        data.table(
          'variable' = colnames(x)[j], 
          'mu' = NA_real_, 'sigma' = NA_real_,
          'value' = levels(x[[j]]), 
          'prob' = as.numeric(pr)
        )
      }
    } 
    # Put it all together, export
    psi <- merge(rbind(psi_cnt, psi_cat), bnds_bl, by = 'variable')
    psi <- psi[, .(tree, leaf, cvg, variable, min, max, mu, sigma, value, prob)]
    return(psi)
  }
  # Loop over leaves in parallel
  psi <- foreach(ii = leaves[, which(cvg > 0)], .combine = rbind) %dopar% psi_fn(ii)
  # BS hack for zero-variance points (this will be resolved with min.bucket)
  psi[is.na(prob) & is.na(sigma), sigma := 0.01]
  psi[sigma == 0, sigma := 0.01]
  # Optionally prep test data
  if (!is.null(x_tst)) {
    x <- as.data.frame(x_tst)
    n <- nrow(x_tst)
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
    pred <- predict(arf, x, type = 'terminalNodes')$predictions + 1L
  }
  # Compute log-likelihood
  loglik <- foreach(i = 1:n, .combine = c) %dopar% {
    tree_lik <- foreach(b = 1:num_trees, .combine = c) %do% {
      cvg_b <- leaves[tree == b & leaf == pred[i, b], cvg]
      if (cvg_b == 0) {
        ll_b <- 0
      } else {
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
        ll_b <- sum(j_lik) * cvg_b
      }
      return(ll_b)
    }
    return(mean(tree_lik))
  }
  # Export
  out <- list('psi' = psi, 'loglik' = loglik)
  return(out)
}


#' Forests for generative modelling
#' 
#' Uses pre-trained FORDE model to simulate synthetic data.
#' 
#' @param psi Parameters learned via FORDE. 
#' @param m Number of synthetic samples to generate
#'
#' @import data.table
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom truncnorm rtruncnorm 
#' 

forge <- function(psi, m) {
  # Draw random leaves with probability proportional to coverage
  omega <- unique(psi[, .(tree, leaf, cvg)])
  omega[, pr := cvg / max(tree)][, idx := .I]
  draws <- sample(omega$idx, size = m, replace = TRUE, prob = omega$pr)
  psi_idx <- foreach(i = draws, .combine = rbind) %do% {
    psi[tree == omega[idx == i, tree] & leaf == omega[idx == i, leaf]]
  }
  length_psi_i <- nrow(psi) / nrow(omega)
  psi_idx[, idx := rep(1:m, each = length_psi_i)]
  # Simulate data
  synth_cnt <- synth_cat <- NULL
  if (any(is.na(psi$prob))) {  # Continuous
    psi_cnt <- psi_idx[is.na(prob)]
    psi_cnt[, dat := rtruncnorm(nrow(psi_cnt), a = min, b = max,
                                mean = mu, sd = sigma)]
    synth_cnt <- dcast(psi_cnt, idx ~ variable, value.var = 'dat')
    synth_cnt[, idx := NULL]
  }
  if (any(!is.na(psi$prob))) { # Categorical
    psi_cat <- psi_idx[!is.na(prob)]
    psi_cat[, dat := sample(value, 1, prob = prob), by = .(idx, variable)]
    synth_cat <- dcast(unique(psi_cat[, .(idx, variable, dat)]), 
                       idx ~ variable, value.var = 'dat')
    synth_cat[, idx := NULL]
  }
  # Export
  x_synth <- cbind(synth_cnt, synth_cat)
  return(x_synth)
}



