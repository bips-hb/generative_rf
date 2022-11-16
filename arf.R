library(data.table)
library(ranger)
library(truncnorm)
library(doMC)
registerDoMC(8)

#' Adversarial random forest
#' 
#' Implements an adversarial RF to learn independence-inducing splits.
#' 
#' @param x Input data. 
#' @param delta Tolerance parameter. Algorithm converges when OOB accuracy is
#'   < 0.5 plus \code{delta}.
#' @param num_trees Number of trees to grow. 
#' @param min_node_size Minimum size for terminal nodes.
#' @param prune Prune leaves with insufficient real data? 
#' @param parallel Compute in parallel? Must register backend beforehand.
#' @param max_iters Maximum iterations.
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
    prune = TRUE,
    parallel = TRUE,
    max_iters = 10,
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
  if (acc > 0.5 + delta & iters < max_iters) {
    converged <- FALSE
    while (!isTRUE(converged)) {
      nodeIDs <- predict(rf0, x_real, type = 'terminalNodes')$predictions
      # Create synthetic data
      trees <- sample(1:num_trees, n, replace = TRUE)
      leaves <- sapply(1:n, function(i) sample(nodeIDs[, trees[i]], 1))
      
      
      x_synth <- foreach(i = 1:n, .combine = rbind) %do% {
        tree <- sample(1:num_trees, 1)
        leaf <- sample(nodeIDs[, tree], 1)
        leaf_idx <- nodeIDs[, tree] == leaf
        as.data.frame(lapply(x_real[leaf_idx, ], function(x) sample(x, 1)))
      }
      # Merge real and synthetic data
      dat <- rbind(data.frame(y = 1L, x_real),
                   data.frame(y = 0L, x_synth))
      # Train unsupervised random forest
      rf1 <- ranger(y ~ ., dat, keep.inbag = TRUE, classification = TRUE, 
                    num.trees = num_trees, min.node.size = min_node_size, 
                    respect.unordered.factors = TRUE, ...)
      # Evaluate
      acc <- 1 - rf1$prediction.error
      if (acc <= 0.5 + delta | iters >= max_iters) {
        converged <- TRUE
      } else {
        rf0 <- rf1
        iters <- iters + 1L
      }
    }
  }
  
  if (isTRUE(prune)) {
    # Get terminal nodes for all observations
    pred <- predict(rf0, x, type = 'terminalNodes')$predictions + 1L
    
    # Prune leaves without real data, i.e. zero coverage, or with only one obs. (cannot estimate sd)
    for (tree in 1:num_trees) {
      leaves <- which(rf0$forest$child.nodeIDs[[tree]][[1]] == 0)
      to_prune <- leaves[!(leaves %in% which(tabulate(pred[, tree]) > 1))]
      
      while(length(to_prune) > 0) {
        #message('Tree ', tree, ', Pruning ', paste(to_prune, collapse = ' '))
        for (tp in to_prune) {
          # Find parents
          parent <- which((rf0$forest$child.nodeIDs[[tree]][[1]] + 1) == tp)
          if (length(parent) > 0) {
            # Left child
            rf0$forest$child.nodeIDs[[tree]][[1]][parent] <- rf0$forest$child.nodeIDs[[tree]][[2]][parent]
          } else {
            # Right child
            parent <- which((rf0$forest$child.nodeIDs[[tree]][[2]] + 1) == tp)
            rf0$forest$child.nodeIDs[[tree]][[2]][parent] <- rf0$forest$child.nodeIDs[[tree]][[1]][parent]
          }
        }
        to_prune <- which((rf0$forest$child.nodeIDs[[tree]][[1]] + 1) %in% to_prune)
      }
    }
  }
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
#' @param prune Was pruning applied to the input \code{arf}?
#' @param loglik Return log-likelihood of training or test data? If \code{FALSE},
#'   function only returns leaf and distribution paramters \code{psi}. 
#' @param truncate Apply truncated Gaussian to ensure integration to unity?
#'
#' @import ranger 
#' @import data.table
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom truncnorm dtruncnorm 
#' 

forde <- function(arf, x_trn, x_tst = NULL, prune = TRUE, loglik = TRUE, truncate = TRUE) {
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
  
  # Compute leaf bounds and coverage
  num_trees <- arf$num.trees
  pred <- predict(arf, x, type = 'terminalNodes')$predictions + 1L
  bnds <- foreach(tree = 1:num_trees, .combine = rbind) %dopar% {
    num_nodes <- length(arf$forest$split.varIDs[[tree]])
    lb <- matrix(-Inf, nrow = num_nodes, ncol = d)
    ub <- matrix(Inf, nrow = num_nodes, ncol = d)
    for (i in 1:num_nodes) {
      left_child <- arf$forest$child.nodeIDs[[tree]][[1]][i] + 1
      right_child <- arf$forest$child.nodeIDs[[tree]][[2]][i] + 1
      splitvarID <- arf$forest$split.varIDs[[tree]][i] + 1
      splitval <- arf$forest$split.value[[tree]][i]
      if (left_child > 1 & left_child != right_child) {
        ub[left_child, ] <- ub[right_child, ] <- ub[i, ]
        lb[left_child, ] <- lb[right_child, ] <- lb[i, ]
        ub[left_child, splitvarID] <- lb[right_child, splitvarID] <- splitval
      }
    }
    leaves <- which(arf$forest$child.nodeIDs[[tree]][[1]] == 0) 
    colnames(lb) <- arf$forest$independent.variable.names
    colnames(ub) <- arf$forest$independent.variable.names
    merge(melt(data.table(tree = tree, leaf = leaves, lb[leaves, ]), 
               id.vars = c('tree', 'leaf'), value.name = 'min'), 
          melt(data.table(tree = tree, leaf = leaves, ub[leaves, ]), 
               id.vars = c('tree', 'leaf'), value.name = 'max'), 
          by = c('tree', 'leaf', 'variable'))
  }
  if (!isTRUE(prune)) {
    bnds[, num := sum(pred[, tree] == leaf), by = .(tree, leaf)]
    bnds <- bnds[num > 1][, num := NULL]
  }
  bnds[, cvg := sum(pred[, tree] == leaf) / n, by = .(tree, leaf)]
  
  psi_cnt <- psi_cat <- NULL
  # Calculate mean and std dev for continuous features
  if (any(!factor_cols)) {
    psi_cnt <- foreach(tree = 1:num_trees, .combine = rbind) %dopar% { 
      dt <- data.table(tree = tree, x[, !factor_cols, drop = FALSE], leaf = pred[, tree])
      long <- melt(dt, id.vars = c('tree', 'leaf'))
      long[, list(cat = NA_character_, prob = NA_real_, mu = mean(value), sigma = sd(value)), 
           by = .(tree, leaf, variable)]
    }
  }
  # Calculate class probabilities for categorical features
  if (any(factor_cols)) {
    psi_cat <- foreach(tree = 1:num_trees, .combine = rbind) %dopar% { 
      dt <- data.table(tree = tree, x[, factor_cols, drop = FALSE], leaf = pred[, tree])
      long <- melt(dt, id.vars = c('tree', 'leaf'), value.factor = FALSE, value.name = 'cat')
      long[, count := .N, by = .(tree, leaf, variable)]
      unique(setDT(long)[, list(prob = .N/count, mu = NA_real_, sigma = NA_real_), 
                         by = .(tree, leaf, variable, cat)])
    }
  } 
  psi <- merge(rbind(psi_cnt, psi_cat), bnds, by = c('tree', 'leaf', 'variable'))
  rm(psi_cnt, psi_cat)
  
  if (isTRUE(loglik)) {
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
    # Compute per-feature likelihoods
    psi_x_cnt <- psi_x_cat <- NULL
    preds <- rbindlist(lapply(1:ncol(pred), function(i) {
      data.table(tree = i, obs = 1:nrow(pred), leaf = pred[, i])
    }))
    if (any(!factor_cols)) {
      x_long_cnt <- melt(data.table(obs = 1:nrow(x), x[, !factor_cols, drop = FALSE]), id.vars = 'obs')
      preds_x_cnt <- merge(preds, x_long_cnt, by = 'obs', allow.cartesian = TRUE)
      psi_x_cnt <- merge(psi[!is.na(sigma), .(tree, leaf, cvg, variable, min, max, mu, sigma)], 
                         preds_x_cnt, by = c('tree', 'leaf', 'variable'))
      if (isTRUE(truncate)) {
        psi_x_cnt[, lik := dtruncnorm(value, a = min, b = max, mean = mu, sd = sigma)]
      } else {
        psi_x_cnt[, lik := dnorm(value,  mean = mu, sd = sigma)]
      }
      psi_x_cnt <- psi_x_cnt[, .(tree, obs, cvg, lik)]
    } 
    rm(x_long_cnt, preds_x_cnt)
    
    if (any(factor_cols)) {
      x_long_cat <- melt(data.table(obs = 1:nrow(x), x[, factor_cols, drop = FALSE]), 
                         id.vars = 'obs', value.name = 'cat')
      preds_x_cat <- merge(preds, x_long_cat, by = 'obs', allow.cartesian = TRUE)
      psi_x_cat <- merge(psi[!is.na(cat), .(tree, leaf, cvg, variable, cat, prob)], 
                         preds_x_cat, by = c('tree', 'leaf', 'variable', 'cat'), 
                         allow.cartesian = TRUE)
      psi_x_cat[, lik := prob]
      psi_x_cat <- psi_x_cat[, .(tree, obs, cvg, lik)]
    } 
    rm(x_long_cat, preds_x_cat, preds)
    
    psi_x <- rbind(psi_x_cnt, psi_x_cat)
    rm(psi_x_cnt, psi_x_cat)
    
    # Compute per-sample log-likelihoods, export
    loglik <- psi_x[, prod(lik) * cvg, by = .(obs, tree)]
    loglik <- loglik[, log(mean(V1)), by = obs]
    loglik <- loglik[order(obs), V1]
  } else {
    loglik <- NULL
  }
  
  out <- list('psi' = psi, 'loglik' = loglik)
  return(out)
}


#' Forests for generative modelling
#' 
#' Uses pre-trained FORDE model to simulate synthetic data.
#' 
#' @param psi Parameters learned via FORDE. 
#' @param m Number of synthetic samples to generate
#' @param truncate Apply truncated Gaussian to ensure integration to unity?
#'
#' @import data.table
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom truncnorm rtruncnorm 
#' 

forge <- function(psi, m, truncate = TRUE) {
  # Draw random leaves with probability proportional to coverage
  omega <- unique(psi[, .(tree, leaf, cvg)])
  omega[, pr := cvg / max(tree)][, idx := .I]
  draws <- sample(omega$idx, size = m, replace = TRUE, prob = omega$pr)
  psi_idx <- foreach(i = 1:m, .combine = rbind) %do% {
    id <- draws[i]
    out <- psi[tree == omega[idx == id, tree] & leaf == omega[idx == id, leaf]]
    out[, idx := i]
  }
  # Simulate data
  synth_cnt <- synth_cat <- NULL
  if (any(is.na(psi$prob))) {  # Continuous
    psi_cnt <- psi_idx[is.na(prob)]
    if (isTRUE(truncate)) {
      psi_cnt[, dat := rtruncnorm(nrow(psi_cnt), a = min, b = max,
                                  mean = mu, sd = sigma)]
    } else {
      psi_cnt[, dat := rnorm(nrow(psi_cnt), mean = mu, sd = sigma)]
    }
    synth_cnt <- dcast(psi_cnt, idx ~ variable, value.var = 'dat')
    synth_cnt[, idx := NULL]
  }
  if (any(!is.na(psi$prob))) { # Categorical
    psi_cat <- psi_idx[!is.na(prob)]
    psi_cat[, dat := sample(cat, 1, prob = prob), by = .(idx, variable)]
    synth_cat <- dcast(unique(psi_cat[, .(idx, variable, dat)]), 
                       idx ~ variable, value.var = 'dat')
    synth_cat[, idx := NULL]
  }
  # Export
  x_synth <- cbind(synth_cnt, synth_cat)
  return(x_synth)
}


