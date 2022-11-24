library(data.table)
library(ranger)
library(truncnorm)
library(MASS)
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
#' @param max_iters Maximum iterations.
#' @param parallel Fit forest in parallel?
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
    max_iters = 10,
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
  if (isTRUE(parallel)) {
    rf0 <- ranger(y ~ ., dat, keep.inbag = TRUE, classification = TRUE, 
                  num.trees = num_trees, min.node.size = min_node_size, 
                  respect.unordered.factors = TRUE, ...)
  } else {
    rf0 <- ranger(y ~ ., dat, keep.inbag = TRUE, classification = TRUE, 
                  num.trees = num_trees, min.node.size = min_node_size, 
                  respect.unordered.factors = TRUE, num.threads = 1, ...)
  }
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
      if (isTRUE(parallel)) {
        rf1 <- ranger(y ~ ., dat, keep.inbag = TRUE, classification = TRUE, 
                      num.trees = num_trees, min.node.size = min_node_size, 
                      respect.unordered.factors = TRUE, ...)
      } else {
        rf1 <- ranger(y ~ ., dat, keep.inbag = TRUE, classification = TRUE, 
                      num.trees = num_trees, min.node.size = min_node_size, 
                      respect.unordered.factors = TRUE, num.threads = 1, ...)
      }
      
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
    pred <- predict(rf0, x, type = 'terminalNodes')$predictions + 1
    
    # Prune leaves without real data, i.e. zero coverage, or with only one obs. (cannot estimate sd)
    for (tree in 1:num_trees) {
      leaves <- which(rf0$forest$child.nodeIDs[[tree]][[1]] == 0)
      to_prune <- leaves[!(leaves %in% which(tabulate(pred[, tree]) > 1))]
      
      while(length(to_prune) > 0) {
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
#' @param dist Distribution to use for density estimation of continuous features.. 
#'   Default is truncated normal. 
#' @param epsilon Slack parameter on empirical bounds when \code{dist = "unif"}.
#'   This avoids zero-density points when test data falls outside the support
#'   of training data. The gap between lower and upper bounds is expanded by 
#'   a factor of \code{epsilon}. Only matters when a variable is never used for
#'   splits.
#' @param prune Was pruning applied to the input \code{arf}?
#' @param loglik Return log-likelihood of training or test data? If \code{FALSE},
#'   function only returns leaf and distribution parameters \code{psi}. 
#' @param batch Batch size. The default is to compute parameters for the full 
#'   dataset in one round, which is always the fastest option if memory allows. 
#'   However, with large samples and/or many trees, it is more memory efficient 
#'   to split the data into batches. This has no impact on results.
#' @param parallel Compute parameters in parallel?
#'
#' @import ranger 
#' @import data.table
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom truncnorm dtruncnorm 
#' 

forde <- function(arf, x_trn, x_tst = NULL, dist = 'truncnorm', epsilon = 0.1, 
                  prune = TRUE, loglik = TRUE, batch = NULL, parallel = TRUE) {
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
  if (class(arf) == 'list') {
    num_trees <- length(arf)
    pred <- do.call('cbind', lapply(arf, function(f) {
      predict(f, x, type = 'terminalNodes')$predictions + 1
    }))
    bnd_fn <- function(tree) {
      num_nodes <- length(arf[[tree]]$forest$split.varIDs[[1]])
      lb <- matrix(-Inf, nrow = num_nodes, ncol = d)
      ub <- matrix(Inf, nrow = num_nodes, ncol = d)
      if (dist == 'unif') {
        if (!is.null(x_tst)) {
          x_tst <- as.data.frame(x_tst)
        }
        for (j in seq_len(d)) {
          if (factor_cols[j] == FALSE) {
            min_j <- min(c(min(x[[j]]), min(x_tst[[j]])))
            max_j <- max(c(max(x[[j]]), max(x_tst[[j]])))
            gap <- max_j - min_j
            lb[, j] <- min_j - epsilon/2 * gap
            ub[, j] <- max_j + epsilon/2 * gap
          }
        }
      }
      for (i in 1:num_nodes) {
        left_child <- arf[[tree]]$forest$child.nodeIDs[[1]][[1]][i] + 1
        right_child <- arf[[tree]]$forest$child.nodeIDs[[1]][[2]][i] + 1
        splitvarID <- arf[[tree]]$forest$split.varIDs[[1]][i] + 1
        splitval <- arf[[tree]]$forest$split.value[[1]][i]
        if (left_child > 1 & left_child != right_child) {
          ub[left_child, ] <- ub[right_child, ] <- ub[i, ]
          lb[left_child, ] <- lb[right_child, ] <- lb[i, ]
          ub[left_child, splitvarID] <- lb[right_child, splitvarID] <- splitval
        }
      }
      leaves <- which(arf[[tree]]$forest$child.nodeIDs[[1]][[1]] == 0) 
      colnames(lb) <- arf[[tree]]$forest$independent.variable.names
      colnames(ub) <- arf[[tree]]$forest$independent.variable.names
      merge(melt(data.table(tree = tree, leaf = leaves, lb[leaves, ]), 
                 id.vars = c('tree', 'leaf'), value.name = 'min'), 
            melt(data.table(tree = tree, leaf = leaves, ub[leaves, ]), 
                 id.vars = c('tree', 'leaf'), value.name = 'max'), 
            by = c('tree', 'leaf', 'variable'))
    }
  } else {
    num_trees <- arf$num.trees
    pred <- predict(arf, x, type = 'terminalNodes')$predictions + 1
    bnd_fn <- function(tree) {
      num_nodes <- length(arf$forest$split.varIDs[[tree]])
      lb <- matrix(-Inf, nrow = num_nodes, ncol = d)
      ub <- matrix(Inf, nrow = num_nodes, ncol = d)
      if (dist == 'unif') {
        if (!is.null(x_tst)) {
          x_tst <- as.data.frame(x_tst)
        }
        for (j in seq_len(d)) {
          if (factor_cols[j] == FALSE) {
            min_j <- min(c(min(x[[j]]), min(x_tst[[j]])))
            max_j <- max(c(max(x[[j]]), max(x_tst[[j]])))
            gap <- max_j - min_j
            lb[, j] <- min_j - epsilon/2 * gap
            ub[, j] <- max_j + epsilon/2 * gap
          }
        }
      }
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
  }
  if (isTRUE(parallel)) {
    bnds <- foreach(tree = 1:num_trees, .combine = rbind) %dopar% bnd_fn(tree)
  } else {
    bnds <- foreach(tree = 1:num_trees, .combine = rbind) %do% bnd_fn(tree)
  }
  bnds[, cvg := sum(pred[, tree] == leaf) / n, by = .(tree, leaf)]
  if (!isTRUE(prune)) { 
    bnds[cvg == 1/n, cvg := 0]
  }
  
  psi_cnt <- psi_cat <- NULL
  # Calculate mean and std dev for continuous features
  if (any(!factor_cols)) {
    psi_cnt_fn <- function(tree) {
      dt <- data.table(tree = tree, x[, !factor_cols, drop = FALSE], leaf = pred[, tree])
      long <- melt(dt, id.vars = c('tree', 'leaf'))
      if (dist %in% c('norm', 'truncnorm')) {
        long[, list(cat = NA_character_, prob = NA_real_, mu = mean(value), sigma = sd(value),
                    type = 'cnt'), 
             by = .(tree, leaf, variable)]
      } else if (dist == 'beta') {
        long[, list(cat = NA_character_, prob = NA_real_, mu = mean(value), s2 = var(value),
                    type = 'cnt'), 
             by = .(tree, leaf, variable)]
        long[, alpha := ((1 - mu) / s2 - 1 / mu) * mu^2]
        long[, beta := alpha * (1 / mu - 1)]
        long[, c('mu', 's2') := NULL]
      } else if (dist != 'unif') {
        long[, as.list(MASS::fitdistr(value, dist)$estimate), by = .(tree, leaf, variable)]
        long[, type := 'cnt']
      }
    }
    if (isTRUE(parallel)) {
      psi_cnt <- foreach(tree = 1:num_trees, .combine = rbind) %dopar% psi_cnt_fn(tree)
    } else {
      psi_cnt <- foreach(tree = 1:num_trees, .combine = rbind) %do% psi_cnt_fn(tree)
    }
  }
  # Calculate class probabilities for categorical features
  if (any(factor_cols)) {
    psi_cat_fn <- function(tree) {
      dt <- data.table(tree = tree, x[, factor_cols, drop = FALSE], leaf = pred[, tree])
      long <- melt(dt, id.vars = c('tree', 'leaf'), value.factor = FALSE, value.name = 'cat')
      long[, count := .N, by = .(tree, leaf, variable)]
      unique(setDT(long)[, list(prob = .N/count, mu = NA_real_, sigma = NA_real_, type = 'cat'), 
                         by = .(tree, leaf, variable, cat)])
    }
    if (isTRUE(parallel)) {
      psi_cat <- foreach(tree = 1:num_trees, .combine = rbind) %dopar% psi_cat_fn(tree)
    } else {
      psi_cat <- foreach(tree = 1:num_trees, .combine = rbind) %do% psi_cat_fn(tree)
    }
  } 
  
  # NEED: EITHER KEEP CONTINUOUS/CATEGORICAL PSI SEPARATE OR CREATE SOME SMARTER
  # RBIND WHERE RELEVANT FEATURES ARE ADDED AND KEPT NA. ON A RELATED TOPIC, 
  # SHOULD WE ALLOW DIFFERENT DISTRIBUTIONS PER FEATURE? MAKES LIFE A LITTLE
  # COMPLICATED BUT ARGUABLY MAKES SENSE.
  
  psi_tmp <- rbind(psi_cnt, psi_cat)
  if (is.null(psi_tmp)) {
    psi <- bnds
    psi[, mu := NA_real_][, sigma := NA_real_][, type := 'cnt']
  } else {
    psi <- merge(psi_tmp, bnds, by = c('tree', 'leaf', 'variable'))
  }
  rm(psi_cnt, psi_cat, psi_tmp)
  
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
      pred <- predict(arf, x, type = 'terminalNodes')$predictions + 1
    }
    
    # Optional batch index
    if (!is.null(batch)) {
      k <- round(n / batch)
      batch_idx <- suppressWarnings(split(1:n, seq_len(k)))
    } else {
      k <- 1L
      batch_idx <- list(1:n)
    }
    # Compute per-feature likelihoods
    psi_x_cnt <- psi_x_cat <- NULL
    
    ### BATCHING BEGINS HERE AND GOES RIGHT THROUGH TO LOGLIK CALCULATION ###
    loglik_fn <- function(fold) {
      # Predictions
      preds <- rbindlist(lapply(1:ncol(pred), function(b) {
        data.table(tree = b, leaf = pred[batch_idx[[fold]], b], obs = batch_idx[[fold]])
      }))
      # Continuous data
      if (any(!factor_cols)) {
        x_long_cnt <- melt(
          data.table(obs = batch_idx[[fold]], 
                     x[batch_idx[[fold]], !factor_cols, drop = FALSE]), 
          id.vars = 'obs'
        )
        preds_x_cnt <- merge(preds, x_long_cnt, by = 'obs', allow.cartesian = TRUE)
        psi_x_cnt <- merge(psi[type == 'cnt', .(tree, leaf, cvg, variable, min, max, mu, sigma)], 
                           preds_x_cnt, by = c('tree', 'leaf', 'variable'))
        if (dist == 'truncnorm') {
          psi_x_cnt[, lik := dtruncnorm(value, a = min, b = max, mean = mu, sd = sigma)]
        } else if (dist == 'norm') {
          psi_x_cnt[, lik := dnorm(value, mean = mu, sd = sigma)]
        } else if (dist == 'beta') {
          psi_x_cnt[, lik := dbeta(value, shape1 = alpha, shape2 = beta)]
        } else if (dist == 'unif') {
          psi_x_cnt[, lik := dunif(value, min = min, max = max)]
        } else if (dist == 'exponential') {
          psi_x_cnt[, lik := dexp(value, rate = rate)]
        } else if (dist == 'geometric') {
          psi_x_cnt[, lik := dgeom(value, prob = prob)]
        } else if (dist %in% c('log-normal', 'lognormal', 'lnorm')) {
          psi_x_cnt[, lik := dlnorm(value, meanlog = meanlog, sdlog = sdlog)]
        } else if (dist == 'Poisson') {
          psi_x_cnt[, lik := dpois(value, lambda = lambda)]
        }
        psi_x_cnt <- psi_x_cnt[, .(tree, obs, cvg, lik)]
        rm(x_long_cnt, preds_x_cnt)
      }
      # ADD NORMALIZATION CONSTANT FOR UNTRUNCATED VARIABLES
      
      # Categorical data
      if (any(factor_cols)) {
        x_long_cat <- melt(
          data.table(obs = batch_idx[[fold]], 
                     x[batch_idx[[fold]], factor_cols, drop = FALSE]), 
          id.vars = 'obs', value.name = 'cat'
        )
        preds_x_cat <- merge(preds, x_long_cat, by = 'obs', allow.cartesian = TRUE)
        psi_x_cat <- merge(psi[type == 'cat', .(tree, leaf, cvg, variable, cat, prob)], 
                           preds_x_cat, by = c('tree', 'leaf', 'variable', 'cat'), 
                           allow.cartesian = TRUE)
        psi_x_cat[, lik := prob]
        psi_x_cat <- psi_x_cat[, .(tree, obs, cvg, lik)]
        rm(x_long_cat, preds_x_cat)
      } 
      rm(preds)
      # Put it together
      psi_x <- rbind(psi_x_cnt, psi_x_cat)
      rm(psi_x_cnt, psi_x_cat)
      # Compute per-sample log-likelihoods
      loglik <- unique(psi_x[, prod(lik) * cvg, by = .(obs, tree)])
      loglik[is.na(V1), V1 := 0]
      loglik <- loglik[, log(mean(V1)), by = obs]
      return(loglik)
    }
    if (k == 1L) {
      ll <- loglik_fn(1)
    } else {
      if (isTRUE(parallel)) {
        ll <- foreach(fold = 1:k, .combine = rbind) %dopar% loglik_fn(fold)
      } else {
        ll <- foreach(fold = 1:k, .combine = rbind) %do% loglik_fn(fold)
      }
    }
    loglik <- ll[order(obs), V1]
  } else {
    loglik <- NULL
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
#' @param dist Distribution to use for random sampling. Default is truncated
#'   normal. 
#'
#' @import data.table
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom truncnorm rtruncnorm 
#' 

forge <- function(psi, m, dist = 'truncnorm') {
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
    if (dist == 'truncnorm') {
      psi_cnt[, dat := rtruncnorm(nrow(psi_cnt), a = min, b = max,
                                  mean = mu, sd = sigma)]
    } else if (dist == 'norm') {
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

# Batch option for FORDE?
# Other exponential family options, with normalization constant
# psi could be a list with d elements, one for each feature
# Idea: let each tree have its own synthetic dataset
# Add KDE option?




