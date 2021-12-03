
# Things to try: 
# * Use only OOB data
# * Include synthetic data for probability estimation
# * How to benchmark generative models?
# * How can iteration (run again with x_synth=x_new from previous run) make sense?

library(ranger)

#' Generative Random Forests
#'
#' @param x_real Original data (data.frame).
#' @param x_synth Naive synthetic data, if NULL will be sampled from marginals.
#' @param n_new Number of synthetic observations to sample.
#' @param ... Passed on to the ranger() call; use for 'num.trees', 'min.node.size', etc.
#'
#' @return data.frame with synthetic data.
#' @export
#'
#' @examples
#' generative_ranger(x_real = iris, n_new = 100, num.trees = 50)
generative_ranger <- function(x_real, x_synth = NULL, n_new, ...) {
  
  p <- ncol(x_real) 
  factor_cols <- sapply(x_real, is.factor)
  factor_col_names <- names(factor_cols)[factor_cols]
  
  # If no synthetic data provided, sample from marginals
  x_synth <- as.data.frame(lapply(x_real, function(x) {
    sample(x, length(x), replace = TRUE)
  }))
  
  # Merge real and synthetic data
  dat <- rbind(data.frame(y = 0, x_real), 
               data.frame(y = 1, x_synth))
  
  # Fit ranger to both data
  rf <- ranger(y ~ ., dat, keep.inbag = TRUE, classification = TRUE, ...)
  
  # Get terminal nodes for all observations
  pred <- predict(rf, x_real, type = "terminalNodes")$predictions
  
  # Sample new observations and their terminal nodes
  probs <- apply(pred, 2, function(x) {
    tabulate(x, nbins = max(pred))/length(x)
  })
  probs[probs <= 1/nrow(x_real)] <- 0 # Avoid terminal nodes with just one obs
  nodeids <- apply(probs, 2, function(x) {
    sample(length(x), n_new, replace = TRUE, prob = x)
  })
  
  # Non-factor cols: Sample from normal distribution
  # For each observation, get means and sds for each variable from terminal node
  mean_sd_trees <- simplify2array(lapply(1:rf$num.trees, function(tree) {
    pt <- pred[, tree]
    nd <- nodeids[, tree]
    
    means <- sapply(unique(pt), function(t) {
      apply(x_real[which(pt == t), !factor_cols, drop = FALSE], 2, mean, na.rm = TRUE)
    })
    if (!is.matrix(means)) {
      means <- matrix(means, nrow = 1)
    }
    colnames(means) <- unique(pt)
    sds <- sapply(unique(pt), function(t) {
      apply(x_real[which(pt == t), !factor_cols, drop = FALSE], 2, sd, na.rm = TRUE)
    })
    if (!is.matrix(sds)) {
      sds <- matrix(sds, nrow = 1)
    }
    colnames(sds) <- unique(pt)
    
    mean_sd <- simplify2array(lapply(nd, function(x) {
      cbind(mean = means[, as.character(x)], sd = sds[, as.character(x)])
    }))
    mean_sd
  }))
  # Dims: vars, mean/sd, obs, trees
  
  # Factor cols: Sample from multinomial distribution
  # For each observation, get class probs. for each variable from terminal node
  class_probs_trees <- lapply(factor_col_names, function(col) {
    simplify2array(lapply(1:rf$num.trees, function(tree) {
      pt <- pred[, tree]
      nd <- nodeids[, tree]
      
      probs_nd <- sapply(unique(pt), function(t) {
        table(x_real[which(pt == t), col, drop = FALSE])
      })
      colnames(probs_nd) <- unique(pt)
      
      probs_obs <- sapply(nd, function(x) {
        probs_nd[, as.character(x)]
      })
      probs_obs
    }))
  })
  names(class_probs_trees) <- factor_col_names
  # Dims: vars, levels, obs, trees
  
  # Sample new data from mixture distribution over trees
  data_new <- data.frame(matrix(NA, nrow = n_new, ncol = p))
  
  for (i in 1:n_new) {
    # Randomly select tree for each obs. (mixture distribution with equal prob.)
    tree <- sample(ncol(pred), 1)
    for (j in 1:p) {
      if (factor_cols[j]) {
        colname <- names(factor_cols)[j]
        draw <- rmultinom(1, 1, prob = class_probs_trees[[colname]][, i, tree])
        data_new[i, j] <- rownames(draw)[draw == 1]
      } else {
        data_new[i, j] <- rnorm(1,
                                mean = mean_sd_trees[j, "mean", i, tree], 
                                sd = mean_sd_trees[j, "sd", i, tree])
      }
      
    }
  }
  
  colnames(data_new) <- colnames(x_real)
  data_new[, factor_cols] <- lapply(data_new[, factor_cols, drop = FALSE], factor)
  
  # Return synthetic data
  data_new
}