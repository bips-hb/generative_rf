
library(ranger)
library(foreach)

#' Generative Random Forests
#'
#' @param x_real Original data (data.frame).
#' @param x_synth Naive synthetic data, if NULL will be sampled from marginals.
#' @param n_new Number of synthetic observations to sample.
#' @param oob Use only out-of-bag data to calculate leaf probabilities?
#' @param dist Distribution to fit in terminal nodes to continuous data. Currently implemented: "normal", "exponential", "geometric", "lognormal", "Poisson".
#' @param ... Passed on to the ranger() call; use for 'num.trees', 'min.node.size', etc.
#'
#' @return data.frame with synthetic data.
#' @export
#'
#' @examples
#' generative_ranger(x_real = iris, n_new = 100, num.trees = 50)
generative_ranger <- function(x_real, x_synth = NULL, n_new, oob = FALSE, 
                              dist = "normal", ...) {
  
  x_real <- data.frame(x_real)
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
  num_trees <- ncol(pred)
  
  # If OOB, use only OOB trees
  if (oob) {
    inbag <- (do.call(cbind, rf$inbag.counts) > 0)[1:nrow(x_real), ]
    pred[inbag] <- NA
  }
  
  # Get probabilities of terminal nodes for each tree 
  # probs dims: [nodeid, tree]
  probs <- apply(pred, 2, function(x) {
    tab <- tabulate(x, nbins = max(pred, na.rm = TRUE))
    tab[tab == 1] <- 0 # Avoid terminal nodes with just one obs
    tab/sum(tab)
  })

  # Sample new observations and get their terminal nodes
  # nodeids dims: [new obs, tree]
  nodeids <- apply(probs, 2, function(x) {
    sample(length(x), n_new, replace = TRUE, prob = x)
  })
  
  # Fit continuous distribution in all used terminal nodes
  # params dims: [[tree]][[nodeid]][[colname]][distr. parameters]
  if (any(!factor_cols)) {
    params <- foreach(tree=1:num_trees) %dopar% { 
      unique_nodeids <- unique(nodeids[, tree])
      if (dist == "normal") {
        # Use faster analytical version for normal distribution
        res <- lapply(unique_nodeids, function(nodeid) {
          idx <- which(pred[, tree] == nodeid)
          sapply(x_real[idx, !factor_cols, drop = FALSE], function(x) {
            c(mean = mean(x), sd = sd(x)) # Or use MLE (1/n)?
          })
        })
      } else {
        res <- lapply(unique_nodeids, function(nodeid) {
          idx <- which(pred[, tree] == nodeid)
          apply(x_real[idx, !factor_cols, drop = FALSE], 2, function(x) {
            MASS::fitdistr(x, dist)$estimate
          })
        })
      }
      names(res) <- unique_nodeids
      res
    }
  }
  
  # Calculate class probabilities for categorical data in all used terminal nodes
  # class_probs dims: [[tree]][[nodeid]][[colname]][class probs]
  if (any(factor_cols)) {
    class_probs <- foreach(tree=1:num_trees) %dopar% { 
      unique_nodeids <- unique(nodeids[, tree])
      res <- lapply(unique_nodeids, function(nodeid) {
        idx <- which(pred[, tree] == nodeid)
        lapply(x_real[idx, factor_cols, drop = FALSE], function(x) {
         tabulate(x, nbins = nlevels(x))
        })
      })
      names(res) <- unique_nodeids
      res
    }
  }
  
  # Randomly select tree for each new obs. (mixture distribution with equal prob.)
  sampled_trees <- sample(num_trees, n_new, replace = TRUE)
  sampled_nodes <- sapply(1:n_new, function(i) {
    as.character(nodeids[i, sampled_trees[i]])
  })
 
  # Get distributions parameters for each new obs.
  if (any(!factor_cols)) {
    obs_params <- sapply(1:n_new, function(i) {
      params[[sampled_trees[i]]][[sampled_nodes[i]]]
    }, simplify = "array")
  }
  
  # Sample new data from mixture distribution over trees
  data_new <- foreach (j = 1:p, .combine = data.frame) %dopar% {
    colname <- names(factor_cols)[j]
    
    if (factor_cols[j]) {
      # Factor columns: Multinomial distribution
      draws <- sapply(1:n_new, function(i) {
        which(rmultinom(n = 1, size = 1, prob = class_probs[[sampled_trees[i]]][[sampled_nodes[i]]][[colname]]) == 1)
      })
      factor(levels(x_real[, j])[draws])
    } else {
      # Continuous columns: Match estimated distribution parameters with r...() function
      if (dist == "normal") {
        rnorm(n = n_new, mean = obs_params["mean", colname, ], 
              sd = obs_params["sd", colname, ])
      } else if (dist == "exponential") {
        rexp(n = n_new, obs_params[colname, ])
      } else if (dist == "geometric") {
        rgeom(n = n_new, obs_params[colname, ])
      } else if (dist %in% c("log-normal", "lognormal")) {
        rlnorm(n = n_new, meanlog = obs_params["meanlog", colname, ], 
               sdlog = obs_params["sdlog", colname, ])
      } else if (dist == "Poisson") {
        rpois(n = n_new, obs_params[colname, ])
      } else {
        stop("Unknown distribution.")
      }
    }
  }
  colnames(data_new) <- colnames(x_real)
  
  # Return synthetic data
  data_new
}