
library(ranger)

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
    params <- lapply(1:num_trees, function(tree) {
      unique_nodeids <- unique(nodeids[, tree])
      res <- lapply(unique_nodeids, function(nodeid) {
        apply(x_real[which(pred[, tree] == nodeid), !factor_cols, drop = FALSE], 2, function(x) {
          MASS::fitdistr(x, dist)$estimate
        })
      })
      names(res) <- unique_nodeids
      res
    })
  }
  
  # Calculate class probabilities for categorical data in all used terminal nodes
  # class_probs dims: [[tree]][[nodeid]][[colname]][class probs]
  if (any(factor_cols)) {
    class_probs <- lapply(1:num_trees, function(tree) {
      unique_nodeids <- unique(nodeids[, tree])
      res <- lapply(unique_nodeids, function(nodeid) {
        lapply(x_real[which(pred[, tree] == nodeid), factor_cols, drop = FALSE], function(x) {
         table(x)
        })
      })
      names(res) <- unique_nodeids
      res
    })
  }
 
  # Sample new data from mixture distribution over trees
  data_new <- data.frame(matrix(NA, nrow = n_new, ncol = p))
  for (i in 1:n_new) {
    # Randomly select tree for each obs. (mixture distribution with equal prob.)
    tree <- sample(num_trees, 1)
    
    # Sample from distribution in terminal node
    for (j in 1:p) {
      colname <- names(factor_cols)[j]
      nodeid <- as.character(nodeids[i, tree])
      if (factor_cols[j]) {
        # Factor columns: Multinomial distribution
        draw <- rmultinom(1, 1, prob = class_probs[[tree]][[nodeid]][[colname]])
        data_new[i, j] <- rownames(draw)[draw == 1]
      } else {
        # Continuous columns: Match estimated distribution parameters with r...() function
        if (dist == "normal") {
          data_new[i, j] <- rnorm(1, mean = params[[tree]][[nodeid]]["mean", colname], 
                                  sd = params[[tree]][[nodeid]]["sd", colname])
        } else if (dist == "exponential") {
          data_new[i, j] <- rexp(1, params[[tree]][[nodeid]][colname])
        } else if (dist == "geometric") {
          data_new[i, j] <- rgeom(1, params[[tree]][[nodeid]][colname])
        } else if (dist %in% c("log-normal", "lognormal")) {
          data_new[i, j] <- rlnorm(1, meanlog = params[[tree]][[nodeid]]["meanlog", colname], 
                                   sdlog = params[[tree]][[nodeid]]["sdlog", colname])
        } else if (dist == "Poisson") {
          data_new[i, j] <- rpois(1, params[[tree]][[nodeid]][colname])
        } else {
          stop("Unknown distribution.")
        }
      }
    }
  }
  
  colnames(data_new) <- colnames(x_real)
  data_new[, factor_cols] <- lapply(data_new[, factor_cols, drop = FALSE], factor)
  
  # Return synthetic data
  data_new
}