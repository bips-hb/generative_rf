
library(ranger)
library(foreach)
library(data.table)

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
generative_ranger_datatable <- function(x_real, x_synth = NULL, n_new, oob = FALSE, 
                              dist = "normal", ...) {
  
  # Convert to data.frame
  orig_colnames <- colnames(x_real)
  x_real <- data.frame(x_real)
  p <- ncol(x_real) 
  
  # Convert chars and logicals to factors
  idx_char <- sapply(x_real, is.character)
  if (any(idx_char)) {
    x_real[, idx_char] <- as.data.frame(lapply(x_real[, idx_char], as.factor))
  }
  idx_logical <- sapply(x_real, is.logical)
  if (any(idx_logical)) {
    x_real[, idx_logical] <- as.data.frame(lapply(x_real[, idx_logical], as.factor))
  }
  
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
  
  mean_sd_fun <- function(x) {
    list(mean = mean(x), sd = sd(x))
  }
  
  # Fit continuous distribution in all used terminal nodes
  # params dims: [[tree]][[nodeid]][[colname]][distr. parameters]
  if (any(!factor_cols)) {
    params <- foreach(tree = 1:num_trees, .combine = rbind) %dopar% { 
      dt <- data.table(tree = tree, x_real[, !factor_cols], nodeid = pred[, tree])
      long <- melt(dt, id.vars = c("tree", "nodeid"))
      mean_sd <- long[, mean_sd_fun(value), by = .(tree, nodeid, variable)]
      #probs_dt <- data.table(nodeid = 1:nrow(probs), prob = probs[, tree])
      #merge(mean_sd, probs_dt, by = "nodeid")
      mean_sd
    }
  }

  # Calculate class probabilities for categorical data in all used terminal nodes
  # class_probs dims: [[tree]][[nodeid]][[colname]][class probs]
  if (any(factor_cols)) {
    class_probs <- foreach(tree = 1:num_trees, .combine = rbind) %dopar% { 
      dt <- data.table(tree = tree, x_real[, factor_cols], nodeid = pred[, tree])
      long <- melt(dt, id.vars = c("tree", "nodeid"), value.factor = TRUE)
      setDT(long)[, .N, by = .(tree, nodeid, variable, value)]
    }
  }
  
  # Sample new observations and get their terminal nodes
  # nodeids dims: [new obs, tree]
  nodeids <- apply(probs, 2, function(x) {
    sample(length(x), n_new, replace = TRUE, prob = x)
  })
  
  # Randomly select tree for each new obs. (mixture distribution with equal prob.)
  sampled_trees <- sample(num_trees, n_new, replace = TRUE)
  sampled_nodes <- sapply(1:n_new, function(i) {
    nodeids[i, sampled_trees[i]]
  })
  sampled_trees_nodes <- data.table(obs = 1:n_new, tree = sampled_trees, nodeid = sampled_nodes)
  
  # Get distributions parameters for each new obs.
  if (any(!factor_cols)) {
    obs_params <- merge(sampled_trees_nodes, params, by = c("tree", "nodeid"), sort = FALSE)
  }
  
  # Get probabilities for each new obs.
  if (any(factor_cols)) {
    obs_probs <- merge(sampled_trees_nodes, class_probs, by = c("tree", "nodeid"), sort = FALSE)
  }
  
  # Sample new data from mixture distribution over trees
  data_new <- foreach (j = 1:p, .combine = data.frame) %dopar% {
    colname <- names(factor_cols)[j]
    
    if (factor_cols[j]) {
      # Factor columns: Multinomial distribution
      obs_probs[variable == colname, sample(value, 1, prob = N), by = obs]$V1
    } else {
      # Continuous columns: Match estimated distribution parameters with r...() function
      if (dist == "normal") {
        #browser()
        rnorm(n = n_new, mean = obs_params[variable == colname, mean], 
              sd = obs_params[variable == colname, sd])
      } else if (dist == "exponential") {
        rexp(n = n_new, obs_params[colname, ]) #FIXME
      } else if (dist == "geometric") {
        rgeom(n = n_new, obs_params[colname, ]) #FIXME
      } else if (dist %in% c("log-normal", "lognormal")) {
        rlnorm(n = n_new, meanlog = obs_params["meanlog", colname, ], 
               sdlog = obs_params["sdlog", colname, ]) #FIXME
      } else if (dist == "Poisson") {
        rpois(n = n_new, obs_params[colname, ]) #FIXME
      } else {
        stop("Unknown distribution.")
      }
    }
  }
  
  # Convert chars and logicals back
  if (any(idx_char)) {
    data_new[, idx_char] <- as.data.frame(lapply(data_new[, idx_char], as.character))
  }
  if (any(idx_logical)) {
    data_new[, idx_logical] <- as.data.frame(lapply(data_new[, idx_logical], function(x) {x == "TRUE"}))
  }
  
  # Use original column names
  colnames(data_new) <- orig_colnames
  
  # Return synthetic data
  data_new
}