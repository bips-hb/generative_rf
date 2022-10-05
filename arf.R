#' Adversarial random forest
#' 
#' Implements an adversarial RF to learn distribution parameters
#' 
#' @param x Input data. 
#' @param epsilon Tolerance parameter. Algorithm converges when OOB accuracy is
#'   < 0.5 plus \code{epsilon}.
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
#' @import data.table
#' @import ranger 
#' @import foreach
#'

arf <- function(
    x, 
    epsilon = 0.1,
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
  if (acc > 0.5 + epsilon) {
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
      if (acc <= 0.5 + epsilon) {
        converged <- TRUE
      } else {
        rf0 <- rf1
        iters <- iters + 1L
      }
    }
  }
  # Now move forward with rf0
  
  # Get terminal nodes for all observations
  pred <- predict(rf0, x_real, type = "terminalNodes")$predictions
  
  # Get probabilities of terminal nodes for each tree
  node_probs <- apply(pred, 2, function(x) {
    tab <- tabulate(x, nbins = max(pred, na.rm = TRUE))
    tab[tab == 1] <- 0 # Avoid terminal nodes with just one obs
    tab/sum(tab)
  })
  
  params <- class_probs <- NULL
  
  # Fit continuous distribution in all terminal nodes
  if (any(!factor_cols)) {
    params <- foreach(tree = 1:num_trees, .combine = rbind) %dopar% {
      dt <- data.table(tree = tree, x_real[, !factor_cols, drop = FALSE], nodeid = pred[, tree])
      long <- melt(dt, id.vars = c("tree", "nodeid"))
      long[, list(mean = mean(value), sd = sd(value)), by = .(tree, nodeid, variable)]
    }
  }
  
  # Calculate class probabilities for categorical data in all terminal nodes
  if (any(factor_cols)) {
    class_probs <- foreach(tree = 1:num_trees, .combine = rbind) %dopar% {
      dt <- data.table(tree = tree, x_real[, factor_cols, drop = FALSE], nodeid = pred[, tree])
      long <- melt(dt, id.vars = c("tree", "nodeid"), value.factor = TRUE)
      setDT(long)[, .N, by = .(tree, nodeid, variable, value)]
    }
  }
  
  out <- list('params' = params, 'class_probs' = class_probs, 'iters' = iters)
  return(out)
  
}


