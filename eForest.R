#' Autoencoder forest
#' 
#' Implements an autoencoder forest, a function that maps inputs to a small
#' hyperrectangle defined by the intersection of leaves in a random forest.
#' 
#' @param x Input data. 
#' @param x_tst Optional test set to encode. If left \code{NULL}, the function
#'   simply encodes \code{x}.
#' @param num_trees Number of trees to grow. 
#' @param min_node_size Minimum size for terminal nodes.
#' @param parallel Compute in parallel? Much register backend beforehand.
#' @param ... Extra parameters to be passed to \code{ranger}.
#'
#' @details
#' This is a variant of the original autoencoder forest proposed by Feng & Zhou
#' (2017). This version grows an unsupervised random forest, where the task is 
#' to distinguish original samples from synthetic data drawn from the marginals.
#' Synthetic data is sampled independently for each tree.
#' 
#' @return
#' An encoded dataset of maximum compatible rules for each sample.
#' 
#' @references 
#' Feng, J. and Zhou, Z. (2017). AutoEncoder by Forest. \emph{arXiv} preprint, 
#' 1709.09018. 
#' 
#' @import data.table
#' @import ranger 
#' @import foreach
#'

# Autoencoder forest function
eForest <- function(
    x, 
    x_tst = NULL,
    num_trees = 200, 
    min_node_size = 5, 
    parallel = TRUE,
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
  # Tree growing function (each tree gets its own synthetic data)
  grow_tree <- function(b) {
    # Draw data, fit model
    x_real <- x[sample(n, replace = TRUE), , drop = FALSE]
    x_synth <- as.data.frame(lapply(x, function(xj) {
      sample(xj, n, replace = TRUE)
    }))
    dat <- rbind(data.table(y = 1, x_real),
                 data.table(y = 0, x_synth))
    f <- ranger(y ~ ., dat, classification = TRUE, num.trees = 1,
                replace = FALSE, min.node.size = min_node_size, ...)
    return(f)
  }
  if (isTRUE(parallel)) {
    rf <- foreach(bb = 1:num_trees) %dopar% grow_tree(bb)
  } else {
    rf <- foreach(bb = 1:num_trees) %do% grow_tree(bb)
  }
  # Find global bounds
  if (!is.null(x_tst)) {
    x <- as.data.frame(x_tst)
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
  }
  bnds_cnt <- bnds_cat <- NULL
  if (any(!factor_cols)) {
    x_cnt <- as.matrix(x[, !factor_cols, drop = FALSE])
    d_cnt <- ncol(x_cnt)
    bnds_cnt <- data.table(
      'idx' = 0, 'nodeID' = NA_integer_,
      'variable' = rep(colnames(x_cnt), times = 2),
      'value' = c(apply(x_cnt, 2, min), apply(x_cnt, 2, max)),
      'bound' = rep(c('lo', 'hi'), each = d_cnt)
    )
  }
  if (any(factor_cols)) {
    x_cat <- x[, factor_cols, drop = FALSE]
    d_cat <- ncol(x_cat)
    bnds_cat <- data.table(
      'idx' = 0, 'nodeID' = NA_integer_,
      'variable' = rep(colnames(x_cat), times = 2),
      'value' = c(rep(0, times = d_cat), 
                  sapply(seq_along(x_cat), function(j) length(levels(x_cat[, j])))),
      'bound' = rep(c('lo', 'hi'), each = d_cat)
    )
  }
  bounds <- rbind(bnds_cnt, bnds_cat)
  # Compute encoding for each sample
  nodeIDs <- do.call('cbind', lapply(rf, function(f) {
    predict(f, x, type = 'terminalNodes')$predictions
  }))
  encoding <- function(b, i) {
    # Pick tree
    f <- rf[[b]]
    tree <- as.data.table(treeInfo(f))
    # Trace the path for each sample
    node <- nodeIDs[i, b]
    path <- tree[nodeID == node]
    path[, bound := NA_character_]
    while(node > 0) {
      tmp <- tree[leftChild == node | rightChild == node, ]
      tmp[, bound := ifelse(leftChild == node, 'hi', 'lo')]
      path <- rbind(tmp, path)
      node <- tmp$nodeID
    }
    path[, idx := i]
    path <- na.omit(path[, .(idx, nodeID, splitvarName, splitval, bound)])
    colnames(path)[c(3, 4)] <- c('variable', 'value')
    path <- rbind(path, bounds)
    # Reduce to tree encoding
    sup <- path[bound == 'hi', min(value), by = variable]
    inf <- path[bound == 'lo', max(value), by = variable]
    out <- merge(sup, inf, by = 'variable')
    colnames(out)[c(2, 3)] <- c('max', 'min')
    out[, idx := i][, tree := b]
    out <- out[, .(tree, idx, variable, max, min)]
    return(out)
  }
  # Loop over trees and samples
  if (isTRUE(parallel)) {
    z <- foreach(bb = 1:num_trees, .combine = rbind) %:%
      foreach(ii = 1:n, .combine = rbind) %dopar% encoding(bb, ii)
  } else {
    z <- foreach(bb = 1:num_trees, .combine = rbind) %:%
      foreach(ii = 1:n, .combine = rbind) %do% encoding(bb, ii)
  }
  # Reduce to maximal compatible rule
  sup <- z[, min(max), by = .(idx, variable)]
  inf <- z[, max(min), by = .(idx, variable)]
  z <- merge(sup, inf, by = c('idx', 'variable'))
  colnames(z)[c(3, 4)] <- c('max', 'min')
  return(z)
}








