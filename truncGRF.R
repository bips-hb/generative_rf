
library(ranger)
library(foreach)
library(data.table)
library(truncnorm)

#' Generative Random Forests with truncated normal distributions to sample from.
#'
#' @param x_real Original data (data.frame).
#' @param x_synth Naive synthetic data, if NULL will be sampled from marginals.
#' @param n_new Number of synthetic observations to sample.
#' @param oob Use only out-of-bag data to calculate leaf probabilities?
#' @param dist Distribution to fit in terminal nodes to continuous data. Currently implemented: "normal", "exponential", "geometric", "lognormal", "Poisson", "pwc" (piecewise constant). 
#' @param num_trees Number of trees 
#' @param min_node_size Minimal node size
#' @param ... Passed on to the ranger() call; use for 'max.depth', etc.
#'
#' @return data.frame with synthetic data.
#' @export
#'
#' @examples

#registerDoParallel(20)
#set.seed(42, kind = "L'Ecuyer-CMRG")
#smpl <- trunc_generative_ranger(x_real = iris, num_trees = 1, n_new=1000)

trunc_generative_ranger <- function(x_real, x_synth = NULL, iterations=1, n_new, oob = FALSE, 
                              dist = "normal", num_trees = 10, min_node_size = 5, 
                              cat_num = 10, ...) {
  
  # Convert to data.frame
  orig_colnames <- colnames(x_real)
  x_real <- data.frame(x_real)
  p <- ncol(x_real) 
  
  # Convert chars and logicals to factors
  idx_char <- sapply(x_real, is.character)
  if (any(idx_char)) {
    x_real[, idx_char] <- as.data.frame(lapply(x_real[, idx_char, drop = FALSE], as.factor))
  }
  idx_logical <- sapply(x_real, is.logical)
  if (any(idx_logical)) {
    x_real[, idx_logical] <- as.data.frame(lapply(x_real[, idx_logical, drop = FALSE], as.factor))
  }
  
  # Convert numerics with less than cat_num unique values to factor (if requested)
  if (!is.null(cat_num)) {
    idx_catnum <- sapply(x_real, function(x) {is.numeric(x) && length(unique(x)) <= cat_num})
    if (any(idx_catnum)) {
      x_real[, idx_catnum] <- as.data.frame(lapply(x_real[, idx_catnum, drop = FALSE], as.factor))
    }
  }
  
  factor_cols <- sapply(x_real, is.factor)
  factor_col_names <- names(factor_cols)[factor_cols]
  
  
    for(iter in 1:iterations) {
      
    
    # If no synthetic data provided, sample from marginals
    if(is.null(x_synth)) {
    x_synth <- as.data.frame(lapply(x_real, function(x) {
      sample(x, length(x), replace = TRUE)
    }))
    }
    
    # Merge real and synthetic data
    dat <- rbind(data.frame(y = 0, x_real), 
                 data.frame(y = 1, x_synth))
    
    # Fit ranger to both data
    rf <- ranger(y ~ ., dat, keep.inbag = TRUE, classification = TRUE, num.trees = num_trees, min.node.size = min_node_size, ...)
    
    # Get terminal nodes for all observations
    pred <- predict(rf, x_real, type = "terminalNodes")$predictions
    
    
    leafBoundsTrees <- foreach(tree = 1:num_trees, .combine = rbind) %do% {
      treeInf <- treeInfo(rf,tree=tree)
      leaves <- which(treeInf$terminal) -1 
      boundsFilled <- foreach(leavesCounter = 1:length(leaves), .combine = rbind) %dopar% {
        node = leaves[leavesCounter]
        initBounds <- matrix(-Inf, nrow=1, ncol=p)
        boundsLeaf <- data.frame(tree,node,initBounds,-initBounds)
        colnames(boundsLeaf) <- c("tree", "leaf", paste(colnames(x_real),"l",sep="_"),paste(colnames(x_real),"r",sep="_"))
        while(node!=0) {
          parentCoord <- which(treeInf[2:3] == node,arr.ind = T)
          parentRow <- parentCoord[,1]
          parent <- treeInf$nodeID[parentRow]
          if (parentCoord[,2] == 1) {
            parentRel <- "r"
          } else {
            parentRel <- "l"
          }
          splitVar <- paste(treeInf$splitvarName[parentRow],parentRel,sep="_")
          splitVal <- treeInf$splitval[parentRow]
          boundsLeaf[1,splitVar] <- splitVal
          
          node <- parent
        }
        boundsLeaf
      }
      boundsFilled
    }
    
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
    
    # Fit continuous distribution in all terminal nodes
    if (any(!factor_cols)) {
      params <- foreach(tree = 1:num_trees, .combine = rbind) %dopar% { 
        dt <- data.table(tree = tree, x_real[, !factor_cols, drop = FALSE], nodeid = pred[, tree])
        long <- melt(dt, id.vars = c("tree", "nodeid"))
        
        if (dist == "normal") {
          long[, list(mean = mean(value), sd = sd(value)), by = .(tree, nodeid, variable)]
        } else if (dist == "pwc") {
          long[, list(min = min(value), max = max(value)), by = .(tree, nodeid, variable)]
        } else {
          long[, as.list(MASS::fitdistr(value, dist)$estimate), by = .(tree, nodeid, variable)]
        }
      }
      for(i in 1:nrow(params)) {
        params[i,"ltrunc"] <- leafBoundsTrees[leafBoundsTrees$tree == unlist(params[i,"tree"]) & leafBoundsTrees$leaf == unlist(params[i,"nodeid"]),paste(unlist(params[i,"variable"]),"l",sep="_")]
        params[i,"rtrunc"] <- leafBoundsTrees[leafBoundsTrees$tree == unlist(params[i,"tree"]) & leafBoundsTrees$leaf == unlist(params[i,"nodeid"]),paste(unlist(params[i,"variable"]),"r",sep="_")]
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
      obs_params <- merge(sampled_trees_nodes, params, by = c("tree", "nodeid"), sort = FALSE, allow.cartesian = TRUE)
    }
    
    # Get probabilities for each new obs.
    if (any(factor_cols)) {
      obs_probs <- merge(sampled_trees_nodes, class_probs, by = c("tree", "nodeid"), sort = FALSE, allow.cartesian = TRUE)
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
          rtruncnorm(n = n_new, a=obs_params[variable==colname, ltrunc], b=obs_params[variable==colname, rtrunc], mean = obs_params[variable == colname, mean], 
                sd = obs_params[variable == colname, sd])
        } else if (dist == "exponential") {
          rexp(n = n_new, obs_params[variable == colname, rate])
        } else if (dist == "geometric") {
          rgeom(n = n_new, obs_params[variable == colname, prob])
        } else if (dist %in% c("log-normal", "lognormal")) {
          rlnorm(n = n_new, meanlog = obs_params[variable == colname, meanlog], 
                 sdlog = obs_params[variable == colname, sdlog])
        } else if (dist == "Poisson") {
          rpois(n = n_new, obs_params[variable == colname, lambda])
        } else if (dist == "pwc") {
          rnorm(n = n_new, min = obs_params[variable == colname, min], 
                max = obs_params[variable == colname, max])
        } else {
          stop("Unknown distribution.")
        }
      }
    }
    
    # Convert chars and logicals back
    if (any(idx_char)) {
      data_new[, idx_char] <- as.data.frame(lapply(data_new[, idx_char, drop = FALSE], as.character))
    }
    if (any(idx_logical)) {
      data_new[, idx_logical] <- as.data.frame(lapply(data_new[, idx_logical, drop = FALSE], function(x) {x == "TRUE"}))
    }
    
    # Convert numerics back (if requested)
    if (!is.null(cat_num)) {
      data_new[, idx_catnum] <- as.data.frame(lapply(data_new[, idx_catnum, drop = FALSE], function(x) {as.numeric(as.character(x))}))
    }
    
    # Use original column names
    colnames(data_new) <- orig_colnames
    
    # Return synthetic data
    data_synth <- data_new
    }
    data_synth
}
