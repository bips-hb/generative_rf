# Non-R6 version of genrf.R

urf <- function(x, oob = FALSE, num_trees = 10, leaf_size = 5, ...) {
  # Convert input to data.frame
  orig_colnames <- colnames(x)
  x_real <- data.frame(x)
  p <- ncol(x_real)
  
  # Convert chars and logicals to factors
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
  dat <- rbind(data.frame(y = 0, x_real),
               data.frame(y = 1, x_synth))
  
  # Fit ranger to both data
  rf <- ranger::ranger(y ~ ., dat, keep.inbag = TRUE, classification = TRUE, 
                       num.trees = num_trees, respect.unordered.factors = TRUE, ...)
  return(rf)
}

forde_fn <- function(rf, x, oob = FALSE, dist = 'normal') {
  # Convert input to data.frame
  orig_colnames <- colnames(x)
  x_real <- data.frame(x)
  p <- ncol(x_real)
  num_trees <- rf$num.trees
  
  # Convert chars and logicals to factors
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
  
  # Get terminal nodes for all observations
  pred <- predict(rf, x_real, type = "terminalNodes")$predictions
  
  # If OOB, use only OOB trees
  if (oob) {
    inbag <- (do.call(cbind, rf$inbag.counts) > 0)[1:nrow(x_real), ]
    pred[inbag] <- NA
  }
  
  # Fit continuous distribution in all terminal nodes
  if (any(!factor_cols)) {
    params <- foreach(tree = 1:num_trees, .combine = rbind) %dopar% {
      dt <- data.table(tree = tree, x_real[, !factor_cols, drop = FALSE], nodeid = pred[, tree])
      long <- melt(dt, id.vars = c("tree", "nodeid"))
      if (dist == "normal") {
        long[, list(mean = mean(value), sd = sd(value)), by = .(tree, nodeid, variable)]
      } else if (dist == "beta") {
        long[, list(mu = mean(value), s2 = var(value)), by = .(tree, nodeid, variable)]
        long[, alpha := ((1 - mu) / s2 - 1 / mu) * mu^2]
        long[, beta := alpha * (1 / mu - 1)]
      } else if (dist == "pwc") {
        long[, list(min = min(value), max = max(value)), by = .(tree, nodeid, variable)]
      } else {
        long[, as.list(MASS::fitdistr(value, dist)$estimate), by = .(tree, nodeid, variable)]
      }
    }
  } else {
    params <- NULL
  }
  
  # Calculate class probabilities for categorical data in all terminal nodes
  if (any(factor_cols)) {
    class_probs <- foreach(tree = 1:num_trees, .combine = rbind) %dopar% {
      dt <- data.table(tree = tree, x_real[, factor_cols, drop = FALSE], nodeid = pred[, tree])
      long <- melt(dt, id.vars = c("tree", "nodeid"), value.factor = TRUE)
      setDT(long)[, .N, by = .(tree, nodeid, variable, value)]
    }
  } else {
    class_probs <- NULL
  }
  out <- list('params' = params, 'class_probs' = class_probs)
  return(out)
}

forge_fn <- function(rf, x, psi, n, oob = FALSE) {
  # Convert input to data.frame
  orig_colnames <- colnames(x)
  x_real <- data.frame(x)
  p <- ncol(x_real)
  num_trees <- rf$num.trees
  
  # Convert chars and logicals to factors
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
  
  # Get terminal nodes for all observations
  pred <- predict(rf, x_real, type = "terminalNodes")$predictions
  
  # If OOB, use only OOB trees
  if (oob) {
    inbag <- (do.call(cbind, rf$inbag.counts) > 0)[1:nrow(x_real), ]
    pred[inbag] <- NA
  }
  
  # Get probabilities of terminal nodes for each tree
  node_probs <- apply(pred, 2, function(x) {
    tab <- tabulate(x, nbins = max(pred, na.rm = TRUE))
    tab[tab == 1] <- 0 # Avoid terminal nodes with just one obs
    tab/sum(tab)
  })
  
  # Sample new observations and get their terminal nodes
  # nodeids dims: [new obs, tree]
  nodeids <- apply(node_probs, 2, function(x) {
    sample(length(x), n, replace = TRUE, prob = x)
  })
  
  # Randomly select tree for each new obs. (mixture distribution with equal prob.)
  sampled_trees <- sample(num_trees, n, replace = TRUE)
  sampled_nodes <- sapply(1:n, function(i) {
    nodeids[i, sampled_trees[i]]
  })
  sampled_trees_nodes <- data.table(obs = 1:n, tree = sampled_trees, nodeid = sampled_nodes)
  
  # Get distributions parameters for each new obs.
  if (any(!factor_cols)) {
    obs_params <- merge(sampled_trees_nodes, psi$params,
                        by = c("tree", "nodeid"), sort = FALSE, allow.cartesian = TRUE)
  }
  
  # Get probabilities for each new obs.
  if (any(factor_cols)) {
    obs_probs <- merge(sampled_trees_nodes, psi$class_probs,
                       by = c("tree", "nodeid"), sort = FALSE, allow.cartesian = TRUE)
  }
  
  # Sample new data from mixture distribution over trees
  data_new <- foreach(j = 1:p, .combine = data.frame) %dopar% {
    colname <- names(factor_cols)[j]
    
    if (factor_cols[j]) {
      # Factor columns: Multinomial distribution
      obs_probs[variable == colname, droplevels(sample(value, 1, prob = N)), by = obs]$V1
    } else {
      # Continuous columns: Match estimated distribution parameters with r...() function
      rnorm(n = n, mean = obs_params[variable == colname, mean],
            sd = obs_params[variable == colname, sd])
    }
  }
  
  # Convert chars and logicals back
  if (any(idx_char)) {
    data_new[, idx_char] <- as.data.frame(
      lapply(data_new[, idx_char, drop = FALSE], as.character)
    )
  }
  if (any(idx_logical)) {
    data_new[, idx_logical] <- as.data.frame(
      lapply(data_new[, idx_logical, drop = FALSE], function(x) {x == "TRUE"})
    )
  }
  
  # Use original column names
  colnames(data_new) <- orig_colnames
  
  # Return synthetic data
  data_new
  
}
