#' R6 Class for Generative RF
#'
#' @description
#' Description here.
#'
#' @details
#' Details here
#' @importFrom R6 R6Class
#' @importFrom foreach foreach %dopar%
#' @importFrom data.table data.table melt setDT
#' @export
genrf <- R6::R6Class(
  classname = "genrf",
  public = list(
    # Create and fit a generative random forest
    initialize = function(x, oob = FALSE, dist = "normal", num_trees = 10, min_node_size = 5, ...) {
      # Convert input to data.frame
      private$orig_colnames <- colnames(x)
      x_real <- data.frame(x)
      private$p <- ncol(x_real)
      private$dist <- dist
      private$num_trees <- num_trees

      # Convert chars and logicals to factors
      private$idx_char <- sapply(x_real, is.character)
      if (any(private$idx_char)) {
        x_real[, private$idx_char] <- as.data.frame(
          lapply(x_real[, private$idx_char, drop = FALSE], as.factor)
        )
      }
      private$idx_logical <- sapply(x_real, is.logical)
      if (any(private$idx_logical)) {
        x_real[, private$idx_logical] <- as.data.frame(
          lapply(x_real[, private$idx_logical, drop = FALSE], as.factor)
        )
      }
      private$factor_cols <- sapply(x_real, is.factor)

      # Sample from marginals to get naive synthetic data
      x_synth <- as.data.frame(lapply(x_real, function(x) {
        sample(x, length(x), replace = TRUE)
      }))

      # Merge real and synthetic data
      dat <- rbind(data.frame(y = 0, x_real),
                   data.frame(y = 1, x_synth))

      # Fit ranger to both data
      rf <- ranger::ranger(y ~ ., dat, keep.inbag = TRUE, classification = TRUE, num.trees = num_trees, min.node.size = min_node_size, ...)

      # Get terminal nodes for all observations
      pred <- predict(rf, x_real, type = "terminalNodes")$predictions

      # If OOB, use only OOB trees
      if (oob) {
        inbag <- (do.call(cbind, rf$inbag.counts) > 0)[1:nrow(x_real), ]
        pred[inbag] <- NA
      }

      # Get probabilities of terminal nodes for each tree
      private$node_probs <- apply(pred, 2, function(x) {
        tab <- tabulate(x, nbins = max(pred, na.rm = TRUE))
        tab[tab == 1] <- 0 # Avoid terminal nodes with just one obs
        tab/sum(tab)
      })

      # Fit continuous distribution in all terminal nodes
      if (any(!private$factor_cols)) {
        private$params <- foreach(tree = 1:private$num_trees, .combine = rbind) %dopar% {
          dt <- data.table(tree = tree, x_real[, !private$factor_cols, drop = FALSE], nodeid = pred[, tree])
          long <- melt(dt, id.vars = c("tree", "nodeid"))

          if (dist == "normal") {
            long[, list(mean = mean(value), sd = sd(value)), by = .(tree, nodeid, variable)]
          } else if (dist == "beta") {
            long[, list(mu = mean(value), s2 = var(value)), by = .(tree, nodeid, variable)]
            long[, alpha := ((1 - mu) / s2 - 1 / mu) * mu^2]
            long[, beta := alpha * (1 / mu - 1)]
          } else if (dist == "pwc") {
            long[, list(mean = mean(value)), by = .(tree, nodeid, variable)]
          } else {
            long[, as.list(MASS::fitdistr(value, dist)$estimate), by = .(tree, nodeid, variable)]
          }
        }
      }

      # Calculate class probabilities for categorical data in all terminal nodes
      if (any(private$factor_cols)) {
        private$class_probs <- foreach(tree = 1:private$num_trees, .combine = rbind) %dopar% {
          dt <- data.table(tree = tree, x_real[, private$factor_cols, drop = FALSE], nodeid = pred[, tree])
          long <- melt(dt, id.vars = c("tree", "nodeid"), value.factor = TRUE)
          setDT(long)[, .N, by = .(tree, nodeid, variable, value)]
        }
      }
    },
    # Sample new data from generative random forest
    sample = function(n) {
      # Sample new observations and get their terminal nodes
      # nodeids dims: [new obs, tree]
      nodeids <- apply(private$node_probs, 2, function(x) {
        sample(length(x), n, replace = TRUE, prob = x)
      })

      # Randomly select tree for each new obs. (mixture distribution with equal prob.)
      sampled_trees <- sample(private$num_trees, n, replace = TRUE)
      sampled_nodes <- sapply(1:n, function(i) {
        nodeids[i, sampled_trees[i]]
      })
      sampled_trees_nodes <- data.table(obs = 1:n, tree = sampled_trees, nodeid = sampled_nodes)

      # Get distributions parameters for each new obs.
      if (any(!private$factor_cols)) {
        obs_params <- merge(sampled_trees_nodes, private$params, by = c("tree", "nodeid"), sort = FALSE, allow.cartesian = TRUE)
      }

      # Get probabilities for each new obs.
      if (any(private$factor_cols)) {
        obs_probs <- merge(sampled_trees_nodes, private$class_probs, by = c("tree", "nodeid"), sort = FALSE, allow.cartesian = TRUE)
      }

      # Sample new data from mixture distribution over trees
      data_new <- foreach(j = 1:private$p, .combine = data.frame) %dopar% {
        colname <- names(private$factor_cols)[j]

        if (private$factor_cols[j]) {
          # Factor columns: Multinomial distribution
          obs_probs[variable == colname, droplevels(sample(value, 1, prob = N)), by = obs]$V1
        } else {
          # Continuous columns: Match estimated distribution parameters with r...() function
          if (private$dist == "normal") {
            rnorm(n = n, mean = obs_params[variable == colname, mean],
                  sd = obs_params[variable == colname, sd])
          } else if (private$dist == "beta") {
            rbeta(n = n, shape1 = obs_params[variable == colname, alpha],
                  shape2 = obs_params[variable == colname, beta])
          } else if (private$dist == "exponential") {
            rexp(n = n, obs_params[variable == colname, rate])
          } else if (private$dist == "geometric") {
            rgeom(n = n, obs_params[variable == colname, prob])
          } else if (private$dist %in% c("log-normal", "lognormal")) {
            rlnorm(n = n, meanlog = obs_params[variable == colname, meanlog],
                   sdlog = obs_params[variable == colname, sdlog])
          } else if (private$dist == "Poisson") {
            rpois(n = n, obs_params[variable == colname, lambda])
          } else if (private$dist == "pwc") {
            rep(obs_params[variable == colname, mean], n)
          } else {
            stop("Unknown distribution.")
          }
        }
      }

      # Convert chars and logicals back
      if (any(private$idx_char)) {
        data_new[, private$idx_char] <- as.data.frame(lapply(data_new[, private$idx_char, drop = FALSE], as.character))
      }
      if (any(private$idx_logical)) {
        data_new[, private$idx_logical] <- as.data.frame(lapply(data_new[, private$idx_logical, drop = FALSE], function(x) {x == "TRUE"}))
      }

      # Use original column names
      colnames(data_new) <- private$orig_colnames

      # Return synthetic data
      data_new
    }
  ),
  private = list(
    node_probs = matrix(), # Selection probabilities for terminal nodes; dims: [nodeid, tree]
    params = data.table(), # Distribution parameters for numeric columns; columns:
    class_probs = data.table(), # Probabilities for categorical columns; columns:
    num_trees = integer(), # Number of trees in the random forest
    p = integer(), # Number of columns in the data
    factor_cols = logical(), # Which columns are factors after transforming chars and logicals?
    orig_colnames = character(), # Original column names of the input data
    idx_char = logical(), # Which columns are chars before transformations?
    idx_logical = logical(), # Which columns are logicals before transformations?
    dist = character() # Distribution for numeric columns
  )
)
