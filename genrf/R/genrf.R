#' R6 Class for Generative RF
#'
#' @description
#' Description here.
#'
#' @details
#' Details here
#' @importFrom R6 R6Class
#' @importFrom foreach foreach %dopar%
#' @export
genrf <- R6::R6Class(
  classname = "genrf",
  public = list(
    initialize = function(x, oob = FALSE, dist = "normal", ...) {
      # Convert input to data.frame
      private$orig_colnames <- colnames(x)
      x_real <- data.frame(x)
      private$p <- ncol(x_real)
      private$levels <- lapply(x_real, levels)
      private$dist <- dist

      # Convert chars and logicals to factors
      private$idx_char <- sapply(x_real, is.character)
      if (any(private$idx_char)) {
        x_real[, private$idx_char] <- as.data.frame(lapply(x_real[, private$idx_char], as.factor))
      }
      private$idx_logical <- sapply(x_real, is.logical)
      if (any(private$idx_logical)) {
        x_real[, private$idx_logical] <- as.data.frame(lapply(x_real[, private$idx_logical], as.factor))
      }
      private$factor_cols <- sapply(x_real, is.factor)
      #factor_col_names <- names(factor_cols)[factor_cols]

      # Sample from marginals to get naive synthetic data
      x_synth <- as.data.frame(lapply(x_real, function(x) {
        sample(x, length(x), replace = TRUE)
      }))

      # Merge real and synthetic data
      dat <- rbind(data.frame(y = 0, x_real),
                   data.frame(y = 1, x_synth))

      # Fit ranger to both data
      rf <- ranger::ranger(y ~ ., dat, keep.inbag = TRUE, classification = TRUE, ...)

      # Get terminal nodes for all observations
      pred <- predict(rf, x_real, type = "terminalNodes")$predictions
      private$num_trees <- ncol(pred)

      # If OOB, use only OOB trees
      if (oob) {
        inbag <- (do.call(cbind, rf$inbag.counts) > 0)[1:nrow(x_real), ]
        pred[inbag] <- NA
      }

      # Get probabilities of terminal nodes for each tree
      # probs dims: [nodeid, tree]
      private$probs <- apply(pred, 2, function(x) {
        tab <- tabulate(x, nbins = max(pred, na.rm = TRUE))
        tab[tab == 1] <- 0 # Avoid terminal nodes with just one obs
        tab/sum(tab)
      })

      # Fit continuous distribution in all used terminal nodes
      # params dims: [[tree]][[nodeid]][[colname]][distr. parameters]
      if (any(!private$factor_cols)) {
        private$params <- foreach(tree=1:private$num_trees) %dopar% {
          unique_nodeids <- which(private$probs[, tree] > 0)
          if (dist == "normal") {
            # Use faster analytical version for normal distribution
            res <- lapply(unique_nodeids, function(nodeid) {
              idx <- which(pred[, tree] == nodeid)
              sapply(x_real[idx, !private$factor_cols, drop = FALSE], function(x) {
                c(mean = mean(x), sd = sd(x)) # Or use MLE (1/n)?
              })
            })
          } else {
            res <- lapply(unique_nodeids, function(nodeid) {
              idx <- which(pred[, tree] == nodeid)
              apply(x_real[idx, !private$factor_cols, drop = FALSE], 2, function(x) {
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
      if (any(private$factor_cols)) {
        private$class_probs <- foreach(tree=1:private$num_trees) %dopar% {
          unique_nodeids <- which(private$probs[, tree] > 0)
          res <- lapply(unique_nodeids, function(nodeid) {
            idx <- which(pred[, tree] == nodeid)
            lapply(x_real[idx, private$factor_cols, drop = FALSE], function(x) {
              tabulate(x, nbins = nlevels(x))
            })
          })
          names(res) <- unique_nodeids
          res
        }
      }
    },
    sample = function(n) {
      # Sample new observations and get their terminal nodes
      # nodeids dims: [new obs, tree]
      nodeids <- apply(private$probs, 2, function(x) {
        sample(length(x), n, replace = TRUE, prob = x)
      })

      # Randomly select tree for each new obs. (mixture distribution with equal prob.)
      sampled_trees <- sample(private$num_trees, n, replace = TRUE)
      sampled_nodes <- sapply(1:n, function(i) {
        as.character(nodeids[i, sampled_trees[i]])
      })

      # Get distributions parameters for each new obs.
      if (any(!private$factor_cols)) {
        obs_params <- sapply(1:n, function(i) {
          private$params[[sampled_trees[i]]][[sampled_nodes[i]]]
        }, simplify = "array")
      }

      # Sample new data from mixture distribution over trees
      data_new <- foreach(j = 1:private$p, .combine = data.frame) %dopar% {
        colname <- names(private$factor_cols)[j]

        if (private$factor_cols[j]) {
          # Factor columns: Multinomial distribution
          draws <- sapply(1:n, function(i) {
            probs <- private$class_probs[[sampled_trees[i]]][[sampled_nodes[i]]][[colname]]
            sample.int(n = length(probs), size = 1, prob = probs)
          })
          factor(private$levels[[j]][draws])
        } else {
          # Continuous columns: Match estimated distribution parameters with r...() function
          if (private$dist == "normal") {
            rnorm(n = n, mean = obs_params["mean", colname, ],
                  sd = obs_params["sd", colname, ])
          } else if (private$dist == "exponential") {
            rexp(n = n, obs_params[colname, ])
          } else if (private$dist == "geometric") {
            rgeom(n = n, obs_params[colname, ])
          } else if (private$dist %in% c("log-normal", "lognormal")) {
            rlnorm(n = n, meanlog = obs_params["meanlog", colname, ],
                   sdlog = obs_params["sdlog", colname, ])
          } else if (private$dist == "Poisson") {
            rpois(n = n, obs_params[colname, ])
          } else {
            stop("Unknown distribution.")
          }
        }
      }

      # Convert chars and logicals back
      if (any(private$idx_char)) {
        data_new[, private$idx_char] <- as.data.frame(lapply(data_new[, private$idx_char], as.character))
      }
      if (any(private$idx_logical)) {
        data_new[, private$idx_logical] <- as.data.frame(lapply(data_new[, private$idx_logical], function(x) {x == "TRUE"}))
      }

      # Use original column names
      colnames(data_new) <- private$orig_colnames

      # Return synthetic data
      data_new
    }
  ),
  private = list(
    probs = matrix(),
    params = list(),
    class_probs = list(),
    num_trees = integer(),
    p = integer(),
    factor_cols = logical(),
    orig_colnames = character(),
    levels = list(),
    idx_char = logical(),
    idx_logical = logical(),
    dist = character()
  )
)
