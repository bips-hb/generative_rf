# Load libraries
library(data.table)
library(ranger)
library(foreach)

# Split function
part <- function(x_real) {
  # Synthesize
  x_synth <- as.data.frame(lapply(x_real, function(x) {
    sample(x, nrow(x_real), replace = TRUE)
  }))
  dat <- rbind(data.table(y = 1, x_real),
               data.table(y = 0, x_synth))
  # Fit
  f <- ranger(y ~ ., dat, classification = TRUE, num.trees = 1, max.depth = 1,
              replace = FALSE, sample.fraction = 1)
  # Evaluate
  p <- sum(dat$y == predict(f, dat)$predictions) / (2 * nrow(x_real))
  gini <- 4 * p * (1 - p) # Doubling so max = 1
  return(list('f' = f, 'gini' = gini))
}

# Adversarial RF function
adversarial_rf <- function(
    x, 
    delta = 0,
    oob = FALSE, 
    num_trees = 10, 
    min_node_size = 5, 
    ...) {
  # Prelimz
  n <- nrow(x)
  cells <- list(1:n)
  cells_final <- list()
  a <- cells[[1]]
  gini <- 0
  # Tree growing function
  tree_grow <- function(b) {
    x_tmp <- x[sample(n, replace = TRUE), ]
    while (length(cells) > 0) {
      a <- cells[[1]]
      if (length(a) < min_node_size | gini >= 1 - delta) {
        cells[[1]] <- NULL
        cells_final[[length(cells_final) + 1]] <- a
      } else {
        n_a <- length(a)
        x_a <- x_tmp[a, ]
        splt <- part(x_a)
        gini <- splt$gini
        node_ids <- c(predict(splt$f, x_a, type = 'terminalNodes')$predictions)
        cells <- c(cells, split(a, node_ids))
        cells[[1]] <- NULL
      }
    }
    return(cells_final)
  }
  # Merge trees, export results
  f <- foreach(bb = 1:num_trees) %do% tree_grow(bb)
  return(f)
}

# Train a FORDE model
forde_trn <- function(f, x) {
  
}

# FORDE test
forde_tst <- function(forde, x) {
  
}

# FORGE
forge <- function(forde, n) {
  
}

















