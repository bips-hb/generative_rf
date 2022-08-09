# Load libraries
library(data.table)
library(ranger)
library(doMC)
registerDoMC(8)

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
  #phi <- list(0)
  h <- 0
  cells_final <- list()
  #phi_final <- list()
  h_final <- double()
  # Tree growing function
  tree_grow <- function(b) {
    x_tmp <- x[sample(n, replace = TRUE), ]
    while (length(cells) > 0) {
      a <- cells[[1]]
      if (length(a) < min_node_size | h[1] >= 1 - delta) {
        cells_final[[length(cells_final) + 1]] <- a
        #phi_final[[length(phi_final) + 1]] <- phi[[1]]
        h_final[length(h_final) + 1] <- h[1]
        cells <- cells[-1]
        h <- h[-1]
      } else {
        n_a <- length(a)
        x_a <- x_tmp[a, ]
        # Synthesize
        x_synth <- as.data.frame(lapply(x_a, function(x) {
          sample(x, nrow(x_a), replace = TRUE)
        }))
        dat <- rbind(data.table(y = 1, x_a),
                     data.table(y = 0, x_synth))
        # Fit
        f <- ranger(y ~ ., dat, classification = TRUE, num.trees = 1, 
                    max.depth = 1, replace = FALSE, sample.fraction = 1)
        # Evaluate, append
        dat[, node_id := predict(f, dat, type='terminalNodes')$predictions]
        if (dat[, sum(node_id)] == 0) {
          h[1] <- 1
        } else {
          p1 <- dat[node_id == 1, sum(y) / .N]
          p2 <- dat[node_id == 2, sum(y) / .N]
          h <- c(h[-1], 4 * p1 * (1 - p1), 4 * p2 * (1 - p2))
          cells <- c(cells[-1], split(a, dat[y == 1, node_id]))
          #phi <- c(phi, BLAH)
        }
      }
    }
    n_leaves <- length(cells_final)
    q <- sapply(1:n_leaves, function(l) length(cells_final[[l]]) / n)
    out <- data.table('tree' = b, 'leaf' = 1:n_leaves, 
                      'q' = q, 'loss' = 1 - h_final)
    return(out)
  }
  # Merge trees, export results
  f <- foreach(bb = 1:num_trees, .combine = rbind) %dopar% tree_grow(bb)
  return(f)
}

# Spot check
data(iris)
f <- arf(iris)
f[, sum(q * loss) / max(tree)]





