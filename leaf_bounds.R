
library(ranger)
library(data.table)
library(foreach)

# Function to calculate leaf bounds
leaf_bounds <- function(rf) {
  num_trees <- rf$num.trees
  p <- rf$num.independent.variables
  bounds <- foreach(tree = 1:num_trees, .combine = rbind) %do% {
    num_nodes <- length(rf$forest$split.varIDs[[tree]])
    lb <- matrix(-Inf, nrow = num_nodes, ncol = p)
    ub <- matrix(Inf, nrow = num_nodes, ncol = p)
    
    for (i in 1:num_nodes) {
      left_child <- rf$forest$child.nodeIDs[[tree]][[1]][i] + 1
      right_child <- rf$forest$child.nodeIDs[[tree]][[2]][i] + 1
      splitvarID <- rf$forest$split.varIDs[[tree]][i] + 1
      splitval <- rf$forest$split.value[[tree]][i]
      if (left_child > 1) {
        ub[left_child, ] <- ub[i, ]
        ub[right_child, ] <- ub[i, ]
        lb[left_child, ] <- lb[i, ]
        lb[right_child, ] <- lb[i, ]
        ub[left_child, splitvarID] <- splitval
        lb[right_child, splitvarID] <- splitval
      }
    }
    
    leaves <- which(rf$forest$child.nodeIDs[[tree]][[1]] == 0) 
    cbind(tree, leaves, lb[leaves, ], ub[leaves, ])
    
  }
  colnames(bounds) <- c("tree", "leaf",
                        paste0(rf$forest$independent.variable.names, "_lo"), 
                        paste0(rf$forest$independent.variable.names, "_up"))
  bounds
}

# Test 
library(mvtnorm)
n <- 100
p <- 4
mu <- rep(0, p)
sigma <- toeplitz(c(1, rep(.2, p-1)))
x <- matrix(Rfast::rmvnorm(n = n, mu = mu, sigma = sigma), ncol = p,
            dimnames = list(NULL, paste0('x', seq_len(p))))
y <- factor(rbinom(n, 1, .5))
rf <- ranger(y = y, x = x, num.trees = 5)

leaf_bounds(rf)
