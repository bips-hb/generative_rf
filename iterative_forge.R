
source("generative_ranger.R")

iterative_forge <- function(iters = 10, ...) {
  
  # First iteration
  res <- generative_ranger(...)
  
  # More iterations
  if (iters > 1) {
    for (i in seq(2, iters, by = 1)) {
      res <- generative_ranger(x_synth = res, ...)
    }
  }
  
  res
}