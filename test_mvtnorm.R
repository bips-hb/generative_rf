
library(mvtnorm)
library(ggplot2)
library(GGally)
library(ranger)

source("generative_ranger.R")

# Simulation params
n <- 1000
p <- 6
cov_base <- .6

# Simulate multivariate normal data (Toeplitz structure)
sigma <- toeplitz(cov_base^(0:(p-1)))
x <- matrix(rmvnorm(n = n, sigma = sigma), ncol = p,
            dimnames = list(NULL, paste0('x', seq_len(p))))
x_df <- as.data.frame(x)

# Generate synthetic data
x_new <- generative_ranger(x_real = x_df, x_synth = NULL, n_new = 1000,
                           num.trees = 100,  min.node.size = 5)

# Plot distributions and correlations
x_sy_df <- rbind(data.frame(data = "synthetic", x_new),
                 data.frame(data = "original", x_df))
ggpairs(x_sy_df, columns = 2:7, ggplot2::aes(colour=data)) + 
  theme_bw()
