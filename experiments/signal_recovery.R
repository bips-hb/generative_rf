
library(mvtnorm)
library(ggplot2)
library(GGally)
library(ranger)
library(genrf)

# Simulation params
repl <- 10
n <- 100000
p <- 5
cov_base <- .8
sigma <- toeplitz(cov_base^(0:(p-1)))

res <- replicate(repl, {
  # Simulate multivariate normal data (Toeplitz structure)
  x <- matrix(rmvnorm(n = n, sigma = sigma), ncol = p,
              dimnames = list(NULL, paste0('x', seq_len(p))))
  x_df <- as.data.frame(x)
  
  # Generate synthetic data
  mod <- genrf$new(x_df, num.trees = 10,  min.node.size = 5)
  synth <- mod$sample(n)
  cor(synth)
})
res_mean <- apply(res, 1:2, mean)

# Lower triangular matrix: Ground truth correlations
# Upper triangular matrix: Recovered correlations
round(res_mean * upper.tri(res_mean) + sigma * lower.tri(sigma), 2)
