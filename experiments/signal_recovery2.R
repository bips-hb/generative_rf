
library(mvtnorm)
library(ggplot2)
library(GGally)
library(ranger)
library(genrf)
library(xtable)

set.seed(42)

# Simulation params
repl <- 20
n <- c(1000, 10000)
p <- 4
cov_base <- .8
sigma <- toeplitz(cov_base^(0:(p-1)))

sim_fun <- function(n) {
  # Simulate multivariate normal data (Toeplitz structure)
  x <- matrix(rmvnorm(n = n, sigma = sigma), ncol = p,
              dimnames = list(NULL, paste0('x', seq_len(p))))
  x_df <- as.data.frame(x)
  
  # Generate synthetic data
  mod <- genrf$new(x_df, num_trees = 10, min_node_size = 5)
  synth <- mod$sample(n)
  cor(synth)
}

# Run simulation
res_100 <- replicate(repl, sim_fun(100))
res_1k <- replicate(repl, sim_fun(1000))
res_10k <- replicate(repl, sim_fun(10000))

# Average over replications
res_mean_100 <- apply(res_100, 1:2, mean)
res_mean_1k <- apply(res_1k, 1:2, mean)
res_mean_10k <- apply(res_10k, 1:2, mean)

# Prepare table for manuscript
tab0 <- matrix(sprintf("%.2f", sigma), 
               nrow = nrow(sigma), ncol = ncol(sigma))
tab0[upper.tri(tab0)] <- ""
diag(tab0) <- ""

tab1 <- matrix(sprintf("%.2f", res_mean_100), 
               nrow = nrow(res_mean_100), ncol = ncol(res_mean_100))
tab1[upper.tri(tab1)] <- ""
diag(tab1) <- ""

tab2 <- matrix(sprintf("%.2f", res_mean_1k), 
               nrow = nrow(res_mean_1k), ncol = ncol(res_mean_1k))
tab2[upper.tri(tab2)] <- ""
diag(tab2) <- ""

tab3 <- matrix(sprintf("%.2f", res_mean_10k), 
               nrow = nrow(res_mean_10k), ncol = ncol(res_mean_10k))
tab3[upper.tri(tab3)] <- ""
diag(tab3) <- ""

tab <- rbind(tab0, tab1, tab2, tab3)
print(xtable(tab), booktabs = TRUE)
