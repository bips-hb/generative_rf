
library(data.table)
library(doParallel)
doParallel::registerDoParallel(20)
library(ranger)
library(truncnorm)
source("arf.R")

n <- 1000
p <- 4
mu <- rep(0, p)
cov_base <- 0.5
sigma <- toeplitz(cov_base^(0:(p-1)))
trn <- matrix(Rfast::rmvnorm(n = n, mu = mu, sigma = sigma), ncol = p,
              dimnames = list(NULL, paste0('x', seq_len(p))))
tst <- matrix(Rfast::rmvnorm(n = n, mu = mu, sigma = sigma), ncol = p,
              dimnames = list(NULL, paste0('x', seq_len(p))))

# Gaussian
est <- Rfast::mvnorm.mle(as.matrix(trn))
-mean(mvtnorm::dmvnorm(tst, mean = est$mu, sigma = est$sigma, log = TRUE))

# FORDE
arf <- adversarial_rf(trn, num_trees = 10, min_node_size = 10, delta = 0)
fd <- forde(arf, x_trn = trn, x_tst = tst, alpha = 0.01)

# Performance too good to be true
-mean(fd$loglik)

# Many -Inf values
table(is.finite(fd$loglik))

