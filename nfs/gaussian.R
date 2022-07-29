
library(data.table)
library(genrf)
library(doParallel)
doParallel::registerDoParallel(20)
source("myloglik.R")

n <- 1000
p <- 4
mu <- rep(0, p)
cov_base <- 0.5
sigma <- toeplitz(cov_base^(0:(p-1)))
trn <- matrix(Rfast::rmvnorm(n = n, mu = mu, sigma = sigma), ncol = p,
            dimnames = list(NULL, paste0('x', seq_len(p))))
tst <- matrix(Rfast::rmvnorm(n = n, mu = mu, sigma = sigma), ncol = p,
              dimnames = list(NULL, paste0('x', seq_len(p))))

# FORDE
mod <- genrf$new(trn, num_trees = 10, min_node_size = 5, oob = FALSE) # max.depth = 1
ll <- myloglik(mod, tst)
-mean(ll[is.finite(ll)], na.rm = TRUE)

# Gaussian
est <- Rfast::mvnorm.mle(as.matrix(trn))
-mean(mvtnorm::dmvnorm(tst, mean = est$mu, sigma = est$sigma, log = TRUE))

# We are a little bit worse, as expected