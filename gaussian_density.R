
library(data.table)
library(doParallel)
doParallel::registerDoParallel(10)
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
ll_mle <- mvtnorm::dmvnorm(tst, mean = est$mu, sigma = est$sigma, log = TRUE)
-mean(ll_mle)

# Adversarial RF
arf <- adversarial_rf(trn, num_trees = 100, mtry = 3)
fd <- forde(arf, x_trn = trn, x_tst = tst)
ll_arf <- fd$loglik
-mean(ll_arf)

# Any -Inf values?
table(is.finite(ll_arf))

# Plot
library(ggplot2)
df <- data.frame('MLE' = -ll_mle, 'ARF' = -ll_arf)
ggplot(df, aes(MLE, ARF)) + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', color = 'red') + 
  scale_x_log10() + 
  scale_y_log10() + 
  theme_bw()

