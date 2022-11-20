# Load libraries, register cores
library(data.table)
library(ranger)
library(truncnorm)
library(mvtnorm)
library(Rfast)
library(monomvn)
library(ggplot2)
library(ggsci)
library(doMC)
registerDoMC(10)

# Load ARF
source('arf.R')

# Set seed
set.seed(123, "L'Ecuyer-CMRG")

# Simulation script

sim_exp <- function(b, n, d, sparsity) {
  # Effects 
  beta <- double(length = d)
  k <- round((1 - sparsity) * d)
  beta[sample(d, k)] <- sample(c(1, -1), k, replace = TRUE)
  
  # Parameters
  mu <- rep(0, d)
  sigma <- toeplitz(0.5^(0:(d-1)))
  
  # Create data
  x <- matrix(Rfast::rmvnorm(n = 2 * n, mu = mu, sigma = sigma), ncol = d,
              dimnames = list(NULL, paste0('x', 1:d)))
  y <- rbinom(n, size = 1, prob = plogis(x %*% beta))
  
  # Split train/test
  trn_x <- x[1:n, ]
  trn_y <- y[1:n]
  tst_x <- x[(n+1):(2*n), ]
  tst_y <- y[(n+1):(2*n)]
  
  # Adversarial RF
  arf <- adversarial_rf(trn_x, max_iters = 1)
  # Truncated normal density
  fd_tnorm <- forde(arf, x_trn = trn_x, x_tst = tst_x, dist = 'truncnorm')
  # PWC unsupervised
  fd_pwc_u <- forde(arf, x_trn = trn_x, x_tst = tst_x, dist = 'unif')

  # Competition
  trn_dat <- data.table(y = trn_y, trn_x)
  rf <- ranger(y ~ ., data = trn_dat,
               keep.inbag = TRUE, classification = TRUE, 
               respect.unordered.factors = TRUE)
  # Correia
  fd_gef <- forde(rf, x_trn = trn_x, x_tst = tst_x, dist = 'norm',
                  prune = FALSE)
  # PWC supervised
  fd_pwc_s <- forde(rf, x_trn = trn_x, x_tst = tst_x, dist = 'unif',
                    prune = FALSE)
  
  # Results
  out <- data.table(
    'b' = b, 'n' = n, 'd' = d, 'sparsity' = sparsity,
    'ARF' = -mean(fd_tnorm$loglik), 'PWCu' = -mean(fd_pwc_u$loglik),
    'GeF' = -mean(fd_gef$loglik), 'PWCs' = -mean(fd_pwc_s$loglik)
  )
  return(out)
}

# By sample size
df1 <- foreach(bb = 1:20, .combine = rbind) %:%
  foreach(nn = round(10^(seq(2, 4, length.out = 10))), .combine = rbind) %dopar%
  sim_exp(b = bb, n = nn, d = 10, sparsity = 1/2)

# By data dimensionality
df2 <- foreach(bb = 1:20, .combine = rbind) %:%
  foreach(dd = seq(2, 200, length.out = 10), .combine = rbind) %dopar%
  sim_exp(b = bb, n = 2000, d = dd, sparsity = 1/2)


# By signal sparsity
df3 <- foreach(bb = 1:20, .combine = rbind) %:%
  foreach(sp = seq(0, 1, by = 0.1), .combine = rbind) %dopar%
  sim_exp(b = bb, n = 2000, d = 10, sparsity = sp)


df <- rbind(df1, df2, df3)
saveRDS(df, 'NLL_exp.rds')

# Plot: NLL by sample size
tmp0 <- melt(df1, id.vars = c('b', 'n'), 
             measure.vars = c('ARF', 'PWCu', 'GeF', 'PWCs'),
             variable.name = 'Method', value.name = 'NLL')
tmp <- tmp0[, mean(NLL), by = .(n, Method)]
colnames(tmp)[3] <- 'NLL'
tmp[, se := tmp0[, sd(NLL), by = .(n, Method)]$V1]
ggplot(tmp, aes(n, NLL, color = Method, fill = Method, 
                ymin = NLL - se, ymax = NLL + se)) + 
  geom_path() + 
  geom_ribbon(alpha = 0.5) + 
  scale_x_log10() + 
  scale_color_npg() +
  scale_fill_npg() + 
  theme_bw()

# Plot: NLL by dimensionality
tmp0 <- melt(df2, id.vars = c('b', 'd'), 
             measure.vars = c('ARF', 'PWCu', 'GeF', 'PWCs'),
             variable.name = 'Method', value.name = 'NLL')
tmp <- tmp0[, mean(NLL), by = .(d, Method)]
colnames(tmp)[3] <- 'NLL'
tmp[, se := tmp0[, sd(NLL), by = .(d, Method)]$V1]
ggplot(tmp, aes(d, NLL, color = Method, fill = Method, 
                ymin = NLL - se, ymax = NLL + se)) + 
  geom_path() + 
  geom_ribbon(alpha = 0.5) + 
  scale_color_npg() +
  scale_fill_npg() + 
  theme_bw()

# Plot: NLL by sparsity
tmp0 <- melt(df3, id.vars = c('b', 'sparsity'), 
             measure.vars = c('ARF', 'PWCu', 'GeF', 'PWCs'),
             variable.name = 'Method', value.name = 'NLL')
tmp <- tmp0[, mean(NLL), by = .(sparsity, Method)]
colnames(tmp)[3] <- 'NLL'
tmp[, se := tmp0[, sd(NLL), by = .(sparsity, Method)]$V1]
ggplot(tmp, aes(sparsity, NLL, color = Method, fill = Method, 
                ymin = NLL - se, ymax = NLL + se)) + 
  geom_path() + 
  geom_ribbon(alpha = 0.5) + 
  scale_color_npg() +
  scale_fill_npg() + 
  theme_bw()






