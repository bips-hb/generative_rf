# Load libraries, register cores
library(data.table)
library(ranger)
library(truncnorm)
library(Rfast)
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
  x <- matrix(rmvnorm(n = n + 1000, mu = mu, sigma = sigma), ncol = d,
              dimnames = list(NULL, paste0('x', 1:d)))
  y <- rbinom(n + 1000, size = 1, prob = plogis(x %*% beta))
  
  # Split train/test
  trn_x <- x[1:n, ]
  trn_y <- y[1:n]
  tst_x <- x[(n+1):(n + 1000), ]
  
  # Adversarial RF
  arf <- adversarial_rf(trn_x, num_trees = 100, max_iters = 1, parallel = FALSE)
  # Truncated normal density
  fd_tnorm <- forde(arf, x_trn = trn_x, x_tst = tst_x, dist = 'truncnorm',
                    parallel = FALSE)
  # PWC unsupervised
  fd_pwc_u <- forde(arf, x_trn = trn_x, x_tst = tst_x, dist = 'unif',
                    parallel = FALSE)

  # Competition
  trn_dat <- data.table(y = trn_y, trn_x)
  rf <- ranger(y ~ ., data = trn_dat, num.trees = 100, min.node.size = 5,
               keep.inbag = TRUE, classification = TRUE, num.threads = 1)
  # Correia
  fd_gef <- forde(rf, x_trn = trn_x, x_tst = tst_x, dist = 'norm',
                  prune = FALSE, parallel = FALSE)
  # PWC supervised
  fd_pwc_s <- forde(rf, x_trn = trn_x, x_tst = tst_x, dist = 'unif',
                    prune = FALSE, parallel = FALSE)
  
  # Export
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

# By signal sparsity
df2 <- foreach(bb = 1:20, .combine = rbind) %:%
  foreach(sp = seq(0, 1, by = 0.1), .combine = rbind) %dopar%
  sim_exp(b = bb, n = 2000, d = 10, sparsity = sp)


df <- rbind(df1, df2)
saveRDS(df, 'NLL_exp.rds')

# Plot: NLL by sample size
tmp0 <- melt(df1, id.vars = c('b', 'n'), 
             measure.vars = c('ARF', 'PWCu', 'GeF', 'PWCs'),
             variable.name = 'Method', value.name = 'NLL')
tmp <- tmp0[, mean(NLL), by = .(n, Method)]
colnames(tmp)[3] <- 'NLL'
tmp[, se := tmp0[, sd(NLL), by = .(n, Method)]$V1]
ggplot(tmp, aes(n, NLL, shape = Method, color = Method, fill = Method, 
                ymin = NLL - se, ymax = NLL + se)) + 
  geom_point() +
  geom_path() + 
  geom_ribbon(alpha = 0.25, color = NA) + 
  scale_x_log10() + 
  scale_color_d3() +
  scale_fill_d3() + 
  theme_bw()

# Plot: NLL by sparsity
tmp0 <- melt(df2, id.vars = c('b', 'sparsity'), 
             measure.vars = c('ARF', 'PWCu', 'GeF', 'PWCs'),
             variable.name = 'Method', value.name = 'NLL')
tmp <- tmp0[, mean(NLL), by = .(sparsity, Method)]
colnames(tmp)[3] <- 'NLL'
tmp[, se := tmp0[, sd(NLL), by = .(sparsity, Method)]$V1]
ggplot(tmp, aes(sparsity, NLL, shape = Method, color = Method, fill = Method, 
                ymin = NLL - se, ymax = NLL + se)) + 
  geom_point() +
  geom_path() + 
  geom_ribbon(alpha = 0.25, color = NA) + 
  scale_color_d3() +
  scale_fill_d3() + 
  theme_bw()






