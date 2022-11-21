# Load libraries, register cores, set seed
library(ggplot2)
library(ggsci)
library(data.table)
library(ranger)
library(fdm2id)
library(mlbench)
library(genrf)
library(doMC)
registerDoMC(8)
set.seed(123)

# Simulation function
sim_fun <- function(b, n, dataset) {
  # Simulate data
  if (dataset == 'twomoons') {
    x <- data.twomoons(n = 2*n, graph = FALSE)
    x$Class <- gsub('Class ', '', x$Class)
  } else {
    if (dataset == 'cassini') {
      tmp <- mlbench.cassini(2*n)
    } else if (dataset == 'smiley') {
      tmp <- mlbench.smiley(2*n)
    } else if (dataset == 'shapes') {
      tmp <- mlbench.shapes(2*n)
    }
    x <- data.frame(tmp$x, tmp$classes)
    colnames(x) <- c('X', 'Y', 'Class')
  }
  
  # Train/test split
  trn_idx <- sample(2*n, n)
  tst_idx <- setdiff(1:(2*n), trn_idx)
  trn_x <- x[trn_idx, ]
  tst_x <- x[tst_idx, ]
  
  # Fit model, generate data
  arf <- adversarial_rf(trn_x, num_trees = 100, mtry = 2, parallel = FALSE)
  f0 <- forde(arf, x_trn = trn_x, x_tst = tst_x, parallel = FALSE)
  
  dat <- data.table(trn_x, y = rbinom(n, size = 1, prob = 0.5))
  rf <- ranger(y ~ ., data = dat, num.trees = 100, min.node.size = 5,
               mtry = 2, keep.inbag = TRUE, classification = TRUE,
               num.threads = 1)
  f1 <- forde(rf, x_trn = trn_x, x_tst = tst_x, parallel = FALSE)
  
  # Put it all together, export
  df <- data.table('b' = b, 'n' = n, 'Dataset' = dataset,
                   'ARF' = -mean(f0$loglik), 
                   'CRT' = -mean(f1$loglik))
  return(df)
}

# Execute in parallel
dsets <- c('twomoons', 'cassini', 'smiley', 'shapes')
df <- foreach(bb = 1:20, .combine = rbind) %:%
  foreach(nn = round(10^(seq(2, 3.5, length.out = 10))), .combine = rbind) %:%
  foreach(dd = dsets, .combine = rbind) %dopar% sim_fun(bb, nn, dd)

# Plot
tmp0 <- melt(df, id.vars = c('b', 'n', 'Dataset'), 
             measure.vars = c('ARF', 'CRT'),
             variable.name = 'Method', value.name = 'NLL')
tmp <- tmp0[, mean(NLL), by = .(n, Dataset, Method)]
colnames(tmp)[4] <- 'NLL'
tmp[, se := tmp0[, sd(NLL), by = .(n, Dataset, Method)]$V1]
ggplot(tmp, aes(n, NLL, shape = Method, color = Method, fill = Method, 
                ymin = NLL - se, ymax = NLL + se)) + 
  geom_point() +
  geom_path() + 
  geom_ribbon(alpha = 0.25, color = NA) + 
  scale_x_log10() + 
  scale_color_d3() +
  scale_fill_d3() + 
  facet_wrap(~ Dataset, scales = 'free') + 
  theme_bw()

