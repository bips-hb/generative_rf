
library(data.table)
library(genrf)
library(doParallel)
doParallel::registerDoParallel(20)
source("myloglik.R")

trn <- fread("power_trn.csv")
tst <- fread("power_tst.csv")
#val <- fread("power_val.csv")

mod <- genrf$new(trn, num_trees = 10, min_node_size = 5, oob = FALSE)

ll <- myloglik(mod, tst)
-mean(ll[is.finite(ll)], na.rm = TRUE)

# Gaussian
est <- Rfast::mvnorm.mle(as.matrix(trn))
-mean(mvtnorm::dmvnorm(tst, mean = est$mu, sigma = est$sigma, log = TRUE))