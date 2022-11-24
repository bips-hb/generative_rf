
library(data.table)
library(doParallel)
doParallel::registerDoParallel(20)
source("../arf.R")

trn <- fread("bsds300_trn.csv")
tst <- fread("bsds300_tst.csv")
#val <- fread("bsds300_val.csv")

arf <- adversarial_rf(trn, num_trees = 100, min_node_size = 100, delta = 0, max_iter = 1)
fd <- forde(arf, x_trn = trn, x_tst = tst)
-mean(fd$loglik)

# Gaussian
est <- Rfast::mvnorm.mle(as.matrix(trn))
-mean(mvtnorm::dmvnorm(tst, mean = est$mu, sigma = est$sigma, log = TRUE))
