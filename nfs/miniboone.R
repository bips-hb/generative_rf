
library(data.table)
library(genrf)
library(doParallel)
doParallel::registerDoParallel(20)
source("myloglik.R")

trn <- fread("miniboone_trn.csv")
tst <- fread("miniboone_tst.csv")
#val <- fread("miniboone_val.csv")

mod <- genrf$new(trn, num_trees = 10, min_node_size = 5, oob = FALSE)

num_trees <- mod$.__enclos_env__$private$num_trees
ll <- myloglik(mod, tst)
-mean(ll[is.finite(ll)], na.rm = TRUE)

# # Gaussian
# est <- Rfast::mvnorm.mle(as.matrix(trn))
# -mean(mvtnorm::dmvnorm(tst, mean = est$mu, sigma = est$sigma, log = TRUE))
