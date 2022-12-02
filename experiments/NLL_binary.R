# Set working directory
setwd('~/Documents/Kings/generative_rf/data')

# Load libraries, register cores
library(data.table)
library(arf)
library(doMC)
registerDoMC(10)

# Set seed
set.seed(123, "L'Ecuyer-CMRG")

# Likelihood evaluation function
nll_fn <- function(dataset) {
  # Load data
  trn <- fread(paste0('./', dataset, '/', dataset, '.train.data'))
  val <- fread(paste0('./', dataset, '/', dataset, '.valid.data'))
  tst <- fread(paste0('./', dataset, '/', dataset, '.test.data'))
  colnames(trn) <- colnames(val) <- colnames(tst) <- paste0('x', 1:ncol(trn))
  # Train
  suppressWarnings(
    arf <- adversarial_rf(trn, num_trees = 100, max_iters = 2, verbose = FALSE,
                          parallel = FALSE)
  )
  suppressWarnings(
    fd <- forde(arf, x_trn = val, x_tst = tst, parallel = FALSE)
  )
  # Export
  out <- data.table('dataset' = dataset, 'NLL' = -mean(fd$loglik))
  return(out)
}

# Loop through data, compute log-likelihood
datasets <- list.files()
df <- foreach(d = datasets, .combine = rbind) %dopar%
  nll_fn(d)