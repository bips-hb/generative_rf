# Set working directory
setwd('~/Documents/Kings/generative_rf')

# Load libraries, register cores
library(data.table)
library(arf)
library(doMC)
registerDoMC(10)

# Set seed
set.seed(123, "L'Ecuyer-CMRG")

# Output file
df <- data.table('dataset' = NA_character_, 'NLL' = NA_real_)
saveRDS(df, './experiments/NLL_binary.rds')

# Likelihood evaluation function
nll_fn <- function(dataset) {
  # Load data
  trn <- fread(paste0('./data/', dataset, '/', dataset, '.train.data'))
  val <- fread(paste0('./data/', dataset, '/', dataset, '.valid.data'))
  tst <- fread(paste0('./data/', dataset, '/', dataset, '.test.data'))
  colnames(trn) <- colnames(val) <- colnames(tst) <- paste0('x', 1:ncol(trn))
  # Train
  suppressWarnings(
    arf <- adversarial_rf(trn, num_trees = 100, max_iters = 2, verbose = FALSE)
  )
  suppressWarnings(
    fd <- forde(arf, x_trn = val, x_tst = tst, batch = 5000)
  )
  # Export
  out <- data.table('dataset' = dataset, 'NLL' = -mean(fd$loglik))
  df <- readRDS('./experiments/NLL_binary.rds')
  df <- rbind(df, out)
  saveRDS(df, './experiments/NLL_binary.rds')
}

# Loop through data, compute log-likelihood
datasets <- list.files('./data')
foreach(d = datasets) %do% nll_fn(d)


