
library(data.table)
library(batchtools)
library(ggplot2)
library(ggsci)
library(scales)

set.seed(42)

# Simulation parameters ---------------------------------------------------
repls <- 20
n <- 10000
p <- 10 
effect_cols <- seq(0, p, by = 2)
cov_base <- .5
num_trees <- 10
min_node_size <- 5
oob <- FALSE
dist <- c("normal", "pwc")
beta <- 1

# Registry ----------------------------------------------------------------
reg_name <- "kl_by_effects"
reg_dir <- file.path("registries", reg_name)
unlink(reg_dir, recursive = TRUE)
makeExperimentRegistry(file.dir = reg_dir, 
                       packages = c("genrf", "mvtnorm", "Rfast", "monomvn"), 
                       source = "correia.R")

# Problems -----------------------------------------------------------
myprob <- function(job, data, n, p, cov_base, effect_cols, beta) {
  # Effects 
  beta <- c(rep(beta, effect_cols), rep(0, p-effect_cols))
  
  # Correlation matrix
  mu <- rep(0, p)
  sigma <- toeplitz(cov_base^(0:(p-1)))
  
  # Create data
  x <- matrix(Rfast::rmvnorm(n = n, mu = mu, sigma = sigma), ncol = p,
              dimnames = list(NULL, paste0('x', seq_len(p))))
  lp <- x %*% beta
  #y <- lp + rnorm(n)
  y <- as.factor(rbinom(n, size = 1, prob = plogis(lp)))
  
  # Return data, sigma, mu
  list(data = data.frame(yy = y, x), 
       sigma = sigma, 
       mu = mu)
}
addProblem(name = "myprob", fun = myprob, seed = 43)

# Algorithms -----------------------------------------------------------
run_genrf <- function(data, job, instance, ...) {
  # Generate synthetic data
  mod <- genrf$new(instance$data,  ...)
  x_new <- mod$sample(nrow(instance$data))
  
  # Calculate KL divergence
  est <- Rfast::mvnorm.mle(as.matrix(x_new[, -1]))
  monomvn::kl.norm(mu1 = instance$mu, S1 = instance$sigma,
                   mu2 = est$mu, S2 = est$sigma)
}
addAlgorithm(name = "genrf", fun = run_genrf)

run_correia <- function(data, job, instance, ...) {
  # Generate synthetic data
  x_new <- correia(x_real = instance$data, label = "yy", n_new = nrow(instance$data), ...)
  
  # Calculate KL divergence
  est <- Rfast::mvnorm.mle(as.matrix(x_new[, -1]))
  monomvn::kl.norm(mu1 = instance$mu, S1 = instance$sigma,
                   mu2 = est$mu, S2 = est$sigma)
}
addAlgorithm(name = "correia", fun = run_correia)

# Experiments -----------------------------------------------------------
prob_design <- list(myprob = expand.grid(n = n, 
                                         p = p, 
                                         effect_cols = effect_cols,
                                         cov_base = cov_base,
                                         beta = beta,
                                         stringsAsFactors = FALSE))
algo_design <- list(genrf = expand.grid(num_trees = num_trees, 
                                        min_node_size = min_node_size,
                                        oob = oob,
                                        dist = dist,
                                        stringsAsFactors = FALSE), 
                    correia = expand.grid(num_trees = num_trees, 
                                          min_node_size = min_node_size,
                                          oob = oob,
                                          dist = dist,
                                          stringsAsFactors = FALSE))
addExperiments(prob_design, algo_design, repls = repls)
summarizeExperiments()

# Test jobs -----------------------------------------------------------
#testJob(id = 1)

# Submit -----------------------------------------------------------
if (grepl("node\\d{2}|bipscluster", system("hostname", intern = TRUE))) {
  ids <- findNotStarted()
  ids[, chunk := chunk(job.id, chunk.size = 50)]
  submitJobs(ids = ids, # walltime in seconds, 10 days max, memory in MB
             resources = list(name = reg_name, chunks.as.arrayjobs = TRUE, 
                              ncpus = 1, memory = 6000, walltime = 10*24*3600, 
                              max.concurrent.jobs = 400))
} else {
  submitJobs()
}
waitForJobs()

# Get results -------------------------------------------------------------
res <-  flatten(ijoin(reduceResultsDataTable(), getJobPars()))
res[, KL := result.1]

# Save result
saveRDS(res, paste0(reg_name, ".Rds"))