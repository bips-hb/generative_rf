
library(data.table)
library(batchtools)
library(ggplot2)
library(ggsci)

set.seed(42)


# Simulation parameters ---------------------------------------------------
repls <- 20
n <- c(seq(100, 1000, by = 100), seq(2000, 10000, 1000))
p <- c(2, 4, 6, 8, 10)
cov_base <- .3
num.trees <- 10
min.node.size <- 5
oob <- FALSE

# Registry ----------------------------------------------------------------
reg_name <- "kl_by_n"
reg_dir <- file.path("registries", reg_name)
unlink(reg_dir, recursive = TRUE)
makeExperimentRegistry(file.dir = reg_dir, 
                       packages = c("genrf", "mvtnorm", "Rfast", "monomvn"))

# Problems -----------------------------------------------------------
myprob <- function(job, data, n, p, cov_base) {
  mu <- rep(0, p)
  sigma <- toeplitz(cov_base^(0:(p-1)))
  x <- matrix(Rfast::rmvnorm(n = n, mu = mu, sigma = sigma), ncol = p,
              dimnames = list(NULL, paste0('x', seq_len(p))))
  as.data.frame(x)
}
addProblem(name = "myprob", fun = myprob, seed = 43)

# Algorithms -----------------------------------------------------------
run_genrf <- function(data, job, instance, ...) {
  # Generate synthetic data
  mod <- genrf$new(instance,  ...)
  x_new <- mod$sample(nrow(instance))
  
  # Calculate KL divergence
  p <- ncol(instance)
  mu <- rep(0, p)
  sigma <- toeplitz(cov_base^(0:(p-1)))
  est <- Rfast::mvnorm.mle(as.matrix(x_new))
  monomvn::kl.norm(mu1 = mu, S1 = sigma,
                   mu2 = est$mu, S2 = est$sigma)
}
addAlgorithm(name = "genrf", fun = run_genrf)

# Experiments -----------------------------------------------------------
prob_design <- list(myprob = expand.grid(n = n, 
                                         p = p, 
                                         cov_base = cov_base,
                                         stringsAsFactors = FALSE))
algo_design <- list(genrf = expand.grid(num.trees = num.trees, 
                                        min.node.size = min.node.size,
                                        oob = oob,
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

# Load results
res <- readRDS(paste0(reg_name, ".Rds"))

# Plot KL by n ------------------------------------------------------------
res_mean <- res[, mean(KL), by = .(n, p, cov_base, num.trees, min.node.size, oob)]
res_mean[, KL := V1]
res_mean[, Dimensionality := as.factor(p)]

ggplot(res_mean, aes(x = n, y = KL, col = Dimensionality, shape = Dimensionality)) + 
  geom_line() + geom_point() + 
  geom_hline(yintercept = 0) + 
  theme_bw() + 
  xlab("Sample size") + 
  ylab("KL divergence") + 
  scale_x_continuous(trans='log10') + 
  scale_color_npg()

ggsave(paste0(reg_name, ".pdf"), width = 8, height = 5)

