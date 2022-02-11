
library(mvtnorm)

simdat <- function(n, p_cont = 10, p_cat = 10, cov_base = 0.5) {
  # Simulate multivariate normal data (Toeplitz structure)
  mu <- rep(0, p_cont)
  sigma <- toeplitz(cov_base^(0:(p_cont-1)))
  
  x_cont <- matrix(rmvnorm(n = n, mean = mu, sigma = sigma), ncol = p_cont,
                   dimnames = list(NULL, paste0('x_cont', seq_len(p_cont))))
  x_cat <- matrix(round(runif(n = n, min = .5, max = 4.5)), ncol = p_cat,
                  dimnames = list(NULL, paste0('x_cat', seq_len(p_cat))))
  x_cont_df <- as.data.frame(x_cont)
  x_cat_df <- as.data.frame(x_cat)
  x_cat_df <- data.frame(lapply(x_cat_df, factor))
  cbind(x_cont_df, x_cat_df)
}



