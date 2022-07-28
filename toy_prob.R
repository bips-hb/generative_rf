# Load libraries, set seed
library(data.table)
library(ranger)
library(ggplot2)
library(viridis)
library(doMC)
registerDoMC(8)

# Set seed
set.seed(123, kind = "L'Ecuyer-CMRG")

# Simulate data
n <- 1e5L
x1 <- rnorm(n)
x2 <- x1 + rnorm(n, sd = 0.25)
x1_tilde <- sample(x1, n, replace = TRUE)
x2_tilde <- sample(x2, n, replace = TRUE)
df <- data.table('x1' = c(x1, x1_tilde), 
                 'x2' = c(x2, x2_tilde), 
                 'y' = rep(c(1, 0), each = n))

# Fit URF
urf <- ranger(y ~ ., data = df, classification = TRUE, keep.inbag = TRUE)

# Identify OOB cases
oob_idx <- ifelse(simplify2array(urf$inbag.counts) == 0, TRUE, NA)
 
# Average over trees for just OOB cases
preds <- predict(urf, df, predict.all = TRUE)$predictions
df[, yhat := rowMeans(oob_idx * preds, na.rm = TRUE)] 

# Plot
ggplot(df[y == 1], aes(x1, x2, color = yhat)) + 
  geom_point(size = 0.25, alpha = 0.25) + 
  scale_color_viridis() + 
  theme_bw()