
library(ggplot2)
library(GGally)
library(ranger)
library(fdm2id)
source("generative_ranger.R")

n <- 200

x_train <- data.twomoons(n = n)[, 1:2]
x_test <- data.twomoons(n = n)[, 1:2]

x_sy_rf <- generative_ranger(x_real = x_train, x_synth = NULL, n_new = 500,
                             num.trees = 100, min.node.size = 5)

x_sy_df <- rbind(data.frame(data = "original", x_train), 
                 data.frame(data = "synthetic", x_sy_rf))
ggpairs(x_sy_df, columns = 2:3, ggplot2::aes(colour=data, shape=data)) + 
  theme_bw()

# Correlations (as in Goncalves et al. 2020)
# Data utility metric, i.e. how close is the data to the original?
util <- function(orig, synth) {
  norm(cor(orig) - cor(synth), type = "F")
}
util(x_train, x_sy_rf)

# Proportion of overfitting (as in Lenz et al. 2021)
# Data disclosure metric, i.e. is it too close to the original?
disc <- function(orig_train, orig_test, synth) {
  (util(orig_test, synth) - util(orig_train, synth)) / util(orig_test, synth)
}
disc(x_train, x_test, x_sy_rf)
