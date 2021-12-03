
library(ggplot2)
library(GGally)
library(ranger)
library(fdm2id)
source("generative_ranger.R")

n <- 200
x <- data.twomoons(n = n)[, 1:2]

synth <- generative_ranger(x_real = x, x_synth = NULL, n_new = 500,
                           num.trees = 100, min.node.size = 5)

df <- rbind(data.frame(data = "original", x), 
            data.frame(data = "synthetic", synth))
ggpairs(df, columns = 2:3, ggplot2::aes(colour=data, shape=data)) + 
  theme_bw()
