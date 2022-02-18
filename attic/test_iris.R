
library(ggplot2)
library(GGally)
library(ranger)
source("generative_ranger.R")

synth <- generative_ranger(x_real = iris, x_synth = NULL, n_new = 500,
                           num.trees = 100, min.node.size = 5)

df <- rbind(data.frame(data = "original", iris), 
            data.frame(data = "synthetic", synth))
ggpairs(df, columns = 2:6, ggplot2::aes(colour=data, shape=data)) + 
  theme_bw()