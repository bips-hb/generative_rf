

library(ggplot2)
library(GGally)
source("generative_ranger.R")

synth <- generative_ranger(iris, num_trees = 10, leaf_size = 5, n_new = 500)

df <- rbind(data.frame(data = "original", iris),
            data.frame(data = "synthetic", synth))
ggpairs(df, columns = 2:6, ggplot2::aes(colour=data, shape=data)) +
  theme_bw()

