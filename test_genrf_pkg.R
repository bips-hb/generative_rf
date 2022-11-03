

library(ggplot2)
library(GGally)
library(genrf)

grf <- genrf$new(iris, num_trees = 10, leaf_size = 5)
grf$sample(10)
synth <- grf$sample(500)

df <- rbind(data.frame(data = "original", iris),
            data.frame(data = "synthetic", synth))
ggpairs(df, columns = 2:6, ggplot2::aes(colour=data, shape=data)) +
  theme_bw()

