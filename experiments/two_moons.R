
library(ggplot2)
library(ranger)
library(fdm2id)
library(genrf)

n <- 1000
x <- data.twomoons(n = n, graph = FALSE)

mod <- genrf$new(x, num.trees = 10, min.node.size = 5, mtry = 2)
synth <- mod$sample(n)

df <- rbind(data.frame(Data = "Original", x), 
            data.frame(Data = "Synthetic", synth))

# Scatter plot
ggplot(df, aes(x = X, y = Y, col = Data, shape = Class)) + 
  geom_point() + 
  theme_bw()
ggsave(paste0("twomoons", ".pdf"))

# # Plot pairs
# library(GGally)
# ggpairs(df, columns = 2:3, ggplot2::aes(col = Data, shape = Data)) + 
#   theme_bw()

