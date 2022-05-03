
library(ggplot2)
library(ggsci)
library(data.table)
library(ranger)
library(fdm2id)
library(genrf)

n <- c(100, 1000)

sim_fun <- function(n) {
  x <- data.twomoons(n = n, graph = FALSE)
  
  mod <- genrf$new(x, num.trees = 10, min.node.size = 5, mtry = 2)
  synth <- mod$sample(n)
  
  df <- rbind(data.frame(Data = "Original", x), 
              data.frame(Data = "Synthetic", synth))
  df$n <- paste0("n = ", n)
  df
}
df <- rbindlist(lapply(n, sim_fun))

# Scatter plot
ggplot(df, aes(x = X, y = Y, col = Data, shape = Class)) + 
  facet_wrap(~ n) + 
  geom_point() + 
  theme_bw() + 
  scale_color_npg()
ggsave(paste0("twomoons", ".pdf"), width = 8.5, height = 3.8)

# # Plot pairs
# library(GGally)
# ggpairs(df, columns = 2:3, ggplot2::aes(col = Data, shape = Data)) + 
#   theme_bw()

