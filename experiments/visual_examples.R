# Load libraries, register cores, set seed
library(ggplot2)
library(ggsci)
library(data.table)
library(ranger)
library(fdm2id)
library(mlbench)
library(genrf)
library(doMC)
registerDoMC(8)
set.seed(123)

# Simulation function
sim_fun <- function(n, dataset) {
  # Simulate data
  if (dataset == 'twomoons') {
    x <- data.twomoons(n = n, graph = FALSE)
    x$Class <- gsub('Class ', '', x$Class)
  } else {
    if (dataset == 'cassini') {
      tmp <- mlbench.cassini(n)
    } else if (dataset == 'smiley') {
      tmp <- mlbench.smiley(n)
    } else if (dataset == 'shapes') {
      tmp <- mlbench.shapes(n)
    }
    x <- data.frame(tmp$x, tmp$classes)
    colnames(x) <- c('X', 'Y', 'Class')
  }
  # Fit model, generate data
  mod <- genrf$new(x, num_trees = 10, min_node_size = 5, mtry = 2)
  synth <- mod$sample(n)
  # Put it all together, export
  df <- rbind(data.frame(Data = "Original", x), 
              data.frame(Data = "Synthetic", synth))
  df$Dataset <- dataset
  return(df)
}

# Execute in parallel
dsets <- c('twomoons', 'cassini', 'smiley', 'shapes')
df <- foreach(d = dsets, .combine = rbind) %dopar% sim_fun(1000, d)

# Scatter plot
ggplot(df, aes(x = X, y = Y, color = Class, shape = Class)) + 
  facet_grid(Data ~ Dataset) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  #theme(legend.position = 'bottom') +
  scale_color_npg()
ggsave(paste0("examples", ".pdf"), width = 8, height = 4)



