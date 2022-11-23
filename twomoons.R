
library(fdm2id)
library(genrf)
library(ggplot2)
library(ggsci)

source("arf.R")
source("generative_ranger.R")
source("genrf_simple.R")

n <- 1000

x <- data.twomoons(n = n, graph = FALSE)

# ARF (default)
arf <- adversarial_rf(x, num_trees = 10, min_node_size = 5, mtry = 2)
fd <- forde(arf, x_trn = x)
synth_arf <- forge(psi = fd$psi, m = n)

# ARF (no prune and truncate)
arf <- adversarial_rf(x, num_trees = 10, min_node_size = 5, mtry = 2, prune = FALSE)
fd <- forde(arf, x_trn = x, truncate = FALSE)
synth_arf_nopt <- forge(psi = fd$psi, m = n)

# GRF (genrf package)
mod <- genrf$new(x, num_trees = 10, leaf_size = 5, mtry = 2)
synth_genrf <- mod$sample(n)

# GRF (genrf_simple)
rf <- urf(x, num_trees = 10, leaf_size = 5, mtry = 2)
fd <- forde_fn(rf, x)
synth_genrf_simple <- forge_fn(rf, x, fd, n)

# GRF (generative_ranger)
synth_gran <- generative_ranger(x, n_new = n, num_trees = 10, leaf_size = 5, mtry = 2)

# Plot
df <- rbind(data.frame(Method = "ARF Default", synth_arf), 
            data.frame(Method = "ARF w/o prune/truncate", synth_arf_nopt), 
            data.frame(Method = "GRF genrf package", synth_genrf), 
            data.frame(Method = "GRF genrf simple", synth_genrf_simple), 
            data.frame(Method = "GRF generative_ranger", synth_gran))
ggplot(df, aes(x = X, y = Y, color = Class, shape = Class)) + 
  facet_wrap(~ Method) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  #theme(legend.position = 'bottom') +
  scale_color_npg()
