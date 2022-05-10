
library(data.table)
library(batchtools)
library(ggplot2)
library(ggsci)
library(cowplot)
library(scales)

set.seed(42)

# KL by n -----------------------------------------------------------------
res_n <- readRDS("kl_by_n_mean.Rds")
p1 <- ggplot(res_n, aes(x = n, y = KL, col = Method)) + 
  geom_line() + 
  #geom_hline(yintercept = 0) + 
  xlab("Sample size") + 
  ylab("KL divergence") + 
  scale_x_continuous(trans='log10') + 
  scale_y_continuous(trans='log10') + 
  scale_color_npg() + 
  theme_bw() + 
  theme(legend.position = "none")

# KL by informative features ----------------------------------------------
res_inf <- readRDS("kl_by_effects_mean.Rds")
res_inf[, effect_cols := effect_cols / max(effect_cols)]
p2 <- ggplot(res_inf, aes(x = effect_cols, y = KL, col = Method)) + 
  geom_line() + 
  #geom_hline(yintercept = 0) + 
  xlab("Proportion of informative features") + 
  scale_color_npg() + 
  scale_x_continuous(breaks = pretty_breaks()) + 
  scale_y_continuous(trans = 'log10', breaks = c(.2, .5, 1)) + 
  theme_bw() + 
  theme(axis.title.y = element_blank(), 
        legend.text = element_text(lineheight = .8), 
        legend.key.height=unit(22, "pt"))

# Plot together -----------------------------------------------------------
plot_grid(p1, p2, ncol = 2, rel_widths = c(.39, .61))
ggsave("kl.pdf", width = 8, height = 3)

