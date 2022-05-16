
library(data.table)
library(batchtools)
library(ggplot2)
library(ggsci)
library(cowplot)
library(scales)

set.seed(42)

# KL by n -----------------------------------------------------------------
res_n <- readRDS("kl_by_n.Rds")
res_n[, KL_mean := mean(KL), by = .(n, p, cov_base, num_trees, min_node_size, oob, dist, algorithm, effect_cols, beta)]
res_n[, KL_lo := quantile(KL, probs = c(.05)), by = .(n, p, cov_base, num_trees, min_node_size, oob, dist, algorithm, effect_cols, beta)]
res_n[, KL_hi := quantile(KL, probs = c(.95)), by = .(n, p, cov_base, num_trees, min_node_size, oob, dist, algorithm, effect_cols, beta)]
res_n[, Method := factor(paste(algorithm, dist, sep = "_"), 
                           levels = c("correia_pwc", "genrf_pwc", "correia_normal", "genrf_normal"), 
                           labels = c("Piecewise constant\n(supervised)", "Piecewise constant\n(unsupervised)", "GeFs (Correia et al.)", "FORGE"))]
res_n <- unique(res_n[, .(KL_mean, KL_lo, KL_hi, n, Method)])

p1 <- ggplot(res_n, aes(x = n)) + 
  geom_line(aes(col = Method, y = KL_mean)) + 
  geom_ribbon(aes(ymin = KL_lo, ymax = KL_hi, fill = Method), alpha = .1) + 
  #geom_hline(yintercept = 0) + 
  xlab("Sample size") + 
  ylab("KL divergence") + 
  scale_x_continuous(trans='log10') + 
  scale_y_continuous(trans='log10') + 
  scale_color_npg() + 
  theme_bw() + 
  theme(legend.position = "none")

# KL by informative features ----------------------------------------------
res_inf <- readRDS("kl_by_effects.Rds")
res_inf[, KL_mean := mean(KL), by = .(n, p, cov_base, num_trees, min_node_size, oob, dist, algorithm, effect_cols, beta)]
res_inf[, KL_lo := quantile(KL, probs = c(.05)), by = .(n, p, cov_base, num_trees, min_node_size, oob, dist, algorithm, effect_cols, beta)]
res_inf[, KL_hi := quantile(KL, probs = c(.95)), by = .(n, p, cov_base, num_trees, min_node_size, oob, dist, algorithm, effect_cols, beta)]
res_inf[, Method := factor(paste(algorithm, dist, sep = "_"), 
                           levels = c("correia_pwc", "genrf_pwc", "correia_normal", "genrf_normal"), 
                           labels = c("Piecewise constant\n(supervised)", "Piecewise constant\n(unsupervised)", "GeFs (Correia et al.)", "FORGE"))]
res_inf[, effect_cols := effect_cols / max(effect_cols)]
res_inf <- unique(res_inf[, .(KL_mean, KL_lo, KL_hi, effect_cols, Method)])

p2 <- ggplot(res_inf, aes(x = effect_cols)) + 
  geom_line(aes(col = Method, y = KL_mean)) + 
  geom_ribbon(aes(ymin = KL_lo, ymax = KL_hi, fill = Method), alpha = .1) + 
  #geom_hline(yintercept = 0) + 
  xlab("Proportion of informative features") + 
  scale_color_npg() + 
  scale_fill_npg() + 
  scale_x_continuous(breaks = pretty_breaks()) + 
  scale_y_continuous(trans = 'log10', breaks = c(.2, .5, 1)) + 
  theme_bw() + 
  theme(axis.title.y = element_blank(), 
        legend.text = element_text(lineheight = .8), 
        legend.key.height=unit(22, "pt"))

# Plot together -----------------------------------------------------------
plot_grid(p1, p2, ncol = 2, rel_widths = c(.39, .61), labels = "AUTO", label_x = c(.06, 0))
ggsave("kl.pdf", width = 8, height = 3)

