
library(data.table)
library(batchtools)
library(ggplot2)
library(ggsci)
library(cowplot)
library(scales)

set.seed(42)

# KL by n -----------------------------------------------------------------
res_n <- readRDS("loglik_by_n.Rds")
res_n[, KL_mean := mean(-KL), by = .(n, p, cov_base, num_trees, min_node_size, oob, dist, algorithm, effect_cols, beta)]
res_n[, KL_lo := quantile(-KL, probs = c(.05)), by = .(n, p, cov_base, num_trees, min_node_size, oob, dist, algorithm, effect_cols, beta)]
res_n[, KL_hi := quantile(-KL, probs = c(.95)), by = .(n, p, cov_base, num_trees, min_node_size, oob, dist, algorithm, effect_cols, beta)]
res_n[, Method := factor(paste(algorithm, dist, sep = "_"), 
                         levels = c("correia_pwc", "genrf_pwc", "correia_normal", "genrf_normal"), 
                         labels = c("PWC\n(sup.)", "PWC\n(unsup.)", "GeFs", "FORGE"))]
res_n <- unique(res_n[, .(KL_mean, KL_lo, KL_hi, n, Method)])

p1 <- ggplot(res_n, aes(x = n)) + 
  geom_line(aes(col = Method, y = KL_mean)) + 
  geom_point(aes(col = Method, shape = Method, y = KL_mean)) + 
  geom_ribbon(aes(ymin = KL_lo, ymax = KL_hi, fill = Method), alpha = .1) + 
  #geom_hline(yintercept = 0) + 
  xlab("Sample size") + 
  ylab("Negative log-likelihood") + 
  scale_x_continuous(trans = 'log10') + 
  scale_y_continuous(breaks = c(14, 16, 18, 20)) + 
  #scale_linetype_manual(values = c("twodash", "dashed", "dotted", "solid")) + 
  scale_shape_manual(values = c(16, 3, 17, 15)) + 
  scale_color_nejm() + 
  scale_fill_nejm() + 
  theme_bw()

# KL by informative features ----------------------------------------------
res_inf <- readRDS("loglik_by_effects.Rds")
res_inf[, KL_mean := mean(-KL), by = .(n, p, cov_base, num_trees, min_node_size, oob, dist, algorithm, effect_cols, beta)]
res_inf[, KL_lo := quantile(-KL, probs = c(.05)), by = .(n, p, cov_base, num_trees, min_node_size, oob, dist, algorithm, effect_cols, beta)]
res_inf[, KL_hi := quantile(-KL, probs = c(.95)), by = .(n, p, cov_base, num_trees, min_node_size, oob, dist, algorithm, effect_cols, beta)]
res_inf[, Method := factor(paste(algorithm, dist, sep = "_"), 
                           levels = c("correia_pwc", "genrf_pwc", "correia_normal", "genrf_normal"), 
                           labels = c("PWC\n(sup.)", "PWC\n(unsup.)", "GeFs", "FORGE"))]
res_inf[, effect_cols := effect_cols / max(effect_cols)]
res_inf <- unique(res_inf[, .(KL_mean, KL_lo, KL_hi, effect_cols, Method)])

p2 <- ggplot(res_inf, aes(x = effect_cols)) + 
  geom_line(aes(col = Method, y = KL_mean)) + 
  geom_point(aes(col = Method, shape = Method, y = KL_mean)) + 
  geom_ribbon(aes(ymin = KL_lo, ymax = KL_hi, fill = Method), alpha = .1) + 
  #geom_hline(yintercept = 0) + 
  xlab("Proportion of informative features") + 
  scale_color_nejm() + 
  scale_fill_nejm() + 
  scale_x_continuous(breaks = pretty_breaks()) + 
  scale_y_continuous(breaks = c(14, 16, 18, 20)) + 
  #scale_linetype_manual(values = c("twodash", "dashed", "dotted", "solid")) + 
  scale_shape_manual(values = c(16, 3, 17, 15)) + 
  theme_bw() + 
  theme(axis.title.y = element_blank(), 
        legend.text = element_text(lineheight = .8), 
        legend.key.height=unit(22, "pt"))

# Plot together -----------------------------------------------------------
prow <- plot_grid(p1 + theme(legend.position = "none"), 
                  p2 + theme(legend.position = "none"), 
                  ncol = 2, labels = "AUTO", label_x = c(.08, 0))
legend <- get_legend(
  p1 + theme(legend.position = "bottom")
)
plot_grid(prow, legend, ncol = 1, rel_heights = c(1, .1))
ggsave("loglik.pdf", width = 7, height = 3)

