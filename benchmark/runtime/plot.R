library(dplyr)
library(ggplot2)
library(reshape2)
library(ggsci)
library(cowplot)
################################
# subsample size
################################

files <- c(list.files(path = "./results_samplesize",
                      pattern = ".*\\.csv$",
                      all.files = FALSE, full.names = TRUE, recursive = FALSE, ignore.case = TRUE,
                      include.dirs = FALSE, no.. = FALSE))
df <- data.frame()
for (i in 1:length(files)) {
  my_df <- read.csv(files[i])
  my_df$processing_unit <- ifelse(grepl("cpu", files[i]), "CPU", "GPU")
  df <- bind_rows(df, my_df)
} 

df =df %>% group_by(n, processing_unit, model) %>% 
  # calculate mean for training time
  mutate(mean_process_time_train = mean(process_time_train), mean_wall_time_train = mean(wall_time_train),
         sd_process_time_train = sd(process_time_train), sd_wall_time_train = sd(wall_time_train))%>%
  # calculate mean for sampling time
  mutate(mean_process_time_sample = mean(process_time_sample), mean_wall_time_sample = mean(wall_time_sample),
         sd_process_time_sample = sd(process_time_sample), sd_wall_time_sample = sd(wall_time_sample))%>%
  select(- c(X, wall_time_sample, process_time_sample)) %>% distinct()

# rename gen_rf to FORGE
df$model[df$model == "gen_rf"] = "FORGE"
df$`model and processing unit` = paste0(df$model, " ", "(", df$processing_unit, ")")

#-------------------
# time plots
# 1. wall time
#-------------------


colcol = c("#0072B5FF",  "#6F99ADEF","#E18727FF", "#FEDC91FF","#20854EFF")

#--------------------
# training
#--------------------
plt_samplesize_train = ggplot(data = df , aes(x = n, y = mean_wall_time_train, color = `model and processing unit`)) + geom_line()+
  geom_ribbon(aes(ymin=mean_wall_time_train-sd_wall_time_train, ymax=mean_wall_time_train+sd_wall_time_train, fill = `model and processing unit`), 
              alpha=0.2, lty = "blank")+ 
  geom_point(aes(shape= `model and processing unit`) )+
  # scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans= 'log10')+
  ylab("Time (sec)")+
  theme_bw()+
  #theme(legend.position = 'none', legend.title = element_blank())+
  scale_discrete_manual(values = colcol ,aesthetics = c("colour", "fill"), 
                        breaks=c('CTGAN (CPU)', 'CTGAN (GPU)', 'TVAE (CPU)', 'TVAE (GPU)', 'FORGE (CPU)'),
                        name = "Method")+
  scale_shape_manual(values = c(3, 4, 16, 17,15), name = "Method",breaks=c('CTGAN (CPU)', 'CTGAN (GPU)', 'TVAE (CPU)', 'TVAE (GPU)', 'FORGE (CPU)'))+
  xlab("Sample size")+
  ggtitle("Training")

#--------------------
# sampling
#--------------------
plt_samplesize_sample = ggplot(data = df , aes(x = n, y = mean_wall_time_sample, color = `model and processing unit`)) + geom_line()+
  geom_ribbon(aes(ymin=mean_wall_time_sample-sd_wall_time_sample, ymax=mean_wall_time_sample+sd_wall_time_sample, fill = `model and processing unit`), 
              alpha=0.2, lty = "blank")+ 
  geom_point(aes(shape= `model and processing unit`) )+
  # scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans= 'log10')+
  ylab("")+
  theme_bw()+
  #theme(legend.key = element_rect(fill = "white"), legend.text = element_text(color = "white"), legend.title = element_text(color = "white")) +
  #guides(color = guide_legend(override.aes = list(color = NA, fill = NA)))+
  scale_discrete_manual(values = colcol ,aesthetics = c("colour", "fill"), 
                        breaks=c('CTGAN (CPU)', 'CTGAN (GPU)', 'TVAE (CPU)', 'TVAE (GPU)', 'FORGE (CPU)'),
                        name = "Method")+
  scale_shape_manual(values = c(3, 4, 16, 17,15), name = "Method",breaks=c('CTGAN (CPU)', 'CTGAN (GPU)', 'TVAE (CPU)', 'TVAE (GPU)', 'FORGE (CPU)'))+
  xlab("Sample size")+
  ggtitle("Sampling")

#-------------------
# time plots
# 2. process time
#-------------------
#--------------------
# training
#--------------------

plt_samplesize_process_time_train <- ggplot(data = df, aes(x = n, y = mean_process_time_train, color = `model and processing unit`)) + geom_line()+
  geom_ribbon(aes(ymin=mean_process_time_train-sd_process_time_train, ymax=mean_process_time_train+sd_process_time_train, fill = `model and processing unit`), 
              alpha=0.2, lty = "blank")+
  geom_point(aes(shape= `model and processing unit`) )+
  #scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans= 'log10')+
  ylab("Process time (sec)")+
  theme_bw()+
  #theme(legend.position = 'none', legend.title = element_blank())+
  scale_discrete_manual(values = colcol ,aesthetics = c("colour", "fill"), 
                        breaks=c('CTGAN (CPU)', 'CTGAN (GPU)', 'TVAE (CPU)', 'TVAE (GPU)', 'FORGE (CPU)'),
                        name = "Method")+
  scale_shape_manual(values = c(3, 4, 16, 17,15), name = "Method",breaks=c('CTGAN (CPU)', 'CTGAN (GPU)', 'TVAE (CPU)', 'TVAE (GPU)', 'FORGE (CPU)'))+
  xlab("Sample size")+
  ggtitle("Training")

#--------------------
# sampling
#--------------------

plt_samplesize_process_time_sample <- ggplot(data = df, aes(x = n, y = mean_process_time_sample, color = `model and processing unit`)) + geom_line()+
  geom_ribbon(aes(ymin=mean_process_time_sample-sd_process_time_sample, ymax=mean_process_time_sample+sd_process_time_sample, fill = `model and processing unit`), 
              alpha=0.2, lty = "blank")+
  geom_point(aes(shape= `model and processing unit`) )+
  #scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans= 'log10')+
  ylab("")+
  theme_bw()+
  #theme(legend.key = element_rect(fill = "white"), legend.text = element_text(color = "white"), legend.title = element_text(color = "white")) +
  #guides(color = guide_legend(override.aes = list(color = NA, fill = NA)))+
  scale_discrete_manual(values = colcol ,aesthetics = c("colour", "fill"), 
                        breaks=c('CTGAN (CPU)', 'CTGAN (GPU)', 'TVAE (CPU)', 'TVAE (GPU)', 'FORGE (CPU)'),
                        name = "Method")+
  scale_shape_manual(values = c(3, 4, 16, 17,15), name = "Method",breaks=c('CTGAN (CPU)', 'CTGAN (GPU)', 'TVAE (CPU)', 'TVAE (GPU)', 'FORGE (CPU)'))+
  xlab("Sample size")+
  ggtitle("Sampling")

#############################
# dimensionality 
#############################

files <- c(list.files(path = "./results_dimensionality",
                      pattern = ".*\\.csv$",
                      all.files = FALSE, full.names = TRUE, recursive = FALSE, ignore.case = TRUE,
                      include.dirs = FALSE, no.. = FALSE))
df <- data.frame()
for (i in 1:length(files)) {
  my_df <- read.csv(files[i])
  my_df$processing_unit <- ifelse(grepl("cpu", files[i]), "CPU", "GPU")
  df <- bind_rows(df, my_df)
} 

df =df %>% group_by(d, processing_unit, model) %>% 
  # calculate mean for training time
  mutate(mean_process_time_train = mean(process_time_train), mean_wall_time_train = mean(wall_time_train),
         sd_process_time_train = sd(process_time_train), sd_wall_time_train = sd(wall_time_train))%>%
  # calculate mean for sampling time
  mutate(mean_process_time_sample = mean(process_time_sample), mean_wall_time_sample = mean(wall_time_sample),
         sd_process_time_sample = sd(process_time_sample), sd_wall_time_sample = sd(wall_time_sample))%>%
  select(- c(X, wall_time_train, process_time_train)) %>% distinct()

df$model[df$model == "gen_rf"] = "FORGE"
df$`model and processing unit` = paste0(df$model, " ", "(", df$processing_unit, ")")

#-------------------
# time plots dimensionality
# 1. wall time
#-------------------

#-------------------
# training
#-------------------
colcol = c("#0072B5FF",  "#6F99ADEF","#E18727FF", "#FEDC91FF","#20854EFF")
plt_dimensionality_train = ggplot(data = df, aes(x = d, y = mean_wall_time_train, color = `model and processing unit`)) + geom_line()+
  geom_ribbon(aes(ymin=mean_wall_time_train-sd_wall_time_train, ymax=mean_wall_time_train+sd_wall_time_train, fill = `model and processing unit`), 
              alpha=0.2, lty = "blank")+
  geom_point(aes(shape= `model and processing unit`) )+
  scale_x_continuous(breaks = seq(2,14,2), minor_breaks = c() )+
  #scale_y_continuous(trans= 'log10')+
  #scale_color_npg()+
  ylab("Time (sec)")+
  theme_bw()+
  #theme(legend.position = 'none', legend.title = element_blank())+
  scale_discrete_manual(values = colcol ,aesthetics = c("colour", "fill"), 
                        breaks=c('CTGAN (CPU)', 'CTGAN (GPU)', 'TVAE (CPU)', 'TVAE (GPU)', 'FORGE (CPU)'),
                        name = "Method")+
  scale_shape_manual(values = c(3, 4, 16, 17,15), name = "Method",breaks=c('CTGAN (CPU)', 'CTGAN (GPU)', 'TVAE (CPU)', 'TVAE (GPU)', 'FORGE (CPU)'))+
  xlab("Dimensionality")+
  ggtitle("Training")

#-------------------
# sampling
#-------------------
colcol = c("#0072B5FF",  "#6F99ADEF","#E18727FF", "#FEDC91FF","#20854EFF")
plt_dimensionality_sample = ggplot(data = df, aes(x = d, y = mean_wall_time_sample, color = `model and processing unit`)) + geom_line()+
  geom_ribbon(aes(ymin=mean_wall_time_sample-sd_wall_time_sample, ymax=mean_wall_time_sample+sd_wall_time_sample, fill = `model and processing unit`), 
              alpha=0.2, lty = "blank")+
  geom_point(aes(shape= `model and processing unit`) )+
  scale_x_continuous(breaks = seq(2,14,2), minor_breaks = c() )+
  #scale_y_continuous(trans= 'log10')+
  #scale_color_npg()+
  ylab(" ")+
  theme_bw()+
  #theme(legend.position = 'none', legend.title = element_blank())+
  scale_discrete_manual(values = colcol ,aesthetics = c("colour", "fill"), 
                        breaks=c('CTGAN (CPU)', 'CTGAN (GPU)', 'TVAE (CPU)', 'TVAE (GPU)', 'FORGE (CPU)'),
                        name = "Method")+
  scale_shape_manual(values = c(3, 4, 16, 17,15), name = "Method",breaks=c('CTGAN (CPU)', 'CTGAN (GPU)', 'TVAE (CPU)', 'TVAE (GPU)', 'FORGE (CPU)'))+
  xlab("Dimensionality")+
  ggtitle("Sampling")

#-------------------
# time plots dimensionality
# 2. process time
#-------------------

#-------------------
# training
#-------------------
plt_dimensionality_process_time_train <- ggplot(data = df, aes(x = d, y = mean_process_time_train, color = `model and processing unit`)) + geom_line()+
  geom_ribbon(aes(ymin=mean_process_time_train-sd_process_time_train, ymax=mean_process_time_train+sd_process_time_train, fill = `model and processing unit`), 
              alpha=0.2, lty = "blank")+
  geom_point(aes(shape= `model and processing unit`) )+
  #scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans= 'log10')+
  ylab("Process time (sec)")+
  theme_bw()+
  #theme(legend.position = 'none', legend.title = element_blank())+
  scale_discrete_manual(values = colcol ,aesthetics = c("colour", "fill"), 
                        breaks=c('CTGAN (CPU)', 'CTGAN (GPU)', 'TVAE (CPU)', 'TVAE (GPU)', 'FORGE (CPU)'),
                        name = "Method")+
  scale_shape_manual(values = c(3, 4, 16, 17,15), name = "Method",breaks=c('CTGAN (CPU)', 'CTGAN (GPU)', 'TVAE (CPU)', 'TVAE (GPU)', 'FORGE (CPU)'))+
  xlab("Dimensionality")+
  ggtitle("Training")

#-------------------
# sampling
#-------------------
plt_dimensionality_process_time_sample <- ggplot(data = df, aes(x = d, y = mean_process_time_sample, color = `model and processing unit`)) + geom_line()+
  geom_ribbon(aes(ymin=mean_process_time_sample-sd_process_time_sample, ymax=mean_process_time_sample+sd_process_time_sample, fill = `model and processing unit`), 
              alpha=0.2, lty = "blank")+
  geom_point(aes(shape= `model and processing unit`) )+
  #scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans= 'log10')+
  ylab("")+
  theme_bw()+
  #theme(legend.position = 'none', legend.title = element_blank())+
  scale_discrete_manual(values = colcol ,aesthetics = c("colour", "fill"), 
                        breaks=c('CTGAN (CPU)', 'CTGAN (GPU)', 'TVAE (CPU)', 'TVAE (GPU)', 'FORGE (CPU)'),
                        name = "Method")+
  scale_shape_manual(values = c(3, 4, 16, 17,15), name = "Method",breaks=c('CTGAN (CPU)', 'CTGAN (GPU)', 'TVAE (CPU)', 'TVAE (GPU)', 'FORGE (CPU)'))+
  xlab("Dimensionality")+
  ggtitle("Sampling")


##############
# final plot
###############

#-----------------
# wall time
# ----------------
p1 <- plot_grid(plt_samplesize_train + theme(legend.position = "none"), 
          plt_samplesize_sample + theme(legend.position = "none"), 
          plt_dimensionality_train + theme(legend.position = "none"), 
          plt_dimensionality_sample + theme(legend.position = "none"), 
          ncol = 2, rel_widths = c(.53, .47) ,rel_heights = c(.5, .5), labels = "AUTO", label_x = c(.06, 0))
legend1 <- get_legend(
  plt_dimensionality_sample + theme(legend.position = "bottom")
)
plot_grid(p1, legend1, ncol = 1, rel_heights = c(1, .1))
#ggsave("time.pdf", width = 8, height = 4)
# bump in adult at dimensionality = 14 (~3 sec until d = 13, ~6 sec at d >= 14)
p2 <- plot_grid(plt_samplesize_process_time_train + theme(legend.position = "none"),
          plt_samplesize_process_time_sample + theme(legend.position = "none"), 
          plt_dimensionality_process_time_train + theme(legend.position = "none"), 
          plt_dimensionality_process_time_sample + theme(legend.position = "none"), 
          ncol = 2, rel_widths = c(.513, .487), rel_heights = c(.5, .5), labels = "AUTO", label_x = c(.06, 0))
legend2 <- get_legend(
  plt_dimensionality_process_time_sample + theme(legend.position = "bottom")
)
plot_grid(p2, legend2, ncol = 1, rel_heights = c(1, .1))
ggsave("process_time2.pdf", width = 10, height = 5.7)

