library(dplyr)
library(ggplot2)
library(reshape2)
library(ggsci)
library(cowplot)
################################
# subsample size
################################

files <- c(list.files(path = ".",
                      pattern = ".*\\.csv$",
                      all.files = FALSE, full.names = TRUE, recursive = FALSE, ignore.case = TRUE,
                      include.dirs = FALSE, no.. = FALSE))
df <- data.frame()
for (i in 1:length(files)) {
  my_df <- read.csv(files[i])
  my_df$processing_unit <- ifelse(grepl("cpu", files[i]), "CPU", "GPU")
  df <- bind_rows(df, my_df)
} 

df =df %>% group_by(dataset, processing_unit, model) %>% 
  mutate(mean_process_time_train = mean(process_time_train), mean_wall_time_train = mean(wall_time_train),
         sd_process_time_train = sd(process_time_train), sd_wall_time_train = sd(wall_time_train))%>%
  select(- c(X, wall_time_train, process_time_train)) %>% distinct()

df$model[df$model == "gen_rf"] = "FORGE"
df$`model and processing unit` = paste0(df$model, " ", "(", df$processing_unit, ")")

#-------------------
# time plots
#-------------------

# wall time
colcol = c("#0072B5FF",  "#6F99ADEF","#E18727FF", "#FEDC91FF","#20854EFF")
plt_samplesize = ggplot(data = df %>% filter(model != "oracle") , aes(x = dataset, y = mean_wall_time_train, color = `model and processing unit`)) + geom_line()+
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

# process time
plt_samplesize_process_time_train <- ggplot(data = df %>% filter(model != "oracle") , aes(x = dataset, y = mean_process_time_train, color = `model and processing unit`)) + geom_line()+
  geom_ribbon(aes(ymin=mean_process_time_train-sd_process_time_train, ymax=mean_process_time_train+sd_process_time_train, fill = `model and processing unit`), 
              alpha=0.2, lty = "blank")+
  geom_point(aes(shape= `model and processing unit`) )+
  #scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans= 'log10')+
  ylab("Process time (sec)")+
  theme_bw()+
 # theme(legend.position = 'none')+
  scale_discrete_manual(values = colcol ,aesthetics = c("colour", "fill"), 
                        breaks=c('CTGAN (CPU)', 'CTGAN (GPU)', 'TVAE (CPU)', 'TVAE (GPU)', 'FORGE (CPU)'),
                        name = "Method")+
  scale_shape_manual(values = c(3, 4, 16, 17,15), name = "Method",breaks=c('CTGAN (CPU)', 'CTGAN (GPU)', 'TVAE (CPU)', 'TVAE (GPU)', 'FORGE (CPU)'))+
  xlab("Sample size")+
  ggtitle("Training")

#############################
# dimensionality 
#############################

files <- c(list.files(path = "./dimensionality_benchmark_results",
                      pattern = ".*\\.csv$",
                      all.files = FALSE, full.names = TRUE, recursive = FALSE, ignore.case = TRUE,
                      include.dirs = FALSE, no.. = FALSE))
df <- data.frame()
for (i in 1:length(files)) {
  my_df <- read.csv(files[i])
  my_df$processing_unit <- ifelse(grepl("cpu", files[i]), "CPU", "GPU")
  df <- bind_rows(df, my_df)
} 

df =df %>% group_by(dataset, processing_unit, model) %>% 
  mutate(mean_process_time_train = mean(process_time_train), mean_wall_time_train = mean(wall_time_train),
         sd_process_time_train_train = sd(process_time_train), sd_wall_time_train = sd(wall_time_train))%>%
  select(- c(X, wall_time_train, process_time_train)) %>% distinct()

df$model[df$model == "gen_rf"] = "FORGE"
df$`model and processing unit` = paste0(df$model, " ", "(", df$processing_unit, ")")

#-------------------
# time plots
#-------------------

# wall time
colcol = c("#0072B5FF",  "#6F99ADEF","#E18727FF", "#FEDC91FF","#20854EFF")
plt_dimensionality = ggplot(data = df %>% filter(model != "oracle") , aes(x = dataset, y = mean_wall_time_train, color = `model and processing unit`)) + geom_line()+
  geom_ribbon(aes(ymin=mean_wall_time_train-sd_wall_time_train, ymax=mean_wall_time_train+sd_wall_time_train, fill = `model and processing unit`), 
              alpha=0.2, lty = "blank")+
  geom_point(aes(shape= `model and processing unit`) )+
  scale_x_continuous(breaks = seq(2,14,2), minor_breaks = c() )+
  #scale_y_continuous(trans= 'log10')+
  #scale_color_npg()+
  ylab(" ")+
  theme_bw()+
  theme(legend.position = 'right')+
  scale_discrete_manual(values = colcol ,aesthetics = c("colour", "fill"), 
                        breaks=c('CTGAN (CPU)', 'CTGAN (GPU)', 'TVAE (CPU)', 'TVAE (GPU)', 'FORGE (CPU)'),
                        name = "Method")+
  scale_shape_manual(values = c(3, 4, 16, 17,15), name = "Method",breaks=c('CTGAN (CPU)', 'CTGAN (GPU)', 'TVAE (CPU)', 'TVAE (GPU)', 'FORGE (CPU)'))+
  xlab("Dimensionality")

# process time
plt_dimensionality_process_time_train <- ggplot(data = df %>% filter(model != "oracle") , aes(x = dataset, y = mean_process_time_train, color = `model and processing unit`)) + geom_line()+
  geom_ribbon(aes(ymin=mean_process_time_train-sd_process_time_train, ymax=mean_process_time_train+sd_process_time_train, fill = `model and processing unit`), 
              alpha=0.2, lty = "blank")+
  geom_point(aes(shape= `model and processing unit`) )+
  #scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans= 'log10')+
  ylab("")+
  theme_bw()+
  theme(legend.position = 'right')+
  scale_discrete_manual(values = colcol ,aesthetics = c("colour", "fill"), 
                        breaks=c('CTGAN (CPU)', 'CTGAN (GPU)', 'TVAE (CPU)', 'TVAE (GPU)', 'FORGE (CPU)'),
                        name = "Method")+
  scale_shape_manual(values = c(3, 4, 16, 17,15), name = "Method",breaks=c('CTGAN (CPU)', 'CTGAN (GPU)', 'TVAE (CPU)', 'TVAE (GPU)', 'FORGE (CPU)'))+
  xlab("Dimensionality")

##############
# final plot
###############
plot_grid(plt_samplesize, plt_dimensionality, ncol = 2, rel_widths = c(.41, .59),rel_heights = c(.5, .5), labels = "AUTO", label_x = c(.06, 0))
#ggsave("time.pdf", width = 8, height = 3)

plot_grid(plt_samplesize_process_time_train, plt_dimensionality_process_time_train, ncol = 2, rel_widths = c(.41, .59),rel_heights = c(.5, .5), labels = "AUTO", label_x = c(.06, 0))
#ggsave("process_time.pdf", width = 8, height = 3)