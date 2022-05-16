library(dplyr)
library(ggplot2)
library(reshape2)
library(ggsci)
library(cowplot)
################################
# subsample size
################################

files <- c(list.files(path = "./subsample_benchmark_results",
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
  mutate(mean_process_time = mean(process_time), mean_wall_time = mean(wall_time),
                                      sd_process_time = sd(process_time), sd_wall_time = sd(wall_time))%>%
  select(- c(X, wall_time, process_time)) %>% distinct()

df$model[df$model == "gen_rf"] = "FORGE"
df$`model and processing unit` = paste(df$model, df$processing_unit, sep = "_")

#-------------------
# time plots
#-------------------

# wall time
colcol = c("#00A087FF",  "#91D1C2FF","#E64B35FF", "#F39B7FFF","#3C5488FF")
plt_samplesize = ggplot(data = df %>% filter(model != "oracle") , aes(x = dataset, y = mean_wall_time, color = `model and processing unit`)) + geom_line()+
  geom_ribbon(aes(ymin=mean_wall_time-sd_wall_time, ymax=mean_wall_time+sd_wall_time, fill = `model and processing unit`), 
              alpha=0.2, lty = "blank")+ 
  geom_point()+
 # scale_x_continuous(trans='log10')+
 #scale_y_continuous(trans= 'log10')+
  ylab("wall time (sec)")+
  theme_bw()+
  theme(legend.position = 'none', legend.title = element_blank())+
  scale_discrete_manual(values = colcol ,aesthetics = c("colour", "fill"), breaks=c('CTGAN_CPU', 'CTGAN_GPU', 'TVAE_CPU', 'TVAE_GPU', 'FORGE_CPU'))+
  xlab("sample size")

# process time
plt_samplesize_process_time <- ggplot(data = df %>% filter(model != "oracle") , aes(x = dataset, y = mean_process_time, color = `model and processing unit`)) + geom_line()+
  geom_ribbon(aes(ymin=mean_process_time-sd_process_time, ymax=mean_process_time+sd_process_time, fill = `model and processing unit`), 
              alpha=0.2, lty = "blank")+
  geom_point()+
  #scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans= 'log10')+
  ylab("wall time (sec)")+
  theme_bw()+
  theme(legend.position = 'none')+
  scale_discrete_manual(values = colcol ,aesthetics = c("colour", "fill"), breaks=c('CTGAN_CPU', 'CTGAN_GPU', 'TVAE_CPU', 'TVAE_GPU', 'FORGE_CPU'))+
  xlab("sample size")

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
  mutate(mean_process_time = mean(process_time), mean_wall_time = mean(wall_time),
         sd_process_time = sd(process_time), sd_wall_time = sd(wall_time))%>%
  select(- c(X, wall_time, process_time)) %>% distinct()

df$model[df$model == "gen_rf"] = "FORGE"
df$`model and processing unit` = paste(df$model, df$processing_unit, sep = "_")

#-------------------
# time plots
#-------------------

# wall time
colcol = c("#00A087FF",  "#91D1C2FF","#E64B35FF", "#F39B7FFF","#3C5488FF")
plt_dimensionality = ggplot(data = df %>% filter(model != "oracle") , aes(x = dataset, y = mean_wall_time, color = `model and processing unit`)) + geom_line()+
  geom_ribbon(aes(ymin=mean_wall_time-sd_wall_time, ymax=mean_wall_time+sd_wall_time, fill = `model and processing unit`), 
              alpha=0.2, lty = "blank")+
  geom_point()+
  scale_x_continuous(breaks = seq(2,14,2), minor_breaks = c() )+
  #scale_y_continuous(trans= 'log10')+
  #scale_color_npg()+
  ylab("wall time (sec)")+
  theme_bw()+
  theme(legend.position = 'right')+
  scale_discrete_manual(values = colcol ,aesthetics = c("colour", "fill"), 
                        breaks=c('CTGAN_CPU', 'CTGAN_GPU', 'TVAE_CPU', 'TVAE_GPU', 'FORGE_CPU'),
                        name = "Synthesizer")+
  xlab("dimensionality")

# process time
plt_dimensionality_process_time <- ggplot(data = df %>% filter(model != "oracle") , aes(x = dataset, y = mean_process_time, color = `model and processing unit`)) + geom_line()+
  geom_ribbon(aes(ymin=mean_process_time-sd_process_time, ymax=mean_process_time+sd_process_time, fill = `model and processing unit`), 
              alpha=0.2, lty = "blank")+
  geom_point()+
  #scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans= 'log10')+
  ylab("wall time (sec)")+
  theme_bw()+
  theme(legend.position = 'right')+
  scale_discrete_manual(values = colcol ,
                        name = "Synthesizer",
                        aesthetics = c("colour", "fill"), breaks=c('CTGAN_CPU', 'CTGAN_GPU', 'TVAE_CPU', 'TVAE_GPU', 'FORGE_CPU'))+
  xlab("dimensionality")

##############
# final plot
###############
plot_grid(plt_samplesize, plt_dimensionality, ncol = 2, rel_widths = c(.41, .59),rel_heights = c(.5, .5), labels = "AUTO", label_x = c(.06, 0))
ggsave("time.pdf", width = 8, height = 3)

plot_grid(plt_samplesize_process_time, plt_dimensionality_process_time, ncol = 2, rel_widths = c(.41, .59),rel_heights = c(.5, .5), labels = "AUTO", label_x = c(.06, 0))
ggsave("process_time.pdf", width = 8, height = 3)
