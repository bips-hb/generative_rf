library(dplyr)
library(ggplot2)
library(reshape2)
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
colcol = c("cornflowerblue",  "cadetblue3","coral3", "coral","seagreen")
ggplot(data = df %>% filter(model != "oracle") , aes(x = dataset, y = mean_wall_time, color = `model and processing unit`)) + geom_line()+
  geom_ribbon(aes(ymin=mean_wall_time-sd_wall_time, ymax=mean_wall_time+sd_wall_time, fill = `model and processing unit`), 
              alpha=0.2, lty = "blank")+
  ggtitle("Wall time for data synthesis mean and sd, stratified adult data (5 rep.)")+ geom_point()+
  #scale_x_continuous(trans='log10')+
 #scale_y_continuous(trans= 'log10')+
  ylab("wall time (sec)")+
  theme_bw()+
  theme(legend.position = 'bottom', legend.title = element_blank())+
  scale_discrete_manual(values = colcol ,aesthetics = c("colour", "fill"), breaks=c('CTGAN_CPU', 'CTGAN_GPU', 'TVAE_CPU', 'TVAE_GPU', 'FORGE_CPU'))+
  xlab("sample size")

# process time
colcol = c("cornflowerblue",  "cadetblue3","coral3", "coral","seagreen")
ggplot(data = df %>% filter(model != "oracle") , aes(x = dataset, y = mean_process_time, color = `model and processing unit`)) + geom_line()+
  geom_ribbon(aes(ymin=mean_process_time-sd_process_time, ymax=mean_process_time+sd_process_time, fill = `model and processing unit`), 
              alpha=0.2, lty = "blank")+
  ggtitle("Process time for data synthesis mean and sd, stratified adult data (5 rep.)")+ geom_point()+
  #scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans= 'log10')+
  ylab("wall time (sec)")+
  theme_bw()+
  theme(legend.position = 'bottom', legend.title = element_blank())+
  scale_discrete_manual(values = colcol ,aesthetics = c("colour", "fill"), breaks=c('CTGAN_CPU', 'CTGAN_GPU', 'TVAE_CPU', 'TVAE_GPU', 'FORGE_CPU'))+
  xlab("sample size")
