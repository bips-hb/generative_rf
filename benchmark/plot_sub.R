library(dplyr)
library(ggplot2)
library(reshape2)
files <- c(list.files(path = "/Users/kristinblesch/Desktop/BIPS/BIPS_inhalt/generative_RF/generative_rf/benchmark/sub_results",
                      pattern = ".*\\.csv$",
                      all.files = FALSE, full.names = TRUE, recursive = FALSE, ignore.case = TRUE,
                      include.dirs = FALSE, no.. = FALSE))
df <- data.frame()
for (i in 1:length(files)) {
  my_df <- read.csv(files[i])
  my_df$processing_unit <- ifelse(grepl("cpu", files[i]), "CPU", "GPU")
  df <- bind_rows(df, my_df)
} 

helper_fun <- function(f1_char, first = T){
  list_f1 = gsub('[', "",f1_char , fixed = T) %>% gsub(']', "", ., fixed = T) %>% 
    strsplit(., split = " ") %>% lapply(as.numeric) %>% lapply(., function(x) {na.omit(x)[1:2]})
  if(first){return(unlist(lapply(list_f1, function(x) x[1])))}
  else{return(unlist(lapply(list_f1, function(x) x[2])))}
}

df =df %>% mutate(f1_1 = helper_fun(f1_none, first = T)) %>% 
  mutate(f1_2 = helper_fun(f1_none, first = F)) %>%
  group_by(dataset, processing_unit, model) %>% mutate(mean_f1_1 = mean(f1_1), mean_f1_2 = mean(f1_2), mean_accuracy_score = mean(accuracy_score),
                                                       sd_f1_1 = sd(f1_1), sd_f1_2 = sd(f1_2), sd_accuracy_score = sd(accuracy_score),
                                      mean_process_time = mean(process_time), mean_wall_time = mean(wall_time),
                                      sd_process_time = sd(process_time), sd_wall_time = sd(wall_time))%>%
  select(- c(classifier, X, f1_none, wall_time, process_time, accuracy_score, f1_1, f1_2)) %>% distinct()

df$model[df$model == "gen_rf"] = "FORGE"
df$`model and processing unit` = paste(df$model, df$processing_unit, sep = "_")

#-------------------
# time plots
#-------------------
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

ggplot(data = df %>% filter(model != "oracle") , aes(x = dataset, y = mean_process_time, color = `model and processing unit`)) + geom_line()+
  geom_ribbon(aes(ymin=mean_process_time-sd_process_time, ymax=mean_process_time+sd_process_time, fill = `model and processing unit`), 
              alpha=0.2, lty = "blank")+
  ggtitle("process time mean and sd, stratified adult data (5 rep.)")+
 # scale_x_continuous(trans='log10')+ theme_bw()+
#  scale_y_continuous(trans= 'log10')+
  ylab("process time")+
  xlab("n")

#---------------------
# performance plots
#---------------------

ggplot(data = df , aes(x = dataset, y = mean_accuracy_score, color = `model and processing unit`)) + geom_line()+
  geom_ribbon(aes(ymin=mean_accuracy_score-sd_accuracy_score, ymax=mean_accuracy_score+sd_accuracy_score, fill = `model and processing unit`), 
              alpha=0.2, lty = "blank")+
  ggtitle("accuracy_score mean and sd, stratified adult data (5 rep.)")+
  scale_x_continuous(trans='log10')+ theme_bw()+
  #scale_y_continuous(trans= 'log10')+
  ylab("mean accuracy_score")+
  xlab("n")

ggplot(data = df , aes(x = dataset, y = mean_f1_1, color = `model and processing unit`)) + geom_line()+
  geom_ribbon(aes(ymin=mean_f1_1-sd_f1_1, ymax=mean_f1_1+sd_f1_1, fill = `model and processing unit`), 
              alpha=0.2, lty = "blank")+
  ggtitle("f1_1 mean and sd, stratified adult data (5 rep.)")+
  scale_x_continuous(trans='log10')+ theme_bw()+
  #scale_y_continuous(trans= 'log10')+
  ylab("mean accuracy_score")+
  xlab("n")

ggplot(data = df , aes(x = dataset, y = mean_f1_2, color = `model and processing unit`)) + geom_line()+
  geom_ribbon(aes(ymin=mean_f1_2-sd_f1_2, ymax=mean_f1_2+sd_f1_2, fill = `model and processing unit`), 
              alpha=0.2, lty = "blank")+
  ggtitle("f1_2 mean and sd, stratified adult data (5 rep.)")+
  scale_x_continuous(trans='log10')+ theme_bw()+
  #scale_y_continuous(trans= 'log10')+
  ylab("mean accuracy_score")+
  xlab("n")

