library(dplyr)
library(ggplot2)
library(reshape2)
files <- c(list.files(path = "/Users/kristinblesch/Desktop/BIPS/BIPS_inhalt/generative_RF/generative_rf/benchmark/subsample_reproducibility_2",
                      pattern = ".*\\.csv$",
                      all.files = FALSE, full.names = TRUE, recursive = FALSE, ignore.case = TRUE,
                      include.dirs = FALSE, no.. = FALSE))
df <- data.frame()
for (i in 1:length(files)) {
  df <- bind_rows(df, read.csv(files[i]))
} 
helper_fun <- function(f1_char, first = T){
  list_f1 = gsub('[', "",f1_char , fixed = T) %>% gsub(']', "", ., fixed = T) %>% 
    strsplit(., split = " ") %>% lapply(as.numeric) %>% lapply(., function(x) {na.omit(x)[1:2]})
  if(first){return(unlist(lapply(list_f1, function(x) x[1])))}
  else{return(unlist(lapply(list_f1, function(x) x[2])))}
}

df =df %>% mutate(f1_1 = helper_fun(f1_none, first = T)) %>% 
  mutate(f1_2 = helper_fun(f1_none, first = F)) %>%
  group_by(dataset, model) %>% mutate(f1_1_mean = mean(f1_1), f1_2_mean = mean(f1_2), accuracy_score_mean = mean(accuracy_score),
                                      f1_1_sd = sd(f1_1), f1_2_sd = sd(f1_2), accuracy_score_sd = sd(accuracy_score))%>%
  select(- c(classifier, X, f1_none)) %>% distinct()


melted_df <-  melt(df, id.vars = c("model", "dataset", "wall_time", "process_time"),value.name = "value", variable.name = "score" )
plot_by_hand <-  ggplot(data = melted_df)+
  facet_grid(rows = vars(score), cols = vars(dataset))+
  geom_point(aes(x = model, y = value, col = model))
plot_by_hand

melted_df_time <- df %>% select(-c(accuracy_score_mean, f1_1_mean, f1_2_mean, accuracy_score_sd, f1_1_sd, f1_2_sd)) %>% melt(., id.vars = c("model", "dataset"), variable.name = "time", value.name = "value")
plot_time <- ggplot(data = melted_df_time)+
  facet_grid(rows = vars(time), cols = vars(dataset))+
  geom_point(aes(x = model, y = value, col = model))
plot_time

ggplot(data = df, aes(x = dataset, y = f1_1_mean, color = model)) + geom_line()+
  geom_ribbon(aes(ymin=f1_1_mean-f1_1_sd, ymax=f1_1_mean+f1_1_sd, fill = model), 
             alpha=0.2, lty = "blank")+
  theme_bw()+ geom_point()+
  scale_x_continuous(trans='log10')+
  ggtitle("Mean F1 score and standard deviation for stratified census data")

ggplot(data = df, aes(x = dataset, y = f1_2_mean, color = model)) + geom_line()+
  geom_ribbon(aes(ymin=f1_2_mean-f1_2_sd, ymax=f1_2_mean+f1_2_sd, fill = model), 
              alpha=0.2, lty = "blank")+
  ggtitle("Mean F1 score and standard deviation for stratified census data")

df = df %>% mutate(wall_time_mean = mean(wall_time), wall_time_sd = sd(wall_time)) %>% filter(model != "Identity")
ggplot(data = df, aes(x = dataset, y = wall_time_mean, color = model)) + geom_line()+
  geom_ribbon(aes(ymin=wall_time_mean-wall_time_sd, ymax=wall_time_mean+wall_time_sd, fill = model), 
              alpha=0.2, lty = "blank")+
  ggtitle("Wall time mean and sd, stratified census data (5 rep.)")+
  scale_x_continuous(trans='log10')+ theme_bw()+
 #scale_y_continuous(trans= 'log10')+
  ylab("Mean Wall Time")

