library(dplyr)
library(ggplot2)
library(reshape2)
files <- c(list.files(path = "/Users/kristinblesch/Desktop/BIPS/BIPS_inhalt/generative_RF/generative_rf/benchmark/reproducibility",
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
  group_by(dataset, model) %>% mutate(f1_1 = mean(f1_1), f1_2 = mean(f1_2), accuracy_score = mean(accuracy_score),
                                      f1_macro = mean(f1_macro), f1_micro = mean(f1_micro))%>%
  select(- c(classifier, X, f1_none)) %>% distinct()


melted_df <-  melt(df, id.vars = c("model", "dataset", "wall_time", "process_time"),value.name = "value", variable.name = "score" )
plot_by_hand <-  ggplot(data = melted_df)+
  facet_grid(rows = vars(score), cols = vars(dataset))+
  geom_point(aes(x = model, y = value, col = model))
plot_by_hand

melted_df_time <- df %>% select(-c(accuracy_score, f1_1, f1_2, f1_micro, f1_macro)) %>% melt(., id.vars = c("model", "dataset"), variable.name = "time", value.name = "value")
plot_time <- ggplot(data = melted_df_time)+
  facet_grid(rows = vars(time), cols = vars(dataset))+
  geom_point(aes(x = model, y = value, col = model))
plot_time
