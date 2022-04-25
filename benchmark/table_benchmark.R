library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(xtable)
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
                                      f1_macro = mean(f1_macro), f1_micro = mean(f1_micro), r2_score = mean(r2_score))%>%
  select(- c(classifier, X, f1_none)) %>% distinct()


# make accuracy tables
df_acc = df[order(df$dataset), c("dataset", "model", "accuracy_score")] 
spread(df_acc, key = dataset, value = accuracy_score)
# f1 scores
df_acc = df[order(df$dataset), c("dataset", "model", "f1_1")] 
spread(df_acc, key = dataset, value = f1_1) 

spread(df, key = dataset, value = model) %>% select(adult, f1_1, f1_2, accuracy_score)


# full table
xtable_df = df %>% mutate(id = ifelse(model =="Identity", 1, ifelse(model=="gen_rf", 2, ifelse(model=="CTGAN", 3, 4))))%>%
  select(-c(process_time, wall_time)) %>% group_by(dataset) %>% arrange(dataset,id) %>% relocate(f1_1,f1_2, .after = accuracy_score) %>% 
  select(-id)

# table to latex
# print lowest value in bold font
print(xtable(xtable_df), include.rownames = F)
