library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(xtable)
files <- c(list.files(path = "/Users/kristinblesch/Desktop/BIPS/BIPS_inhalt/generative_RF/generative_rf/benchmark/benchmark_results",
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
                                      f1_macro = mean(f1_macro), f1_micro = mean(f1_micro), mean_wall_time = mean(wall_time),
                                      mean_process_time = mean(process_time))%>%
  select(- c(classifier, X, f1_none, wall_time, process_time)) %>% distinct()


# make accuracy tables
df_acc = df[order(df$dataset), c("dataset", "model", "accuracy_score")] 
spread(df_acc, key = dataset, value = accuracy_score)
# f1 scores
df_acc = df[order(df$dataset), c("dataset", "model", "f1_1")] 
spread(df_acc, key = dataset, value = f1_1) 

spread(df, key = dataset, value = model) %>% select(adult, f1_1, f1_2, accuracy_score)


# full table
xtable_df = df %>% mutate(id = ifelse(model =="oracle", 1, ifelse(model=="gen_rf", 2, ifelse(model=="CTGAN", 3, 4))))%>%
  select(-c(mean_process_time)) %>% group_by(dataset) %>% arrange(dataset,id) %>% relocate(f1_1,f1_2, .after = accuracy_score) %>% 
  select(-id)

# table to latex
print(xtable(xtable_df), include.rownames = F)

# merge F1 scores -- always use lowest score for F1 in binary classification, F1 micro for multiclass classification
xtable_f1 = df %>% mutate(id = ifelse(model =="oracle", 1, ifelse(model=="gen_rf", 2, ifelse(model=="CTGAN", 3, 4))))%>%
  select(-c(mean_process_time)) %>% group_by(dataset) %>% arrange(dataset,id) 
xtable_f1$F1 = c()
xtable_f1[xtable_f1$dataset == "adult","F1"] = xtable_f1[xtable_f1$dataset == "adult", "f1_1"]
xtable_f1[xtable_f1$dataset == "census","F1"] = xtable_f1[xtable_f1$dataset == "census", "f1_1"]
xtable_f1[xtable_f1$dataset == "covtype","F1"] = xtable_f1[xtable_f1$dataset == "covtype", "f1_macro"]
xtable_f1[xtable_f1$dataset == "credit","F1"] = xtable_f1[xtable_f1$dataset == "credit", "f1_2"]
xtable_f1[xtable_f1$dataset == "intrusion","F1"] = xtable_f1[xtable_f1$dataset == "intrusion", "f1_macro"]
xtable_f1[xtable_f1$dataset == "mnist12","F1"] = xtable_f1[xtable_f1$dataset == "mnist12", "f1_macro"]
xtable_f1[xtable_f1$dataset == "mnist28","F1"] = xtable_f1[xtable_f1$dataset == "mnist28", "f1_macro"]

xtable_f1$characteristics = c()
xtable_f1[xtable_f1$dataset == "adult" & xtable_f1$model == "gen_rf" ,"characteristics"] = "n = 32,561"
xtable_f1[xtable_f1$dataset == "adult" & xtable_f1$model == "CTGAN" ,"characteristics"] = "p = 14"
xtable_f1[xtable_f1$dataset == "adult" & xtable_f1$model == "TVAE" ,"characteristics"] = "target = binary"
xtable_f1[xtable_f1$dataset == "census" & xtable_f1$model == "gen_rf" ,"characteristics"] = "n = 298,006"
xtable_f1[xtable_f1$dataset == "census" & xtable_f1$model == "CTGAN" ,"characteristics"] = "p = 40"
xtable_f1[xtable_f1$dataset == "census" & xtable_f1$model == "TVAE" ,"characteristics"] = "target = binary"
xtable_f1[xtable_f1$dataset == "covtype" & xtable_f1$model == "gen_rf" ,"characteristics"] = "n = 581,012"
xtable_f1[xtable_f1$dataset == "covtype" & xtable_f1$model == "CTGAN" ,"characteristics"] = "p = 54"
xtable_f1[xtable_f1$dataset == "covtype" & xtable_f1$model == "TVAE" ,"characteristics"] = "target = multiclass"
xtable_f1[xtable_f1$dataset == "credit" & xtable_f1$model == "gen_rf" ,"characteristics"] = "n = 284,807"
xtable_f1[xtable_f1$dataset == "credit" & xtable_f1$model == "CTGAN" ,"characteristics"] = "p = 30"
xtable_f1[xtable_f1$dataset == "credit" & xtable_f1$model == "TVAE" ,"characteristics"] = "target = binary"
xtable_f1[xtable_f1$dataset == "intrusion" & xtable_f1$model == "gen_rf" ,"characteristics"] = "n = 494,021"
xtable_f1[xtable_f1$dataset == "intrusion" & xtable_f1$model == "CTGAN" ,"characteristics"] = "p = 40"
xtable_f1[xtable_f1$dataset == "intrusion" & xtable_f1$model == "TVAE" ,"characteristics"] = "target = multiclass"
xtable_f1[xtable_f1$dataset == "mnist12" & xtable_f1$model == "gen_rf" ,"characteristics"] = "n = 70,000"
xtable_f1[xtable_f1$dataset == "mnist12" & xtable_f1$model == "CTGAN" ,"characteristics"] = "p = 144"
xtable_f1[xtable_f1$dataset == "mnist12" & xtable_f1$model == "TVAE" ,"characteristics"] = "target = multiclass"
xtable_f1[xtable_f1$dataset == "mnist28" & xtable_f1$model == "gen_rf" ,"characteristics"] = "n = 70,000"
xtable_f1[xtable_f1$dataset == "mnist28" & xtable_f1$model == "CTGAN" ,"characteristics"] = "p = 784"
xtable_f1[xtable_f1$dataset == "mnist28" & xtable_f1$model == "TVAE" ,"characteristics"] = "target = multiclass"

xtable_f1$model[xtable_f1$model == "gen_rf"] = "FORGE"
xtable_f1 = xtable_f1 %>%relocate(characteristics, .after = dataset) %>% relocate(F1, .after = accuracy_score) %>% 
  select(-c(id, f1_1, f1_2, f1_micro, f1_macro)) %>% rename(accuracy = accuracy_score) %>% rename(time = mean_wall_time)
# table to latex
print(xtable(xtable_f1), include.rownames = F)

