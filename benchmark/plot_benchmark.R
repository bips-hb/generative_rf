library(dplyr)
library(ggplot2)
library(reshape2)
files <- c(list.files(path = "/Users/kristinblesch/Desktop/BIPS/BIPS_inhalt/generative_RF/generative_rf/benchmark/reproducibility_2",
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

# individual plots
# binary classification: adult, census, credit
df_binary = df %>% filter(dataset %in% c("adult", "census", "credit")) 
colcol = c("blue", "seagreen", "orange","purple")
ggplot(data = df_binary %>% filter(model != "Identity")) + 
  geom_boxplot(position = "dodge",  aes(x = model, y = accuracy_score, col = model))+ 
  facet_grid( rows =NULL , cols = vars(dataset))+
  scale_colour_manual(values = colcol ,aesthetics = c("colour", "fill"))+
  geom_hline(data = df_binary %>% filter(model == "Identity"), aes(yintercept = accuracy_score), col = 'red', linetype = 'dotted')


# individual plots
# multiclass classification: covtype, intrusion, mnist12, mnist28
df_binary = df %>% filter(dataset %in% c("covtype", "intrusion", "mnist12", "mnist28")) 
colcol = c("blue", "seagreen", "orange","purple")
ggplot(data = df_binary %>% filter(model != "Identity")) + 
  geom_boxplot(position = "dodge",  aes(x = model, y = f1_macro, col = model))+ 
  facet_grid( rows =NULL , cols = vars(dataset))+
  scale_colour_manual(values = colcol ,aesthetics = c("colour", "fill"))+
  geom_hline(data = df_binary %>% filter(model == "Identity"), aes(yintercept = f1_macro), col = 'red', linetype = 'dotted')

# individual plots
# regression: news
df_binary = df %>% filter(dataset %in% c("news")) 
colcol = c("blue", "seagreen", "orange","purple")
ggplot(data = df_binary %>% filter(model != "Identity")) + 
  geom_boxplot(position = "dodge",  aes(x = model, y = r2_score, col = model))+ 
  facet_grid( rows =NULL , cols = vars(dataset))+
  scale_colour_manual(values = colcol ,aesthetics = c("colour", "fill"))+
  geom_hline(data = df_binary %>% filter(model == "Identity"), aes(yintercept = r2_score), col = 'red', linetype = 'dotted')

####### other

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
