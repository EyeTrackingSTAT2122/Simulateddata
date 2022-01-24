library(tidyverse)
library(dplyr)
library(FactoMineR)
library(nnet)
library(Rcmdr)

data_fixation <- read_csv("data/data_finale2.csv")
data2 <- read_csv("data/Real_eye/resume_final.csv")
data3 <- read_csv("data/Real_eye/resume_sauvetage.csv")
classes <- rbind(data2, data3)
classes$tester_name[44] <- "34, Female, 23"

classe_ind<- classes %>% 
  dplyr::select(tester_id,tester_name, starts_with("D'après")) %>% 
  separate(tester_name, c('id', 'gender', 'age')) %>%
  mutate(id = as.factor(id)) %>% 
  pivot_longer(cols = starts_with("D'après"), names_to = "num_stimulus", values_to = "classe") %>%
  mutate(tester_id = as.factor(tester_id)) %>% 
  mutate(num_stimulus = as.factor(num_stimulus)) %>% 
  mutate(num_stimulus = fct_recode(num_stimulus,
                                   "156" = "D'après vous, le plateau est-il équilibré ?  - 156.jpg",
                                   "672"="D'après vous, le plateau est-il équilibré ?  - 672.jpg",
                                   "756"="D'après vous, le plateau est-il équilibré ?  - 756.jpg",
                                   "421"="D'après vous, le plateau est-il équilibré ?  - 421.jpg",
                                   "489"="D'après vous, le plateau est-il équilibré ?  - 489.jpg",
                                   "327"="D'après vous, le plateau est-il équilbré ?  - 327.jpg",
                                   "238"="D'après vous, le plateau est-il équilibré ? - 238.jpg",
                                   "352"="D'après vous, le plateau est-il équilibré ? - 352.jpg",
                                   "980"="D'après vous, le plateau est-il équilibré ?  - 980.jpg" 
  )) %>%
  mutate(classe = as.factor(classe)) %>%
  mutate (classe = fct_recode(classe,
                              "PresqueEquilibre" = "Il est presque équilibré",
                              "Equilibre" = "Il est équilibré",
                              "PasEquilibre" = "Il n'est pas équilibré")) %>%
  dplyr::select (id, classe, num_stimulus) %>% 
  pivot_wider(names_from = id, values_from = classe) %>% 
  data.frame()


classes_stimuli<- classes %>% 
  dplyr::select(tester_id,tester_name, starts_with("D'après")) %>% 
  separate(tester_name, c('id', 'gender', 'age')) %>%
  mutate(id = as.factor(id)) %>% 
  pivot_longer(cols = starts_with("D'après"), names_to = "num_stimulus", values_to = "classe") %>%
  mutate(tester_id = as.factor(tester_id)) %>% 
  mutate(num_stimulus = as.factor(num_stimulus)) %>% 
  mutate(num_stimulus = fct_recode(num_stimulus,
                                   "156" = "D'après vous, le plateau est-il équilibré ?  - 156.jpg",
                                   "672"="D'après vous, le plateau est-il équilibré ?  - 672.jpg",
                                   "756"="D'après vous, le plateau est-il équilibré ?  - 756.jpg",
                                   "421"="D'après vous, le plateau est-il équilibré ?  - 421.jpg",
                                   "489"="D'après vous, le plateau est-il équilibré ?  - 489.jpg",
                                   "327"="D'après vous, le plateau est-il équilbré ?  - 327.jpg",
                                   "238"="D'après vous, le plateau est-il équilibré ? - 238.jpg",
                                   "352"="D'après vous, le plateau est-il équilibré ? - 352.jpg",
                                   "980"="D'après vous, le plateau est-il équilibré ?  - 980.jpg" 
  )) %>%
  mutate(classe = as.factor(classe)) %>%
  mutate (classe = fct_recode(classe,
                              "PresqueEquilibre" = "Il est presque équilibré",
                              "Equilibre" = "Il est équilibré",
                              "PasEquilibre" = "Il n'est pas équilibré")) %>% 
  dplyr::select(classe, num_stimulus) %>% 
  data.frame()
res.text <- textual(data_classes2, num.text = 2, contingence.by = 1)
contingence_classe <- res.text$cont.table %>%
  t() %>% 
  data.frame() %>% 
  rownames_to_column("num_stimulus")



data_fix <- data_fixation %>%
  rename(tester_id = id_tester) %>%  
  mutate(num_stimulus = as.factor(num_stimulus)) %>%
  mutate(pays = as.factor(pays)) %>% 
  replace(is.na(.), 0) %>%
  mutate(num_stimulus = as.factor(num_stimulus)) %>% 
  group_by(num_stimulus) %>% 
  summarise(pct_feculents_tps= mean(pct_feculents_tps),pct_proteines_tps= mean(pct_proteines_tps),pct_legumes_tps= mean(pct_legumes_tps),pct_fruits_tps= mean(pct_fruits_tps),pct_produits_laitiers_tps= mean(pct_produits_laitiers_tps)) %>%
  data.frame()


data_AFM <-data_fix %>% 
  full_join(contingence_classe, by = "num_stimulus") %>% 
  full_join(classe_ind, by = "num_stimulus") %>% 
  column_to_rownames("num_stimulus")



library(FactoMineR)

write.table(data, "data/data_AFM.csv", row.names=FALSE, sep=",",dec=".", na=" ")

