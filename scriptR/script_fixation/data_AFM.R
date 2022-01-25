library(tidyverse)
library(dplyr)
library(FactoMineR)
library(nnet)
library(Rcmdr)

data_fixation <- read_csv("data/data_fix_finales/data_finales.csv")
data2 <- read_csv("data/Real_eye/resume_final.csv")
data3 <- read_csv("data/Real_eye/resume_sauvetage.csv")
last_first <- read_csv("data/data_fix_finales/last_first.csv")
suppression <- read_delim("~/Agro/5A/Projet Ingé/Simulateddata/data/data_correction/Suppression.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
classes <- rbind(data2, data3)
classes$tester_name[44] <- "34, Female, 23"
concord <- read_csv("data/data_support/concordances.csv")

data_fixation<- concord %>% 
  full_join(data_fixation, by = c("id_tester", "num_stimulus", "id_item"))

i =1
j=1
supp = vector()
for (i in 1:nrow(data_fixation)){
  for (j in 1:nrow(suppression)){
    if(data_fixation$id[i] == suppression$id[j] && data_fixation$num_stimulus[i] == suppression$num_stimulus[j]){
      supp <- c(supp, i)
    }
  }
}


#Tableau des classe stimulus X tester
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

#Tableau de contingence des classes par stimulus
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
res.text <- textual(classes_stimuli, num.text = 2, contingence.by = 1)
contingence_classe <- res.text$cont.table %>%
  t() %>% 
  data.frame() %>% 
  rownames_to_column("num_stimulus")


#Summarise pour avoir la moyennes des temps de fixation par zone par stimulus
data_fix <- data_fixation %>%
  mutate(num_stimulus = as.factor(num_stimulus)) %>%
  mutate(pays = as.factor(pays)) %>% 
  replace(is.na(.), 0) %>%
  mutate(num_stimulus = as.factor(num_stimulus)) %>% 
  group_by(num_stimulus) %>% 
  summarise(pct_feculents_tps= mean(pct_feculents_tps),pct_proteines_tps= mean(pct_proteines_tps),pct_legumes_tps= mean(pct_legumes_tps),pct_fruits_tps= mean(pct_fruits_tps)) %>%
  data.frame()


#Jointure jeux de données 1ère et dernière fixation
data_fixation2 <- data_fixation %>% 
  full_join(last_first, by =c("id_item", "id_tester")) %>% 
  replace(is.na(.), 0) %>%
  mutate(num_stimulus = as.factor(num_stimulus)) %>% 
  dplyr::select(num_stimulus, first_zone, last_zone) %>%
  mutate(last_zone = as.factor(last_zone)) %>% 
  mutate(first_zone = as.factor(first_zone)) %>% 
  group_by(num_stimulus, first_zone) %>% 
  mutate(n_first = n()) %>% 
  group_by(num_stimulus, last_zone) %>% 
  mutate(n_last = n()) %>% 
  group_by(num_stimulus) %>% 
  filter(n_first == max(n_first),
         n_last == max(n_last)) %>% 
  group_by(num_stimulus, first_zone, last_zone) %>% 
  summarise() %>% 
  data.frame()

data_AFM <-contingence_classe %>% 
  full_join(data_fix, by = "num_stimulus") %>%
  full_join(data_fixation2, by = "num_stimulus") %>% 
  full_join(classe_ind, by = "num_stimulus") %>% 
  column_to_rownames("num_stimulus") %>% 
  data.frame()


write.table(data_AFM, "data/data_fix_finales/data_AFM.csv", row.names=FALSE, sep=",",dec=".", na=" ")

