library(readr)
library(sjmisc)
library(tidyverse)
library(dplyr)
library(FactoMineR)
library(nnet)
data_fixation <- read_csv("data/data_fix_finales/data_totales/data_finales.csv")
classe_ind <- read_csv("data/data_fix_finales/data_totales/classe_ind.csv")
#supression des individus en trop dans le split
supp1 = vector()
for (i in 1:nrow(classe_ind)){
  if(classe_ind$id[i] == 12|classe_ind$id[i] == 1){
    supp1 <- c(supp1, i)
  }
}
classe_ind <- classe_ind[-supp1,]
data.frame(classe_ind)

#Tableau des classe stimulus X tester
classe_ind2<- classe_ind %>% 
  pivot_wider(names_from = id, values_from = classe) %>% 
  mutate(num_stimulus = as.factor(num_stimulus)) %>% 
  data.frame()

#Tableau de contingence des classes par stimulus
classes_stimuli<- classe_ind %>% 
  dplyr::select(classe, num_stimulus) %>% 
  data.frame()
res.text <- textual(classes_stimuli, num.text = 2, contingence.by = 1)
contingence_classe <- res.text$cont.table %>%
  t() %>% 
  data.frame() %>% 
  rownames_to_column("num_stimulus") %>% 
  mutate(num_stimulus = as.factor(num_stimulus))


#Summarise pour avoir la moyennes des temps de fixation par zone par stimulus
data_fix <- data_fixation %>%
  mutate(num_stimulus = as.factor(num_stimulus)) %>%
  mutate(pays = as.factor(pays)) %>% 
  replace(is.na(.), 0) %>%
  mutate(num_stimulus = as.factor(num_stimulus)) %>% 
  group_by(num_stimulus) %>% 
  summarise(feculents= mean(pct_feculents_tps),proteines= mean(pct_proteines_tps),legumes= mean(pct_legumes_tps),fruits= mean(pct_fruits_tps)) %>%
  data.frame()

data<-contingence_classe %>% 
  full_join(data_fix, by = "num_stimulus") %>%
  full_join(classe_ind2, by = "num_stimulus") %>% 
  data.frame()

conc <- read.csv("data/data_support/concordances_pays.csv") %>% 
  dplyr::select(num_stimulus, pays) %>% 
  group_by(num_stimulus, pays) %>% 
  summarise() %>% 
  mutate(num_stimulus = as.factor(num_stimulus))
data_AFM <- data %>% 
  full_join(conc, by = "num_stimulus") %>% 
  column_to_rownames("pays") %>% 
  dplyr::select(-num_stimulus)

MFA(data_AFM, group = c(3,4,46), type = c("f","s","n"), num.group.sup = 2)
