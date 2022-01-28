library(readr)
library(sjmisc)
library(tidyverse)
library(tidyr)
library(dplyr)
library(FactoMineR)
library(readr)
library(tm)
tab <- read.csv("data/data_fix_finales/fixations.csv")
classe_ind <-  read_csv("data/data_fix_finales/data_supp/classe_ind_supp.csv")
concordances_zones <- read_delim("data/data_support/concordances_zones.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)


tab <- tab %>% 
  mutate(zone = as.factor(zone)) %>% 
  mutate(item_id = as.factor(item_id))

#Pivot wider du tableau pour avoir pour chaque individu/plateau les temps de fixation par zone (fruits/legumes/féculent/proteines)
tab2 <- concordances_zones %>% 
  dplyr::select(id_item, Num_zone, Nom_zone) %>% 
  rename (zone = Num_zone) %>%
  rename(item_id = id_item) %>% 
  mutate(item_id = as.factor(item_id)) %>% 
  mutate(zone = as.factor(zone)) %>% 
  mutate(Nom_zone = as.factor(Nom_zone)) %>%
  full_join(tab, by = c("item_id", "zone")) %>% 
  pivot_wider(id_cols = c(id_form, num), names_from = Nom_zone, values_from = tps, values_fn = sum) %>% 
  rename(legumes_tps = legumes) %>% 
  rename( fruits_tps =  fruits) %>% 
  rename(proteines_tps = proteines) %>% 
  rename(feculents_tps = feculents) %>% 
  replace(is.na(.), 0)

#Pivot wider du tableau pour avoir pour chaque individu/plateau les nombres de fixation par zone (fruits/legumes/féculent/proteines)+ jointure avec tableau précédent
tab3 <- concordances_zones %>% 
  dplyr::select(id_item, Num_zone, Nom_zone) %>% 
  rename (zone = Num_zone) %>%
  rename(item_id = id_item) %>% 
  mutate(item_id = as.factor(item_id)) %>% 
  mutate(zone = as.factor(zone)) %>% 
  mutate(Nom_zone = as.factor(Nom_zone)) %>%
  full_join(tab, by = c("item_id", "zone")) %>% 
  pivot_wider(id_cols = c(id_form, num), names_from = Nom_zone, values_from = nb_visites, values_fn = sum) %>% 
  rename(legumes_fix = legumes) %>% 
  rename( fruits_fix =  fruits) %>% 
  rename(proteines_fix = proteines) %>% 
  rename(feculents_fix = feculents) %>% 
  replace(is.na(.), 0) %>% 
  full_join(tab2, by = c("id_form", "num")) %>% 
  rename(id = id_form) %>% 
  rename(num_stimulus = num) %>% 
  full_join(classe_ind, by = c("num_stimulus", "id"))


classe_id <- tab3 %>%
  dplyr::select(num_stimulus, id, classe) %>%
  pivot_wider(names_from = id, values_from = classe) %>%
  mutate(num_stimulus = as.factor(num_stimulus)) %>% 
  data.frame()

classe_stimuli <- tab3 %>% 
  dplyr::select(num_stimulus, classe) %>% 
  mutate(num_stimulus = as.factor(num_stimulus)) %>% 
  mutate(classe = as.factor(classe)) %>% 
  data.frame()
res.text <- textual(classe_stimuli, num.text = 1, contingence.by = 2)
contingence_classe <- res.text$cont.table %>%
  t() %>% 
  data.frame() %>% 
  rownames_to_column("num_stimulus") %>% 
  mutate(num_stimulus = as.factor(num_stimulus))

data_tps <- tab3%>%
  group_by(num_stimulus, id)%>%
  mutate(tot = sum(feculents_tps+proteines_tps+legumes_tps+fruits_tps))%>%
  mutate(feculents_tps = (feculents_tps/tot)*100)%>% 
  mutate(proteines_tps = (proteines_tps/tot)*100)%>% 
  mutate(legumes_tps = (legumes_tps/tot)*100)%>% 
  mutate (fruits_tps = (fruits_tps/tot)*100)%>% 
  group_by(num_stimulus)%>%
  summarise(feculents_tps= mean(feculents_tps),proteines_tps= mean(proteines_tps),legumes_tps= mean(legumes_tps),fruits_tps= mean(fruits_tps)) %>%
  mutate(num_stimulus = as.factor(num_stimulus))%>% 
  data.frame()

data_fix <- tab3%>%
  group_by(num_stimulus, id)%>%
  mutate(tot = sum(feculents_fix+proteines_fix+legumes_fix+fruits_fix))%>%
  mutate(feculents_fix = (feculents_fix/tot)*100)%>% 
  mutate(proteines_fix = (proteines_fix/tot)*100)%>% 
  mutate(legumes_fix = (legumes_fix/tot)*100)%>% 
  mutate (fruits_fix = (fruits_fix/tot)*100)%>% 
  group_by(num_stimulus)%>%
  summarise(feculents_fix= mean(feculents_fix),proteines_fix= mean(proteines_fix),legumes_fix= mean(legumes_fix),fruits_fix= mean(fruits_fix)) %>%
  mutate(num_stimulus = as.factor(num_stimulus))%>% 
  data.frame()

data_AFM2 <- contingence_classe %>%
  full_join(classe_id, by = "num_stimulus") %>%
  full_join(data_tps, by = "num_stimulus") %>% 
  full_join(data_fix, by = "num_stimulus") %>%
  full_join(data1, by = )
  column_to_rownames("num_stimulus") 

text <- read_delim("data/data_support/donnees_explicites.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
t1 <- text[,c(2:3,5,8)]
glimpse(t1)  

corpus = Corpus(VectorSource(t1$Comm_net))
frequencies = DocumentTermMatrix(corpus)
data = as.data.frame(as.matrix(frequencies))
colnames(data) = make.names(colnames(data))
data$Classe = t1$Classe
data$num_stimulus= t1$Stimulus
data$id = t1$Num_Sujet

data1 <- data %>% 
  dplyr::select(id, num_stimulus, ends_with(c("légumes", "protéines","fruits","féculents"))) %>% 
  dplyr::select(id, num_stimulus, starts_with(c("trop", "pas", "assez"))) %>% 
  mutate(legumes = ifelse(trop_légumes == 1, "Trop", ifelse(pas_assez_légumes==1, "PasAssez", "Assez"))) %>% 
  mutate(proteines = ifelse(trop_protéines == 1, "Trop", ifelse(pas_assez_protéines==1, "PasAssez", "Assez"))) %>% 
  mutate(fruits = ifelse(trop_fruits == 1, "Trop", ifelse(pas_assez_fruits==1, "PasAssez", "Assez"))) %>% 
  mutate(feculents = ifelse(trop_féculents == 1, "Trop", ifelse(pas_assez_féculents==1, "PasAssez", "Assez"))) %>% 
  dplyr::select(id, num_stimulus, legumes, fruits, proteines, feculents)

