library(tidyverse)
library(dplyr)
library(FactoMineR)
library(nnet)
library(Rcmdr)
library(factoextra)
data1 <- read_csv("data/data_finale2.csv")
data2 <- read_csv("data/Real_eye/resume_final.csv")
data3 <- read_csv("data/Real_eye/resume_sauvetage.csv")
explicite <- read_csv("data/explicite.csv")

classes <- rbind(data2, data3)

data_classes<- classes %>% 
  dplyr::select(tester_id,tester_name, starts_with("D'après")) %>% 
  mutate(tester_name = ifelse(str_length(tester_name) == 14, substr(tester_name,1,2),substr(tester_name,1,1))) %>%
  mutate(tester_name = as.numeric(tester_name)) %>% 
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
  ))

data_join <- data1 %>%
  rename(tester_id = id_tester) %>%
  mutate(num_stimulus = as.factor(num_stimulus)) %>%
  mutate(pays = as.factor(pays)) %>% 
  full_join(data_classes, by = c("tester_id","num_stimulus")) %>% 
  replace(is.na(.), 0) %>% 
  mutate(classe = as.factor(classe)) %>% 
  mutate (classe = fct_recode(classe,
                              "PresqueEquilibré" = "Il est presque équilibré",
                              "Equilibré" = "Il est équilibré",
                              "PasEquilibré" = "Il n'est pas équilibré")) %>% 
  mutate(classe_stimuli = paste(num_stimulus,classe, sep ="_")) %>% 
  group_by(num_stimulus) %>% 
  summarise(pct_feculents_tps= mean(pct_feculents_tps),pct_proteines_tps= mean(pct_proteines_tps),pct_legumes_tps= mean(pct_legumes_tps),pct_fruits_tps= mean(pct_fruits_tps),pct_produits_laitiers_tps= mean(pct_produits_laitiers_tps))

data_join <- data1 %>%
  rename(tester_id = id_tester) %>%
  mutate(num_stimulus = as.factor(num_stimulus)) %>%
  mutate(pays = as.factor(pays)) %>% 
  full_join(data_classes, by = c("tester_id","num_stimulus")) %>% 
  replace(is.na(.), 0) %>% 
  mutate(classe = as.factor(classe)) %>% 
  mutate (classe = fct_recode(classe,
                              "PresqueEquilibré" = "Il est presque équilibré",
                              "Equilibré" = "Il est équilibré",
                              "PasEquilibré" = "Il n'est pas équilibré")) 

write.table(data_join, "data/Zones_Classes.csv", row.names=FALSE, sep=",",dec=".", na=" ")


data_pca <- data_join %>% 
  dplyr::select(starts_with("pct"), cat_max_tps, classe_stimuli) %>% 
  dplyr::select(-ends_with("fix"), cat_max_tps, classe_stimuli)
res.pca <- PCA(data_pca, quali.sup = c(6:7))
plot.PCA(res.pca, invisible = c("ind"))


data_156 <- data_join %>% 
  filter(num_stimulus == 156) %>% 
  dplyr::select(starts_with("pct"), cat_max_fix,cat_max_tps, classe_stimuli)  
res.156 <- PCA(data_156, quali.sup = c(11:13))
plot.PCA(res.156, invisible = c("ind"))

data_672 <- data_join %>% 
  filter(num_stimulus == 672) %>% 
  dplyr::select(starts_with("pct"), cat_max_fix,cat_max_tps, classe_stimuli)  
res.672 <- PCA(data_672, quali.sup = c(11:13))
plot.PCA(res.672, invisible = c("ind"))

data_756 <- data_join %>% 
  filter(num_stimulus == 756) %>% 
  dplyr::select(starts_with("pct"), cat_max_fix,cat_max_tps, classe_stimuli)  
res.756 <- PCA(data_756, quali.sup = c(11:13))
plot.PCA(res.756, invisible = c("ind"))

data_421 <- data_join %>% 
  filter(num_stimulus == 421) %>% 
  dplyr::select(starts_with("pct"), cat_max_fix,cat_max_tps, classe_stimuli)  
res.421 <- PCA(data_421, quali.sup = c(11:13))
plot.PCA(res.421, invisible = c("ind"))

res <- cbind(res.pca$ind$coord, data_PCA)
mod1 <- lm(Dim.1 ~ classe_stimuli*(pct_legumes + pct_proteines+ pct_fruits+ pct_feculents), data = res)
mod2 <- lm(Dim.2 ~ classe_stimuli*(pct_legumes + pct_proteines+ pct_fruits+ pct_feculents), data = res)
res.anova <- anova(mod1); res.anova
res.anova2 <- anova(mod2)
coefficients(mod1)
summary(mod1)
summary(mod2)

#Prediction

mod <- multinom(classe*pays ~., data = data_PCA)
select = stepwise(mod,direction="forward/backward",criterion="BIC")
