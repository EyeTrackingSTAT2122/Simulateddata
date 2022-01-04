library(flexdashboard)
library(tidyverse)
AOI <- read.csv("~/Agro/5A/Projet Ingé/stats_AOI.csv", sep=";")
classes <- read.csv("~/Agro/5A/Projet Ingé/stat_classes.csv", sep=";")
View(classes)

data_classes<-classes %>% 
  pivot_longer(cols = starts_with("D.apres.toi"), names_to = "stimulus", values_to = "classe") %>%
  mutate(tester_id = as.factor(tester_id)) %>% 
  mutate(stimulus = as.factor(stimulus)) %>% 
  mutate(stimulus = fct_recode(stimulus,
                               "Brazil" = "D.apres.toi..ce.plateau.est.il.equilibre.....1..Brazil.jpg",
                               "Finland"="D.apres.toi..ce.plateau.est.il.equilibre.....2..Finland.jpg",
                               "France"="D.apres.toi..ce.plateau.est.il.equilibre.....3..France.jpg",
                               "Greece"="D.apres.toi..ce.plateau.est.il.equilibre.....4..Greece.jpg",
                               "Italy"="D.apres.toi..ce.plateau.est.il.equilibre.....5..Italy.jpg",
                               "Spain"="D.apres.toi..ce.plateau.est.il.equilibre.....6..Spain.jpg",
                               "South.Korea"="D.apres.toi..ce.plateau.est.il.equilibre.....7..South.Korea.jpg",
                               "Ukraine"="D.apres.toi..ce.plateau.est.il.equilibre.....8..Ukraine.jpg",
                               "USA"="D.apres.toi..ce.plateau.est.il.equilibre.....9..USA.jpg" 
         ))
View(data_classes)

dimnames(AOI)
data_AOI <- AOI %>%
  rename(stimulus = item_filename) %>%
  mutate(tester_id = as.factor(tester_id)) %>%
  mutate(stimulus = as.factor(stimulus)) %>% 
  mutate(stimulus = fct_recode(stimulus,
                               "Brazil" = "1. Brazil.jpg",
                               "Finland" = "2. Finland.jpg",
                               "France" = "3. France.jpg",
                               "Greece" = "4. Greece.jpg",
                               "Italy" = "5. Italy.jpg",
                               "Spain" = "6. Spain.jpg",
                               "South.Korea" = "7. South Korea.jpg",
                               "Ukraine" = "8. Ukraine.jpg",
                               "USA" = "9. USA.jpg")) %>% 
  mutate(aoi_name = fct_recode(aoi_name,
                               "Feculents" = "Feculent",
                               "Feculents/proteines" = "Feculent/proteines",
                               "Proteines" = "Proteine"))
  

data_join <- data_AOI %>% 
  full_join(data_classes, by = c("tester_id","stimulus")) %>% 
  select(tester_id, stimulus, aoi_name, starts_with("aoi_fixation"), classe) %>% 
  replace(is.na(.), 0) %>% 
  mutate(classe = as.factor(classe))

View(data_join)
summary(data_join)
dimnames(data_join)
library(nnet)
library(RcmdrMisc)
mod <- multinom(classe ~., data = data_join)
select = stepwise(mod,direction="forward/backward",criterion="BIC")
res <- PCA(data_join, quali.sup = c(1,2,3,9))

res_aov <- lm(classe~aoi_fixation_average_duration_ms+stimulus+aoi_name, data= data_join)

View(data_join)

ggplot(data_join) +
  aes(x = aoi_name, y = aoi_fixation_average_duration_ms, color = aoi_name) +
  geom_boxplot() +
  theme(legend.position = "none")

+aoi_fixation_total_count+aoi_fixation_average_duration_ms+aoi_fixation_ttff_ms+aoi_fixation_average_total_time_spent_ms+aoi_fixation_average_total_time_spent_ms+aoi_fixation_first_fixation_average_duration_ms