library(readxl)
library(tidyverse)

RealEye <- read_excel("~/Agrocampus/5A/Projet ingénieur/Survey_RealEye.xlsx")

RealEye <- RealEye %>% 
  pivot_longer(cols = starts_with("D'apres toi"), names_to = "Pays", values_to = "Classe") %>%
  mutate(Pays = as.factor(Pays)) %>% 
  mutate(Pays = fct_recode(Pays,
                               "Brazil" = "D'apres toi, ce plateau est-il equilibre ? - 1. Brazil.jpg",
                               "Finland"="D'apres toi, ce plateau est-il equilibre ? - 2. Finland.jpg",
                               "France"="D'apres toi, ce plateau est-il equilibre ? - 3. France.jpg",
                               "Greece"="D'apres toi, ce plateau est-il equilibre ? - 4. Greece.jpg",
                               "Italy"="D'apres toi, ce plateau est-il equilibre ? - 5. Italy.jpg",
                               "Spain"="D'apres toi, ce plateau est-il equilibre ? - 6. Spain.jpg",
                               "South.Korea"="D'apres toi, ce plateau est-il equilibre ? - 7. South Korea.jpg",
                               "Ukraine"="D'apres toi, ce plateau est-il equilibre ? - 8. Ukraine.jpg",
                               "USA"="D'apres toi, ce plateau est-il equilibre ? - 9. USA.jpg")) %>%
  mutate(Classe = case_when(
      Classe == "Il est equilibre" ~ "Equilibre",
      Classe == "Il est presque equilibre" ~ "Presque equilibre",
      Classe == "Il n'est pas equilibre" ~ "Non equilibre")) %>%
  separate(tester_name, c("tester_num", "genre", "age")) %>%
  select(-study_id,-genre,-age,-tester_quality_grade,-tester_gender)


GoogleForm <- read_excel("~/Agrocampus/5A/Projet ingénieur/Survey_GoogleForm.xlsx")

GoogleForm <- GoogleForm %>% 
  select(-Horodateur) %>%
  rename(num_juge = "Numéro de juge :",
         S1 = "Quel est le numéro du stimulus ? (1)",
         S2 = "Quel est le numéro du stimulus ? (2)",
         S3 = "Quel est le numéro du stimulus ? (3)",
         S4= "Quel est le numéro du stimulus ? (4)",
         S5 = "Quel est le numéro du stimulus ? (5)",
         S6 = "Quel est le numéro du stimulus ? (6)",
         S7 = "Quel est le numéro du stimulus ? (7)",
         S8 = "Quel est le numéro du stimulus ? (8)",
         S9 = "Quel est le numéro du stimulus ? (9)",
         HedoS1 = "Note ton appréciation hédonique du plateau sur une échelle allant de 0 à 10 (de 0 = je n'aime pas du tout à 10 = j'aime totalement)...5",
         HedoS2 = "Note ton appréciation hédonique du plateau sur une échelle allant de 0 à 10 (de 0 = je n'aime pas du tout à 10 = j'aime totalement)...10",
         HedoS3 = "Note ton appréciation hédonique du plateau sur une échelle allant de 0 à 10 (de 0 = je n'aime pas du tout à 10 = j'aime totalement)...15",
         HedoS4 = "Note ton appréciation hédonique du plateau sur une échelle allant de 0 à 10 (de 0 = je n'aime pas du tout à 10 = j'aime totalement)...20",
         HedoS5 = "Note ton appréciation hédonique du plateau sur une échelle allant de 0 à 10 (de 0 = je n'aime pas du tout à 10 = j'aime totalement)...25",
         HedoS6 = "Note ton appréciation hédonique du plateau sur une échelle allant de 0 à 10 (de 0 = je n'aime pas du tout à 10 = j'aime totalement)...30",
         HedoS7 = "Note ton appréciation hédonique du plateau sur une échelle allant de 0 à 10 (de 0 = je n'aime pas du tout à 10 = j'aime totalement)...35",
         HedoS8 = "Note ton appréciation hédonique du plateau sur une échelle allant de 0 à 10 (de 0 = je n'aime pas du tout à 10 = j'aime totalement)...40",
         HedoS9 = "Note ton appréciation hédonique du plateau sur une échelle allant de 0 à 10 (de 0 = je n'aime pas du tout à 10 = j'aime totalement)...45",
         NutriS1 = "Note l'équilibre nutritionnel du plateau sur une échelle allant de 0 à 10 (de 0 = pas du tout équilibré à 10 = parfaitement équilibré)...4",
         NutriS2 = "Note l'équilibre nutritionnel du plateau sur une échelle allant de 0 à 10 (de 0 = pas du tout équilibré à 10 = parfaitement équilibré)...9",
         NutriS3 = "Note l'équilibre nutritionnel du plateau sur une échelle allant de 0 à 10 (de 0 = pas du tout équilibré à 10 = parfaitement équilibré)...14",
         NutriS4 = "Note l'équilibre nutritionnel du plateau sur une échelle allant de 0 à 10 (de 0 = pas du tout équilibré à 10 = parfaitement équilibré)...19",
         NutriS5 = "Note l'équilibre nutritionnel du plateau sur une échelle allant de 0 à 10 (de 0 = pas du tout équilibré à 10 = parfaitement équilibré)...24",
         NutriS6 = "Note l'équilibre nutritionnel du plateau sur une échelle allant de 0 à 10 (de 0 = pas du tout équilibré à 10 = parfaitement équilibré)...29",
         NutriS7 = "Note l'équilibre nutritionnel du plateau sur une échelle allant de 0 à 10 (de 0 = pas du tout équilibré à 10 = parfaitement équilibré)...34",
         NutriS8 = "Note l'équilibre nutritionnel du plateau sur une échelle allant de 0 à 10 (de 0 = pas du tout équilibré à 10 = parfaitement équilibré)...39",
         NutriS9 = "Note l'équilibre nutritionnel du plateau sur une échelle allant de 0 à 10 (de 0 = pas du tout équilibré à 10 = parfaitement équilibré)...44",
         ZonePlusS1 = "Quelle zone correspond, selon toi, à la zone du plateau la plus équilibrée ? / Quelle zone aimes-tu le plus sur le plateau ?...6",
         ZonePlusS2 = "Quelle zone correspond, selon toi, à la zone du plateau la plus équilibrée ? / Quelle zone aimes-tu le plus sur le plateau ?...11",
         ZonePlusS3 = "Quelle zone correspond, selon toi, à la zone du plateau la plus équilibrée ? / Quelle zone aimes-tu le plus sur le plateau ?...16",
         ZonePlusS4 = "Quelle zone correspond, selon toi, à la zone du plateau la plus équilibrée ? / Quelle zone aimes-tu le plus sur le plateau ?...21",
         ZonePlusS5 = "Quelle zone correspond, selon toi, à la zone du plateau la plus équilibrée ? / Quelle zone aimes-tu le plus sur le plateau ?...26",
         ZonePlusS6 = "Quelle zone correspond, selon toi, à la zone du plateau la plus équilibrée ? / Quelle zone aimes-tu le plus sur le plateau ?...31",
         ZonePlusS7 = "Quelle zone correspond, selon toi, à la zone du plateau la plus équilibrée ? / Quelle zone aimes-tu le plus sur le plateau ?...36",
         ZonePlusS8 = "Quelle zone correspond, selon toi, à la zone du plateau la plus équilibrée ? / Quelle zone aimes-tu le plus sur le plateau ?...41",
         ZonePlusS9 = "Quelle zone correspond, selon toi, à la zone du plateau la plus équilibrée ? / Quelle zone aimes-tu le plus sur le plateau ?...46",
         ZoneMoinsS1 = "Quelle zone correspond, selon toi, à la zone du plateau la moins équilibrée ? / Quelle zone aimes-tu le moins sur le plateau ?...7",
         ZoneMoinsS2 = "Quelle zone correspond, selon toi, à la zone du plateau la moins équilibrée ? / Quelle zone aimes-tu le moins sur le plateau ?...12",
         ZoneMoinsS3 = "Quelle zone correspond, selon toi, à la zone du plateau la moins équilibrée ? / Quelle zone aimes-tu le moins sur le plateau ?...17",
         ZoneMoinsS4 = "Quelle zone correspond, selon toi, à la zone du plateau la moins équilibrée ? / Quelle zone aimes-tu le moins sur le plateau ?...22",
         ZoneMoinsS5 = "Quelle zone correspond, selon toi, à la zone du plateau la moins équilibrée ? / Quelle zone aimes-tu le moins sur le plateau ?...27",
         ZoneMoinsS6 = "Quelle zone correspond, selon toi, à la zone du plateau la moins équilibrée ? / Quelle zone aimes-tu le moins sur le plateau ?...32",
         ZoneMoinsS7 = "Quelle zone correspond, selon toi, à la zone du plateau la moins équilibrée ? / Quelle zone aimes-tu le moins sur le plateau ?...37",
         ZoneMoinsS8 = "Quelle zone correspond, selon toi, à la zone du plateau la moins équilibrée ? / Quelle zone aimes-tu le moins sur le plateau ?...42",
         ZoneMoinsS9 = "Quelle zone correspond, selon toi, à la zone du plateau la moins équilibrée ? / Quelle zone aimes-tu le moins sur le plateau ?...47")
         