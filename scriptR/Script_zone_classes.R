library(tidyverse)
library(dplyr)
data1 <- read_csv("data/data_dim_pct_fin.csv")
data2 <- read_csv("data/Real_eye/resume_final.csv")
data3 <- read_csv("data/Real_eye/resume_sauvetage.csv")

classes <- rbind(data2, data3)

data_classes<- classes %>% 
  select(tester_id, starts_with("D'après")) %>% 
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
  full_join(data_classes, by = c("tester_id","num_stimulus")) %>% 
  replace(is.na(.), 0) %>% 
  mutate(classe = as.factor(classe)) %>% 
  mutate (classe = fct_recode(classe,
                              "PresqueEquillibré" = "Il est presque équilibré",
                              "Equillibré" = "Il est équilibré",
                              "PasEquillibré" = "Il n'est pas équilibré"))


