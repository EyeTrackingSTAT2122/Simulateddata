library(readr)
library(dbplyr)
library(tidyverse)
suppression <- read_delim("~/Agro/5A/Projet Ingé/Simulateddata/data/data_correction/Suppression.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
concordances <- read_csv("~/Agro/5A/Projet Ingé/Simulateddata/data/data_support/concordances.csv")
split <- read_csv("~/Agro/5A/Projet Ingé/Simulateddata/data/data_support/split.csv")
split2 <- split %>% 
  mutate(diff = num2 - num1) %>% 
  mutate(new_num1 = round(num2-((5/100)*diff))) %>% 
  mutate(num1 = new_num1)

split3 <- split2 %>% 
  rename(id_tester = tester_id) %>%
  rename(id_item = item_id) %>% 
  full_join(concordances, by = c("id_tester", "id_item", "num_stimulus"))

supp = vector()
for (i in 1:nrow(split3)){
  for (j in 1: nrow(suppression)){
    if (split3$id[i]==suppression$id[j] && split3$num_stimulus[i] == suppression$num_stimulus[j]){
      supp <- c(supp, i)
    }
  }
}

split3 <- split3[-supp,] %>% 
  rename(item_id = id_item) %>% 
  rename(tester_id =id_tester )

write.table(split3, "data/data_support/split_supp3.csv", row.names=FALSE, sep=",",dec=".", na=" ")
