library(readr)
library(sjmisc)
library(tidyverse)
library(tidyr)
library(dplyr)

coord_split1 <- read.csv("data/Real_eye/data_finale.csv", sep=",")
coord_split2 <- read.csv("data/Real_eye/data_sauvetage.csv", sep=",")
Coord_Zone <-  read_delim("data/Coord_zones.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
decalage <-  read_csv("data/data_support/coordonnes_decalages.csv")
coord_split <-  read_csv("data/sata_support/coord_split.csv")
split <- read_csv("data/sata_support/split.csv")


calcul_temps<- function (id_item, id_tester){
  #Récuperer le i (num_test) de coord_split correspondant au couple item/tester
  
  
  for (i in 1:nrow(split)){
    if (id_item == split$item_id[i] && id_tester == split$tester_id[i]){
      num1 <- split$num1[i]
      num2 <- split$num2[i]
    }
  }
  
  for (i in 1:nrow(decalage)){
    if (id_item == decalage$id_item && id_tester == decalage$id_tester){
      var_x <- decalage$x_val
      var_y <- decalage$y_val
      dta <- coord_split[(num1+1):(num2-1),] %>%
        dplyr::select(gaze_x_percents,gaze_y_percents, gaze_timestamp_ms) %>% 
        rename(x = gaze_x_percents) %>% 
        rename(time = gaze_timestamp_ms) %>% 
        rename(y = gaze_y_percents) %>% 
        mutate(x = x + var_x ) %>%
        mutate(y = y + var_y)
    }
  }

  
  num_zone <- rep (0,0)
  x1 <- rep (0,0)
  x2 <- rep (0,0)
  y1 <- rep (0,0)
  y2 <- rep (0,0)
  coord_zone<- data.frame(num_zone,x1,x2,y1,y2)
  i=1
  j=1
  for (j in 1:6){
    for (i in 1:nrow(Coord_Zone)){
      if (id_item == Coord_Zone$id_item[i] & j== Coord_Zone$Num_zone[i] ){
        coord_zone[j,"num_zone"] <- j
        coord_zone[j,"x1"]<- Coord_Zone[i, "x1"]
        coord_zone[j,"x2"]<- Coord_Zone[i, "x2"]
        coord_zone[j,"y1"]<- Coord_Zone[i, "y1"]
        coord_zone[j,"y2"]<- Coord_Zone[i, "y2"]
      }
    }
  }
  
  
  Ordre = rep(0,0)
  zone1 = rep(0, 0)
  tps_zone1 = rep(0, 0)
  zone2 = rep(0, 0)
  tps_zone2 = rep(0, 0)
  zone3 = rep(0, 0)
  tps_zone3 = rep(0, 0)
  zone4 = rep(0, 0)
  tps_zone4 = rep(0, 0)
  zone5 = rep(0, 0)
  tps_zone5 = rep(0, 0)
  
  data_zone <- data.frame(Ordre, zone1, tps_zone1, zone2, tps_zone2, zone3, tps_zone3, zone4, tps_zone4, zone5, tps_zone5)
  
  lg_data <- dim(dta)[1]
  x = dta[,1]
  y = dta [,2]
  time = dta[,3]

  
  i=1
  j=1
  while (i < lg_data-1 | time < 5000 ){
    #Zone 1 :
    if (x[i]>= coord_zone[1,"x1"] && x[i]<= coord_zone[1,"x2"] && y[i]>= coord_zone[1,"y1"] && y[i]<= coord_zone[1,"y2"]){
      t1 = time[i]
      while(x[i]>= coord_zone[1,"x1"] && x[i]<= coord_zone[1,"x2"] && y[i]>= coord_zone[1,"y1"] && y[i]<= coord_zone[1,"y2"] && i < (lg_data)){
        i= i+1
      }
      i = i+1
      t2 = time[i]
      data_zone[j, "Ordre"] <- j
      data_zone[j,"zone1"]<- 1
      data_zone[j,"tps_zone1"]<- t2-t1
    }
    
    #Zone 2 : 
    else if (x[i]>= coord_zone[2,"x1"] && x[i]<= coord_zone[2,"x2"] && y[i]>= coord_zone[2,"y1"] && y[i]<= coord_zone[2,"y2"]){
      t1 = time[i]
      while(x[i]>= coord_zone[2,"x1"] && x[i]<= coord_zone[2,"x2"] && y[i]>= coord_zone[2,"y1"] && y[i]<= coord_zone[2,"y2"] && i < (lg_data)){
        i = i+1
      }
      t2 = time[i]
      i = i +1
      data_zone[j, "Ordre"] <- j
      data_zone[j,"zone2"]<- 1
      data_zone[j,"tps_zone2"]<- t2-t1
      
    }
    
    #Zone 3 :
    else if (x[i]>= coord_zone[3,"x1"] && x[i]<= coord_zone[3,"x2"] && y[i]>= coord_zone[3,"y1"] && y[i]<= coord_zone[3,"y2"]){
      t1 = time[i]
      while(x[i]>= coord_zone[3,"x1"] && x[i]<= coord_zone[3,"x2"] && y[i]>= coord_zone[3,"y1"] && y[i]<= coord_zone[3,"y2"] && i < (lg_data)){
        i = i +1
      }
      i = i + 1
      t2 = time[i]
      data_zone[j, "Ordre"] <- j
      data_zone[j,"zone3"]<- 1
      data_zone[j,"tps_zone3"]<- t2-t1
    }
    #Zone 4 :
    else if (x[i]>= coord_zone[4,"x1"] && x[i]<= coord_zone[4,"x2"] && y[i]>= coord_zone[4,"y1"] && y[i]<= coord_zone[4,"y2"]){
      t1 = time[i]
      while(x[i]>= coord_zone[4,"x1"] && x[i]<= coord_zone[4,"x2"] && y[i]>= coord_zone[4,"y1"] && y[i]<= coord_zone[4,"y2"] && i < (lg_data)){
        i = i+1
      }
      i= i + 1
      t2 = time[i]
      data_zone[j, "Ordre"] <- j
      data_zone[j,"zone4"]<- 1
      data_zone[j,"tps_zone4"]<- t2-t1
    }
    
    #Zone 5 :
    else if (x[i]>= coord_zone[5,"x1"] && x[i]<= coord_zone[5,"x2"] && y[i]>= coord_zone[5,"y1"] && y[i]<= coord_zone[5,"y2"]){
      t1 = time[i]
      while(x[i]>= coord_zone[5,"x1"] && x[i]<= coord_zone[5,"x2"] && y[i]>= coord_zone[5,"y1"] && y[i]<= coord_zone[5,"y2"] && i < (lg_data)){
        i = i+1
      }
      i= i + 1
      t2 = time[i]
      data_zone[j, "Ordre"] <- j
      data_zone[j,"zone5"]<- 1
      data_zone[j,"tps_zone5"]<- t2-t1
    }
    
    else{
      i = i+1
    }
    j= j+1
  }
  
  data_zone[is.na(data_zone)] <- 0
  fixation_time_aoi <- data_zone %>% 
    dplyr::select(-Ordre) %>% 
    t()
  fixation_total_time = apply(fixation_time_aoi,1, sum)
  return(fixation_total_time)
  
}

calcul_temps(id_item, id_tester)

#On fait tourner la fonction de calcul de temps de fixation sur l'ensemble des couple item/tester (= un individu stat)
#On met les resultats dans un tableau qui va regrouper tous les resultats de tous les individus
id_tester <- rep (0,0)
id_item <- rep (0,0)
pays <- rep(0,0)
num_stimulus <- rep (0,0)
Temps_Z1 <- rep (0,0)
Temps_Z2 <- rep (0,0)
Temps_Z3 <- rep (0,0)
Temps_Z4 <- rep (0,0)
Temps_Z5 <- rep (0,0)
rep_Z1 <- rep (0,0)
rep_Z2 <- rep(0,0)
rep_Z3 <- rep (0,0)
rep_Z4 <- rep (0,0)
rep_Z5 <- rep (0,0)
tps_tot <- rep (0,0)
fix_tot <- rep (0,0)
tab <- data.frame(id_tester,id_item, pays, num_stimulus,rep_Z1, rep_Z2, rep_Z3, rep_Z4, rep_Z5, fix_tot,Temps_Z1,Temps_Z2,Temps_Z3,Temps_Z4,Temps_Z5, tps_tot)

i= 1
for (i in 1 : nrow(split)){
  fixation <- calcul_temps(split$item_id[i], split$tester_id[i])
  tab[i,"id_tester"] <- split$tester_id[i]
  tab[i,"id_item"] <- split$item_id[i]
  tab[i,"pays"] <- split$pays[i]
  tab[i, "num_stimulus"]<- split$num_stimulus[i]
  tab[i,"rep_Z1"] <- fixation[1]
  tab[i,"rep_Z2"] <- fixation[3]
  tab[i,"rep_Z3"] <- fixation[5]
  tab[i,"rep_Z4"] <- fixation[7]
  tab[i,"rep_Z5"] <- fixation[9]
  tab[i, "fix_tot"]<- sum(fixation[1],fixation[3],fixation[5],fixation[7],fixation[9])
  tab[i,"Temps_Z1"] <- fixation[2]
  tab[i,"Temps_Z2"] <- fixation[4]
  tab[i,"Temps_Z3"] <- fixation[6]
  tab[i,"Temps_Z4"] <- fixation[8]
  tab[i,"Temps_Z5"] <- fixation[10]
  tab[i, "tps_tot"]<- sum(fixation[2],fixation[4],fixation[6],fixation[8],fixation[10])
  print(i)
}

tab

write.table(tab, "data/data_fixation_fin2.csv", row.names=FALSE, sep=",",dec=".", na=" ")

#En fonction de ce qu'il y a dans les zones 1 ? 6, on va affecter des temps de fixation ? nos dimension f?culents, proteines, fruits et l?gumes
tab <- read_csv("~/Agro/5A/Projet Ingé/Simulateddata/data/data_fixation_fin2.csv")

i=1
k = 1
j =5

Coord_Zone2<- Coord_Zone %>% 
  dplyr::select(id_item, Num_zone, Nom_zone)

data1<- tab %>%
  dplyr::select(-fix_tot,-(starts_with("rep"))) %>% 
  rename("1" = Temps_Z1) %>% 
  rename("2" = Temps_Z2) %>% 
  rename("3" = Temps_Z3) %>% 
  rename("4" = Temps_Z4) %>% 
  rename("5" = Temps_Z5) %>%
  pivot_longer(cols = 5:9, names_to = "Num_zone") %>%
  mutate(Num_zone = as.numeric(Num_zone))
  
data2<- tab %>%
  dplyr::select(-tps_tot,-(starts_with("Temps"))) %>% 
  rename("1" = rep_Z1) %>% 
  rename("2" = rep_Z2) %>% 
  rename("3" = rep_Z3) %>% 
  rename("4" = rep_Z4) %>% 
  rename("5" = rep_Z5) %>%
  pivot_longer(cols = 5:9, names_to = "Num_zone") %>%
  mutate(Num_zone = as.numeric(Num_zone)) %>% 
  full_join(data1, by = c("id_item", "Num_zone", "id_tester", "pays", "num_stimulus")) %>% 
  full_join(Coord_Zone2, by = c("id_item", "Num_zone")) %>% 
  rename("nb_fix"= "value.x") %>% 
  rename("tps_fix" = "value.y")
  
rp<-nrow(tab)
id_tester <- rep (0,rp)
id_item <- rep (0,rp)
pays <- rep (0,rp)
num_stimulus<-rep (0,rp)
feculents_tps <- rep (0,rp)
proteines_tps <- rep (0,rp)
fruits_tps <- rep (0,rp)
legumes_tps <- rep (0,rp)
produits_laitiers_tps <- rep (0,rp)
tps_tot <- rep (0,rp)
feculents_fix <- rep (0,rp)
proteines_fix<- rep (0,rp)
fruits_fix <- rep (0,rp)
legumes_fix <- rep (0,rp)
produits_laitiers_fix <- rep (0,rp)
fix_tot <- rep (0,rp)

data <- data.frame(id_tester, id_item, pays, num_stimulus, feculents_fix, proteines_fix, fruits_fix, legumes_fix, produits_laitiers_fix, fix_tot,feculents_tps, proteines_tps, fruits_tps, legumes_tps, produits_laitiers_tps, tps_tot)


i = 124
j = 2

for (i in 1:nrow(tab)){
  data[i,"id_tester"] <- tab$id_tester[i]
  data[i,"id_item"] <- tab$id_item[i]
  data[i, "pays"]<- tab$pays[i]
  data[i,"num_stimulus"]<- tab$num_stimulus[i]
  print(i)
  for (j in 1: nrow(data2)){
    if (tab$id_tester[i]== data2$id_tester[j] && tab$id_item[i] == data2$id_item[j]) {
      
      if(str_contains(data2$Nom_zone[j], "feculent")){
        data[i, "feculents_tps"] <- data[i, "feculents_tps"]+ data2$tps_fix[j]
        data[i, "feculents_fix"] <- data[i, "feculents_fix"]+ data2$nb_fix[j]
      }
    }
  }
  
  for (j in 1: nrow(data2)){
    if (tab$id_tester[i]== data2$id_tester[j] && tab$id_item[i] == data2$id_item[j]) {
      
      if(str_contains(data2$Nom_zone[j], "fruits")){
        data[i, "fruits_tps"] <- data[i, "fruits_tps"]+ data2$tps_fix[j]
        data[i, "fruits_fix"] <- data[i, "fruits_fix"]+ data2$nb_fix[j]
      }
    }
  }
  
  for (j in 1: nrow(data2)){
    if (tab$id_tester[i]== data2$id_tester[j] && tab$id_item[i] == data2$id_item[j]) {
      
      if(str_contains(data2$Nom_zone[j], "legumes")){
        data[i, "legumes_tps"] <- data[i, "legumes_tps"]+ data2$tps_fix[j]
        data[i, "legumes_fix"] <- data[i, "legumes_fix"]+ data2$nb_fix[j]
      }
    }
  }
  
  for (j in 1: nrow(data2)){
    if (tab$id_tester[i]== data2$id_tester[j] && tab$id_item[i] == data2$id_item[j]) {
      
      if(str_contains(data2$Nom_zone[j], "proteines")){
        data[i, "proteines_tps"] <- data[i, "proteines_tps"]+ data2$tps_fix[j]
        data[i, "proteines_fix"] <- data[i, "proteines_fix"]+ data2$nb_fix[j]
      }
    }
  }
  for (j in 1: nrow(data2)){
    if (tab$id_tester[i]== data2$id_tester[j] && tab$id_item[i] == data2$id_item[j]) {
      
      if(str_contains(data2$Nom_zone[j], "produit")){
        data[i, "produits_laitiers_tps"] <- data[i, "produits_laitiers_tps"]+ data2$tps_fix[j]
        data[i, "produits_laitiers_fix"] <- data[i, "produits_laitiers_fix"]+ data2$nb_fix[j]
      }
    }
  }
}


data <- data %>% 
  mutate(tps_tot = legumes_tps + produits_laitiers_tps+proteines_tps+fruits_tps+feculents_tps) %>%
  mutate(pct_feculents_tps = (feculents_tps/tps_tot)*100) %>% 
  mutate(pct_proteines_tps = (proteines_tps/tps_tot)*100) %>% 
  mutate(pct_fruits_tps = (fruits_tps/tps_tot)*100) %>%
  mutate(pct_legumes_tps = (legumes_tps/tps_tot)*100) %>%
  mutate(pct_produits_laitiers_tps = (produits_laitiers_tps/tps_tot)*100) %>%
  mutate(max = pmax(legumes_tps,produits_laitiers_tps,proteines_tps,fruits_tps, feculents_tps)) %>% 
  mutate(cat_max_tps = ifelse(max == legumes_tps, "legumes", ifelse(max == produits_laitiers_tps, "produit_laitiers", ifelse(max == proteines_tps, "proteines", ifelse(max == fruits_tps, "fruits", "feculents"))))) %>% 
  mutate(fix_tot = legumes_fix + produits_laitiers_fix+proteines_fix+fruits_fix+feculents_fix) %>%
  mutate(pct_feculents_fix = (feculents_fix/fix_tot)*100) %>% 
  mutate(pct_proteines_fix = (proteines_fix/fix_tot)*100) %>% 
  mutate(pct_fruits_fix = (fruits_fix/fix_tot)*100) %>%
  mutate(pct_legumes_fix = (legumes_fix/fix_tot)*100) %>%
  mutate(pct_produits_laitiers_fix = (produits_laitiers_fix/fix_tot)*100) %>%
  mutate(max = pmax(legumes_fix,produits_laitiers_fix,proteines_fix,fruits_fix, feculents_fix)) %>% 
  mutate(cat_max_fix = ifelse(max == legumes_fix, "legumes", ifelse(max == produits_laitiers_fix, "produit_laitiers", ifelse(max == proteines_fix, "proteines", ifelse(max == fruits_fix, "fruits", "feculents")))))


write.table(data, "data/data_finale2.csv", row.names=FALSE, sep=",",dec=".", na=" ")
