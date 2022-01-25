library(readr)
library(sjmisc)
library(tidyverse)
library(tidyr)
library(dplyr)
library(raster)

coord_split1 <- read.csv("data/Real_eye/data_finale.csv", sep=",")
coord_split2 <- read.csv("data/Real_eye/data_sauvetage.csv", sep=",")
Coord_Zone <-  read_delim("data/Coord_zones.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
decalage <-  read_csv("data/coord_decalages.csv")

for (i in 1: nrow(coord_split2)){
  if (coord_split2$item_id[i] ==  "6bfa3bbf-ff8f-4719-ade6-08308e2edfe8"){
    coord_split2$item_id[i]<- "34359006-f70f-438e-85d0-420cf2fe6944"
  }
  else if (coord_split2$item_id[i] ==  "2cce5dbf-5148-49e7-94fc-5b8f962a6aff"){
    coord_split2$item_id[i]<- "20f3c152-6ebf-49db-8e1a-00eebd99feb6"
  }
  else if (coord_split2$item_id[i] ==  "84c7b3b8-af92-4987-a4ed-a4e9075e2855"){
    coord_split2$item_id[i]<- "213479f7-39af-4b83-b0e9-4ba473d27429"
  }
  else if (coord_split2$item_id[i] ==  "6f741a6e-b15c-48da-b14d-89aa117da85f"){
    coord_split2$item_id[i]<- "7bdf6ed6-acf9-4a7b-84fe-7e4f96f9582a"
  }
  else if (coord_split2$item_id[i] ==  "37df9db8-beab-4ebe-8538-add6525037e6"){
    coord_split2$item_id[i]<- "cc2a8b56-732c-4e4c-a058-248783b68587"
  }
  else if (coord_split2$item_id[i] ==  "e74916b7-bce5-4833-baae-c57998764f8b"){
    coord_split2$item_id[i]<- "dd13fcba-0ae1-43d3-9d32-1672ed84f151"
  }
  else if (coord_split2$item_id[i] ==  "e88439ca-3bc1-490c-aa6d-93da2d85b083"){
    coord_split2$item_id[i]<- "6afda059-04a5-4308-b776-a532ed142284"
  }
  else if (coord_split2$item_id[i] ==  "1652d0e1-27ce-4743-9313-b303c97ad7c2"){
    coord_split2$item_id[i]<- "f61f9030-d814-495d-af95-0e643697de64"
  }
  else if (coord_split2$item_id[i] ==  "0ca872a3-4c92-4386-8cb2-ebf530c1950e"){
    coord_split2$item_id[i]<- "5445f170-12c1-48fc-9563-3709dd2993b0"
  }
  else if (coord_split2$item_id[i] ==  "35a479cb-716e-4da9-8ff9-2af4c7de6c12"){
    coord_split2$item_id[i]<- "fab315cc-9769-4e2b-bef7-ed998d3754ce"
  }

}


coord_split <- rbind(coord_split1,coord_split2)
  
#Séparation des blocs : un bloc correspondant à un ensemble de coordonées de point. 
#Un bloc correspond à un stimulus vu par une personne

num_test <- rep(0, 0)
tester_id <- rep(0, 0)
item_id <- rep (0, 0)
bloc <- data.frame(num_test, tester_id, item_id)

j =1
for (i in 1:nrow(coord_split)){
  if (!is.na(coord_split[i,"tester_quality_grade"])){
      bloc[j,"num_test"] <- i
      bloc[j,"tester_id"] <- coord_split[i,"tester_id"]
      bloc[j,"item_id"] <- coord_split[i,"item_id"]
      j = j+1
  }
  print(i)
}

num1<- rep(0, 0)
num2 <- rep(0,0)
tester_id <- rep(0, 0)
item_id <- rep (0, 0)
split <- data.frame(num1, num2, tester_id, item_id)

j=1
for (i in 1:(nrow(bloc))){
  if (bloc$item_id[i] != "fab315cc-9769-4e2b-bef7-ed998d3754ce"){
    split[j,"num1"] <- bloc$num_test[i]
    split[j,"num2"] <- bloc$num_test[i+1]
    split[j,"tester_id"] <- bloc$tester_id[i]
    split[j,"item_id"] <- bloc$item_id[i]
    j = j+1
  }
}
split[nrow(split),"num2"] = nrow(coord_split)



split[,"pays"]<- rep(0,nrow(split))
split[,"num_stimulus"]<- rep(0,nrow(split))
i=1
for (i in 1:nrow(split)){
  if(split$item_id[i] == "34359006-f70f-438e-85d0-420cf2fe6944"){
    split[i, "pays"] <- "Ukraine"
    split[i,"num_stimulus"] <- 156
  }
  else if(split$item_id[i] == "213479f7-39af-4b83-b0e9-4ba473d27429"){
    split[i, "pays"] <- "Spain"
    split[i,"num_stimulus"] <- 756
  }
  else if(split$item_id[i] == "7bdf6ed6-acf9-4a7b-84fe-7e4f96f9582a"){
    split[i, "pays"] <- "France"
    split[i,"num_stimulus"] <- 421
  }
  else if(split$item_id[i] == "cc2a8b56-732c-4e4c-a058-248783b68587"){
    split[i, "pays"] <- "South Korea"
    split[i,"num_stimulus"] <- 489
  }
  else if(split$item_id[i] == "dd13fcba-0ae1-43d3-9d32-1672ed84f151"){
    split[i, "pays"] <- "USA"
    split[i,"num_stimulus"] <- 327
  }
  else if(split$item_id[i] == "6afda059-04a5-4308-b776-a532ed142284"){
    split[i, "pays"] <- "Italy"
    split[i,"num_stimulus"] <- 238
  }
  else if(split$item_id[i] == "f61f9030-d814-495d-af95-0e643697de64"){
    split[i, "pays"] <- "Brazil"
    split[i,"num_stimulus"] <- 352
  }
  else if(split$item_id[i] == "5445f170-12c1-48fc-9563-3709dd2993b0"){
    split[i, "pays"] <- "Greece"
    split[i,"num_stimulus"] <- 980
  }
  else if(split$item_id[i] == "20f3c152-6ebf-49db-8e1a-00eebd99feb6"){
    split[i, "pays"] <- "Finland"
    split[i,"num_stimulus"] <- 672
  }
}

id_tester <- "131f7620-d15d-4560-86ab-564145745b9b"
id_item <- "34359006-f70f-438e-85d0-420cf2fe6944"
i=1
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
