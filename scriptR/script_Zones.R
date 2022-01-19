library(readr)
library(sjmisc)
library(tidyverse)
library(tidyr)
library(dplyr)
library(raster)

coord_split1 <- read.csv("~/Agro/5A/Projet Ingé/Simulateddata/data/Real_eye/data_finale.csv", sep=",")
coord_split2 <- read.csv("~/Agro/5A/Projet Ingé/Simulateddata/data/Real_eye/data_sauvetage.csv", sep=",")
Coord_Zone <-  read_delim("~/Agro/5A/Projet Ingé/Simulateddata/data/Coord_zones.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

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
  
#SÃ©paration des blocs : un bloc correspondant Ã  un ensemble de coordonÃ©es de point. 
#Un bloc correspond Ã  un stimulus vu par une personne

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

id_tester <- "199d1269-1d8a-420a-a986-1f4f2ab4355f"
id_item <- "34359006-f70f-438e-85d0-420cf2fe6944"

calcul_temps<- function (id_item, id_tester){
  #RÃ©cuperer le i (num_test) de coord_split correspondant au couple item/tester
  
  
  for (i in 1:nrow(split)){
    if (id_item == split$item_id[i] && id_tester == split$tester_id[i]){
      num1 <- split$num1[i]
      num2 <- split$num2[i]
    }
  }
  
  
  dta <- coord_split[(num1+1):(num2-1),] %>%
    select(gaze_x_percents,gaze_y_percents, gaze_timestamp_ms) %>% 
    rename(x = gaze_x_percents) %>% 
    rename(time = gaze_timestamp_ms) %>% 
    rename(y = gaze_y_percents)
  
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
  zone6 = rep(0, 0)
  tps_zone6 = rep(0, 0)
  
  
  data_zone <- data.frame(Ordre, zone1, tps_zone1, zone2, tps_zone2, zone3, tps_zone3, zone4, tps_zone4, zone5, tps_zone5,zone6, tps_zone6)
  
  lg_data <- dim(dta)[1]
  x = dta[,1]
  y = dta [,2]
  time = dta[,3]

  
  i=1
  j=1
  while (i < lg_data-1){
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
    select(starts_with("tps")) %>% 
    t()
  fixation_total_time = apply(fixation_time_aoi,1, sum)
  return(fixation_total_time)
  
}


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
tps_tot <- rep (0,0)
tab <- data.frame(id_tester,id_item, pays, num_stimulus,Temps_Z1,Temps_Z2,Temps_Z3,Temps_Z4,Temps_Z5, tps_tot)

i= 1
for (i in 1 : nrow(split)){
  fixation <- calcul_temps(split$item_id[i], split$tester_id[i])
  tab[i,"id_tester"] <- split$tester_id[i]
  tab[i,"id_item"] <- split$item_id[i]
  tab[i,"pays"] <- split$pays[i]
  tab[i, "num_stimulus"]<- split$num_stimulus[i]
  tab[i,"Temps_Z1"] <- fixation[1]
  tab[i,"Temps_Z2"] <- fixation[2]
  tab[i,"Temps_Z3"] <- fixation[3]
  tab[i,"Temps_Z4"] <- fixation[4]
  tab[i,"Temps_Z5"] <- fixation[5]
  tab[i, "tps_tot"]<- sum(fixation[1],fixation[2],fixation[3],fixation[4],fixation[5])
  print(i)
}

tab

write.table(tab, "~/Agro/5A/Projet Ingé/Simulateddata/data/data_fixation_fin.csv", row.names=FALSE, sep=",",dec=".", na=" ")

#En fonction de ce qu'il y a dans les zones 1 à 6, on va affecter des temps de fixation à nos dimension féculents, proteines, fruits et légumes
rp<-nrow(tab)
id_tester <- rep (0,rp)
id_item <- rep (0,rp)
pays <- rep (0,rp)
num_stimulus<-rep (0,rp)
feculents <- rep (0,rp)
proteines <- rep (0,rp)
fruits <- rep (0,rp)
legumes <- rep (0,rp)
produits_laitiers <- rep (0,rp)
tps_tot <- rep (0,rp)

data <- data.frame(id_tester, id_item, pays, num_stimulus, feculents, proteines, fruits, legumes, produits_laitiers, tps_tot)

i=1
k = 1
j =5

Coord_Zone2<- Coord_Zone %>% 
  select(id_item, Num_zone, Nom_zone)

data2<- tab %>% 
  rename("1" = Temps_Z1) %>% 
  rename("2" = Temps_Z2) %>% 
  rename("3" = Temps_Z3) %>% 
  rename("4" = Temps_Z4) %>% 
  rename("5" = Temps_Z5) %>% 
  pivot_longer(cols = 5:9, names_to = "Num_zone") %>%
  mutate(Num_zone = as.numeric(Num_zone)) %>% 
  full_join(Coord_Zone2, by = c("id_item", "Num_zone"))

i = 1
j = 2

for (i in 1:nrow(tab)){
  data[i,"id_tester"] <- tab$id_tester[i]
  data[i,"id_item"] <- tab$id_item[i]
  data[i, "pays"]<- tab$pays[i]
  data[i,"num_stimulus"]<- tab$num_stimulus[i]
  for (j in 1: nrow(data2)){
    if (tab$id_tester[i]== data2$id_tester[j] && tab$id_item[i] == data2$id_item[j]) {
      
      if(str_contains(data2$Nom_zone[j], "feculent")){
        data[i, "feculents"] <- data[i, "feculents"]+ data2$value[j]
      }
    }
  }
  
  for (j in 1: nrow(data2)){
    if (tab$id_tester[i]== data2$id_tester[j] && tab$id_item[i] == data2$id_item[j]) {
      
      if(str_contains(data2$Nom_zone[j], "fruits")){
        data[i, "fruits"] <- data[i, "fruits"]+ data2$value[j]
      }
    }
  }
  
  for (j in 1: nrow(data2)){
    if (tab$id_tester[i]== data2$id_tester[j] && tab$id_item[i] == data2$id_item[j]) {
      
      if(str_contains(data2$Nom_zone[j], "legumes")){
        data[i, "legumes"] <- data[i, "legumes"]+ data2$value[j]
      }
    }
  }
  
  for (j in 1: nrow(data2)){
    if (tab$id_tester[i]== data2$id_tester[j] && tab$id_item[i] == data2$id_item[j]) {
      
      if(str_contains(data2$Nom_zone[j], "proteines")){
        data[i, "proteines"] <- data[i, "proteines"]+ data2$value[j]
      }
    }
  }
  for (j in 1: nrow(data2)){
    if (tab$id_tester[i]== data2$id_tester[j] && tab$id_item[i] == data2$id_item[j]) {
      
      if(str_contains(data2$Nom_zone[j], "produit")){
        data[i, "produits_laitiers"] <- data[i, "produits_laitiers"]+ data2$value[j]
      }
    }
  }
}

data3 <- read.csv("~/Agro/5A/Projet Ingé/Simulateddata/data/data_dim_pct.csv", sep=",")

data <- data %>% 
  mutate(tps_tot = legumes + produits_laitiers+proteines+fruits+feculents) %>%
  mutate(pct_feculents = (feculents/tps_tot)*100) %>% 
  mutate(pct_proteines = (proteines/tps_tot)*100) %>% 
  mutate(pct_fruits = (fruits/tps_tot)*100) %>%
  mutate(pct_legumes = (legumes/tps_tot)*100) %>%
  mutate(pct_produits_laitiers = (produits_laitiers/tps_tot)*100) %>%
  mutate(max = pmax(legumes,produits_laitiers,proteines,fruits, feculents)) %>% 
  mutate(cat_max = ifelse(max == legumes, "legumes", ifelse(max == produits_laitiers, "produit_laitiers", ifelse(max == proteines, "proteines", ifelse(max == fruits, "fruits", "feculents")))))


write.table(data, "~/Agro/5A/Projet Ingé/Simulateddata/data/data_dim_pct_fin.csv", row.names=FALSE, sep=",",dec=".", na=" ")
