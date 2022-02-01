library(readr)
library(sjmisc)
library(tidyverse)
library(tidyr)
library(dplyr)

Coord_Zone <-  read_delim("data/data_support/Coord_zones.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
decalage <-  read_csv("data/data_support/coordonnes_decalages.csv")
coord_split <-  read_csv("data/data_support/coord_split.csv")
split <- read_csv("data/data_support/split_supp3.csv")

tester_id = "cc933e0b-091f-4ef4-bfc8-11f2076d4396"
item_id = "7bdf6ed6-acf9-4a7b-84fe-7e4f96f9582a"

calcul_temps<- function (item_id, tester_id){
  #Récuperer le i (num_test) de coord_split correspondant au couple item/tester
  
  
  for (i in 1:nrow(split)){
    if (item_id == split$item_id[i] && tester_id == split$tester_id[i]){
      num1 <- split$num1[i]
      num2 <- split$num2[i]
      num_stimulus <- split$num_stimulus[i]
      id <-split$id[i]
    }
  }

  for (i in 1:nrow(decalage)){
    if (item_id == decalage$id_item[i] && tester_id == decalage$id_tester[i]){
      var_x <- decalage$var_x[i]
      var_y <- decalage$var_y[i]
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
      if (item_id == Coord_Zone$id_item[i] & j== Coord_Zone$Num_zone[i] ){
        coord_zone[j,"num_zone"] <- j
        coord_zone[j,"x1"]<- Coord_Zone[i, "x1"]
        coord_zone[j,"x2"]<- Coord_Zone[i, "x2"]
        coord_zone[j,"y1"]<- Coord_Zone[i, "y1"]
        coord_zone[j,"y2"]<- Coord_Zone[i, "y2"]
      }
    }
  }
  
  
  id_item = rep(item_id, 5 )
  num = rep(num_stimulus, 5)
  id_tester = rep (tester_id, 5)
  id_form = rep(id, 5)
  zone = c(1,2,3,4,5)
  tps = rep(0, 5)
  nb_visites = rep(0, 5)
  
  data_zone <- data.frame(item_id, num, tester_id, id_form, zone, nb_visites, tps)
  
  lg_data <- dim(dta)[1]
  # x = dta[,1]
  # y = dta [,2]
  # time = dta[,3]
  
  
  i=1
  j=1
  while (i < (lg_data-1)){
    #Zone 1 :
    if (dta[i,1]>= coord_zone[1,"x1"] && dta[i,1]<= coord_zone[1,"x2"] && dta[i,2]>= coord_zone[1,"y1"] && dta[i,2]<= coord_zone[1,"y2"]){
      t1 = dta[i,3]
      while(dta[i,1]>= coord_zone[1,"x1"] && dta[i,1]<= coord_zone[1,"x2"] && dta[i,2]>= coord_zone[1,"y1"] && dta[i,2]<= coord_zone[1,"y2"] && i < (lg_data-1)){
        i= i+1
      }
      t2 = dta[i,3]
      data_zone[1, "nb_visites"] <- data_zone[1, "nb_visites"] + 1
      data_zone[1, "tps"]<- data_zone[1, "tps"] + (t2-t1)
    }
    
    #Zone 2 : 
    if (dta[i,1]>= coord_zone[2,"x1"] && dta[i,1]<= coord_zone[2,"x2"] && dta[i,2]>= coord_zone[2,"y1"] && dta[i,2]<= coord_zone[2,"y2"]){
      t1 = dta[i,3]
      while(dta[i,1]>= coord_zone[2,"x1"] && dta[i,1]<= coord_zone[2,"x2"] && dta[i,2]>= coord_zone[2,"y1"] && dta[i,2]<= coord_zone[2,"y2"] && i < (lg_data-1)){
        i = i+1
      }
      t2 = dta[i,3]
      data_zone[2, "nb_visites"] <- data_zone[2, "nb_visites"] + 1
      data_zone[2, "tps"]<- data_zone[2, "tps"] + (t2-t1)
    }
    
    #Zone 3 :
    if (dta[i,1]>= coord_zone[3,"x1"] && dta[i,1]<= coord_zone[3,"x2"] && dta[i,2]>= coord_zone[3,"y1"] && dta[i,2]<= coord_zone[3,"y2"]){
      t1 = dta[i,3]
      while(dta[i,1]>= coord_zone[3,"x1"] && dta[i,1]<= coord_zone[3,"x2"] && dta[i,2]>= coord_zone[3,"y1"] && dta[i,2]<= coord_zone[3,"y2"] && i < (lg_data-1)){
        i = i+1
      }
      t2 = dta[i,3]
      data_zone[3, "nb_visites"] <- data_zone[3, "nb_visites"] + 1
      data_zone[3, "tps"]<- data_zone[3, "tps"] + (t2-t1)
    }
    #Zone 4 :
    if (dta[i,1]>= coord_zone[4,"x1"] && dta[i,1]<= coord_zone[4,"x2"] && dta[i,2]>= coord_zone[4,"y1"] && dta[i,2]<= coord_zone[4,"y2"]){
      t1 = dta[i,3]
      while(dta[i,1]>= coord_zone[4,"x1"] && dta[i,1]<= coord_zone[4,"x2"] && dta[i,2]>= coord_zone[4,"y1"] && dta[i,2]<= coord_zone[4,"y2"] && i < (lg_data-1)){
        i = i+1
      }
      t2 = dta[i,3]
      data_zone[4, "nb_visites"] <- data_zone[4, "nb_visites"] + 1
      data_zone[4, "tps"]<- data_zone[4, "tps"] + (t2-t1)
    }
    
    #Zone 5 :
    if (dta[i,1]>= coord_zone[5,"x1"] && dta[i,1]<= coord_zone[5,"x2"] && dta[i,2]>= coord_zone[5,"y1"] && dta[i,2]<= coord_zone[5,"y2"]){
      t1 = dta[i,3]
      while(dta[i,1]>= coord_zone[5,"x1"] && dta[i,1]<= coord_zone[5,"x2"] && dta[i,2]>= coord_zone[5,"y1"] && dta[i,2]<= coord_zone[5,"y2"] && i < (lg_data-1)){
        i = i+1
      }
      t2 = dta[i,3]
      data_zone[5, "nb_visites"] <- data_zone[5, "nb_visites"] + 1
      data_zone[5, "tps"]<- data_zone[5, "tps"] + (t2-t1)
    }
    
    else{
           i = i+1   
      }
    j= j+1
  }
  
  data_zone[is.na(data_zone)] <- 0
  return(data_zone)
  
}

calcul_temps(item_id, tester_id)

#On fait tourner la fonction de calcul de temps de fixation sur l'ensemble des couple item/tester (= un individu stat)
#On met les resultats dans un tableau qui va regrouper tous les resultats de tous les individus

tab <- data.frame()

for (i in 1 : nrow(split)){
  fixation <- calcul_temps(split$item_id[i], split$tester_id[i])
  tab <- rbind(tab, fixation)
  print(i)
}

write.table(tab, "data/data_fix_finales/fixations_5.csv", row.names=FALSE, sep=",",dec=".", na=" ")

