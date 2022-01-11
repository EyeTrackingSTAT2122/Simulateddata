coord_split <- read.csv("~/Agro/5A/Projet Ingé/Simulateddata/test_AOI/zone_test.csv", sep=",")

#Séparation des blocs : un bloc correspondant à un ensemble de coordonées de point. 
#Un bloc correspond à un stimulus vu par une personne

i=1
liste1 =vector()
while (i<dim(coord_split)[1]){
  if (!is.na(coord_split[i,2])){
    liste1 = c(liste1, i)
  }
  i= i+1
}
liste1 = c(liste1,dim(coord_split[1]))


calcul_temps<- function (num_test, id_item, id_tester){
  num_test= num_test
  data <- coord_split[(liste1[num_test]+1):(liste1[num_test+1]-1),] %>%
    select(test_item_display_order,gaze_x_percents,gaze_y_percents) %>% 
    rename(y = gaze_x_percents) %>% 
    rename(x = test_item_display_order) %>% 
    rename(time = gaze_y_percents)
  
  #Création du data frame qui va accueillir les données des zones
  Ordre = rep(0,0)
  zone1 = rep(0, 0)
  tps_zone1 = rep(0, 0)
  zone2 = rep(0, 0)
  tps_zone2 = rep(0, 0)
  zone3 = rep(0, 0)
  tps_zone3 = rep(0, 0)
  zone4 = rep(0, 0)
  tps_zone4 = rep(0, 0)
  hors_zone = rep(0, 0)
  tps_hors_zone = rep(0,0)
  
  data_zone <- data.frame(Ordre, zone1, tps_zone1, zone2, tps_zone2, zone3, tps_zone3, zone4, tps_zone4, hors_zone, tps_hors_zone)
  
  lg_data <- dim(data)[1]
  x = data[,1]
  y = data [,2]
  time = data[,3]
  #x[lg_data + 1]<- 0
  #y[lg_data + 1]<- 0
  i = 1
  j=1
  while (i < (lg_data)){
    if (x[i]>= 0 && x[i]<= 50 && y[i]>= 0 && y[i]<= 50){
      t1 = time[i]
      while(x[i]>= 0 && x[i]<= 50 && y[i]>= 0 && y[i]<= 50 && i < (lg_data)){
        i= i+1
      }
      i = i+1
      t2 = time[i]
      data_zone[j, "Ordre"] <- j
      data_zone[j,"zone1"]<- 1
      data_zone[j,"tps_zone1"]<- t2-t1
    }
    
    else if (x[i]>=50 && x[i]<=100 && y[i]>=0 && y[i]<=50){
      t1 = time[i]
      while(x[i]>=50 && x[i]<=100 && y[i]>=0 && y[i]<=50 && i < (lg_data)){
        i = i+1
      }
      t2 = time[i]
      i = i +1
      data_zone[j, "Ordre"] <- j
      data_zone[j,"zone2"]<- 1
      data_zone[j,"tps_zone2"]<- t2-t1
    }
    
    else if (x[i]>=50 && x[i]<=100 && y[i]>=50 && y[i]<=100){
      t1 = time[i]
      while(x[i]>=50 && x[i]<=100 && y[i]>=50 && y[i]<=100 && i < (lg_data)){
        i = i +1
      }
      i = i + 1
      t2 = time[i]
      data_zone[j, "Ordre"] <- j
      data_zone[j,"zone3"]<- 1
      data_zone[j,"tps_zone3"]<- t2-t1
    }
    
    else if (x[i]>=0 && x[i]<=50 && y[i]>=50 && y[i]<= 100){
      t1 = time[i]
      while(x[i]>=0 && x[i]<=50 && y[i]>=50 && y[i]<= 100 && i < (lg_data)){
        i = i+1
      }
      i= i + 1
      t2 = time[i]
      data_zone[j, "Ordre"] <- j
      data_zone[j,"zone4"]<- 1
      data_zone[j,"tps_zone4"]<- t2-t1
    }
    else{
      if (i<lg_data){
        t1 = time[i]
        while((i < lg_data) && (x[i]>100 | x[i]<0 | y[i]> 100 | y[i]< 0)){
          i=i+1
        }
      }
      i = i + 1
      t2 = time[i]
      data_zone[j, "Ordre"] <- j
      data_zone[j,"hors_zone"]<- 1
      data_zone[j,"tps_hors_zone"]<- t2-t1
    }
    j= j+1
  }
  
  data_zone[is.na(data_zone)] <- 0
  fixation_time_aoi <- data_zone %>% 
    select(starts_with("tps")) %>% 
    t() %>%
    as.data.frame()
  
  fixation_total_time = apply(fixation_time_aoi,1, sum)
  return(fixation_total_time)
}


calcul_temps(1)
calcul_temps(2)
calcul_temps(3)
calcul_temps(4)
calcul_temps(5)
calcul_temps(6)
calcul_temps(7)
calcul_temps(8)
calcul_temps(9)
calcul_temps(10)
calcul_temps(11)
calcul_temps(12)
calcul_temps(13)
calcul_temps(14)
calcul_temps(15)
calcul_temps(16)




n_debut <- 1
n_fin <- 4

tableau <- function(n_debut, n_fin){
  tab <- data.frame()
  for (i in n_debut:n_fin){
    test <- i
    tab[i,1] <- i
    tps <- calcul_temps(i)
    tab[i,2] <- tps[1]
    tab[i,3] <- tps[2]
    tab[i,4] <- tps[3]
    tab[i,5] <- tps[4]
    tab[i,6] <- tps[5]
  }
  colnames(tab) <- c("Numero","Temps_Z1","Temps_Z2","Temps_Z3","Temps_Z4","Temps_HZ")
  tab<-tab %>%
    drop_na()
  return(tab)
}

test1 <- tableau(1,4)
test2 <- tableau (5,8)
test3 <- tableau (9,12)
test4 <- tableau (13,16)

  
