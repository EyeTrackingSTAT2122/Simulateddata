coord_split <- read.csv("~/Agro/5A/Projet Ingé/Simulateddata/test_AOI/AOI_test_brutes.csv", sep=",")

#Séparation des blocs : un bloc correspondant à un ensemble de coordonées de point. 
#Un bloc correspond à un stimulus vu par une personne

i=1
liste1 =vector()
while (i<dim(coord_split)){
  if (!is.na(coord_split[i,2])){
    liste1 = c(liste1, i)
  }
  i= i+1
}
liste1 = c(liste1,dim(coord_split[1]))

#récupération d'un bloc. i correspond à l'emplacement du bloc. par exemple, i=2 va nous donner le 2ème bloc.

i=1
width<- coord_split[liste1[i], "test_browser_width_px"]
height<- coord_split[liste1[i], "test_browser_height_px"]
data <- coord_split[(liste1[i]+1):(liste1[i+1]-1),] %>%
  select(test_item_display_order,gaze_x_percents,gaze_y_percents) %>% 
  rename(y = gaze_x_percents) %>% 
  rename(x = test_item_display_order) %>% 
  rename(time = gaze_y_percents)
View(coord_split)

#coordonnées des aires d'intérêts en px :

#Zone 1:Bleu
z1x1 = 0
z1x2 = 840
z1y1 = 0
z1y2 = 464

#Zone 2 : Jaune:
z2x1= 840
z2x2= 1680
z2y1= 464
z2y2= 926

#Zone 3 : Orange:
z3x1= 840
z3x2= 1680
z3y1= 0
z3y2= 464

#Zone 4 : Vert:
z4x1= 0
z4x2= 840
z4y1= 464
z4y2= 926

#Création du data frame qui va accueillir les données des zones
Ordre = c(1:1000)
zone1 = rep(0, 1000)
tps_zone1 = rep(0, 1000)
zone2 = rep(0, 1000)
tps_zone2 = rep(0, 1000)
zone3 = rep(0, 1000)
tps_zone3 = rep(0, 1000)
zone4 = rep(0, 1000)
tps_zone4 = rep(0, 1000)
hors_zone = rep(0, 1000)
tps_hors_zone = rep(0, 1000)

data_zone <- data.frame(Ordre, zone1, tps_zone1, zone2, tps_zone2, zone3, tps_zone3, zone4, tps_zone4, hors_zone, tps_hors_zone)

#Boucle pour analyser les zones et les temps de fixation par zone
lg_data <- dim(data)
x = (data[,1]/100)*width
y = (data [,2]/100)*height
time = data[,3]
i = 1
j=1
while (i <= lg_data[1]){
  if (x[i]>=z1x1 && x[i]<=z1x2 && y[i]>=z1y1 && y[i]<= z1y2){
    t1 = time[i]
    while(x[i]>=z1x1 && x[i]<=z1x2 && y[i]>=z1y1 && y[i]<= z1y2){
      print(i)
      print(j)
      i = i +1
      t2 = time[i]
    }
    data_zone[j,"zone1"]<- 1
    data_zone[j,"tps_zone1"]<- t2-t1
  }
  
  else if (x[i]>=z2x1 && x[i]<=z2x2 && y[i]>=z2y1 && y[i]<=z2y2){
    t1 = time[i]
    while(x[i]>=z2x1 && x[i]<=z2x2 && y[i]>=z2y1 && y[i]<=z2y2){
      print(i)
      print(j)
      i =i +1
      t2 = time[i]
    }
    data_zone[j,"zone2"]<- 1
    data_zone[j,"tps_zone2"]<- t2-t1
  }
  
  else if (x[i]>=z3x1 && x[i]<=z3x2 && y[i]>=z3y1 && y[i]<=z3y2){
    t1 = time[i]
    while(x[i]>=z3x1 && x[i]<=z3x2 && y[i]>=z3y1 && y[i]<=z3y2){
      print(i)
      print(j)
      i =i +1
      t2 = time[i]
    }
    data_zone[j,"zone3"]<- 1
    data_zone[j,"tps_zone3"]<- t2-t1
  }
  
  else if (x[i]>=z4x1 && x[i]<=z4x2 && y[i]>=z4y1 && y[i]<= z4y2){
    t1 = time[i]
    while(x[i]>=z4x1 && x[i]<=z4x2 && y[i]>=z4y1 && y[i]<= z4y2){
      print(i)
      print(j)
      i =i +1
      t2 = time[i]
    }
    data_zone[j,"zone4"]<- 1
    data_zone[j,"tps_zone4"]<- t2-t1
  }
  else{
    t1 = time[i]
    
    while(x[i]>1680 | x[i]<0 | y[i]> 926 | y[i]< 0){
      print(i)
      print(j)
      i =i +1
      t2 = time[i]
    }
    data_zone[j,"hors_zone"]<- 1
    data_zone[j,"tps_hors_zone"]<- t2-t1
  }
  j= j+1
}

View(data_zone)