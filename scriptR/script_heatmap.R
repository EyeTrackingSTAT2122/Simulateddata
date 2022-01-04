library(tidyverse)
library(ggplot2)
library(tidyr)
library(jpeg)
library(ggpubr)

test_heat_map <- read.csv("~/Agro/5A/Projet Ingé/data_nutri.csv", sep=";")
classes <- read.csv("~/Agro/5A/Projet Ingé/stat_classes.csv", sep=";")

#Boucle pour séparer les blocs

i=1
liste1 =vector()
while (i<dim(test_heat_map)){
  if (!is.na(test_heat_map[i,2])){
    liste1 = c(liste1, i)
  }
  i= i+1
}
View(test_heat_map)
dimnames(test_heat_map)

#Boucle qui marchent pas trop

while (i < 10){
  data <- test_heat_map[liste1[i]+1:liste1[i+1]-1,] %>%
    select(test_item_display_order,gaze_x_percents,gaze_y_percents) %>% 
    rename(y = gaze_x_percents) %>% 
    rename(x = test_item_display_order) %>% 
    rename(time = gaze_y_percents)
  heatmap <- ggplot(data, aes(x=x, y=y)) +
    theme_void()+
    stat_density_2d(aes(fill = ..density..), 
                    geom = "raster", 
                    contour = FALSE, alpha = 0.8) +
    
    scale_fill_distiller(palette= "Spectral", direction=-1)
  print(heatmap)
  i=i+1
}

#Code hors boucle

i=1
data <- test_heat_map[(liste1[i]+1):(liste1[i+1]-1),] %>%
  select(test_item_display_order,gaze_x_percents,gaze_y_percents) %>% 
  rename(y = gaze_x_percents) %>% 
  rename(x = test_item_display_order) %>% 
  rename(time = gaze_y_percents)

img <- readJPEG("~/Agro/5A/Projet Ingé/Simulateddata/Plateaux_monde/1. Brazil.jpg")

heatmap <- ggplot(data, aes(x=x, y=y)) +
  background_image(img) +
  # theme_void()+
  stat_density_2d(aes(fill = ..density..), 
                  geom = "raster", 
                  contour = FALSE, alpha = 0.4) +
  
  scale_fill_distiller(palette= "Spectral", direction=-1)
heatmap

data_join <- test_heat_map %>% 
  full_join(data_classes, by = c("tester_id","tes")) %>% 
  select(tester_id, stimulus, aoi_name, starts_with("aoi_fixation"), classe)
