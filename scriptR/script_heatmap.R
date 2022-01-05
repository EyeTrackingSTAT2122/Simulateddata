library(tidyverse)
library(ggplot2)
library(tidyr)
library(png)
library(ggpubr)

test_heat_map <- read.csv("study/aoi_4.csv", sep=";")
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
  heatmap
  i=i+1
}

#Code hors boucle

i=1
data <- test_heat_map[(liste1[i]+1):(liste1[i+1]-1),] %>%
  select(test_item_display_order,gaze_x_percents,gaze_y_percents) %>% 
  rename(y = gaze_x_percents) %>% 
  rename(x = test_item_display_order) %>% 
  rename(time = gaze_y_percents)

img <- readPNG("img.png")

heatmap <- ggplot(data, aes(x=x, y=-y)) +
  background_image(img) +
  theme_void()+
  stat_density_2d(aes(fill = ..density..*10e04, alpha = ..density..*10e05, guide = "none"), 
                  geom = "raster", 
                  contour = FALSE) +
  coord_fixed(xlim = c(0,100),ylim = c(-100,0))+
  scale_fill_gradient2(low = "blue", mid = "yellow", high = "red", guide = "none") +
  theme(legend.title = element_blank()) +
  theme(legend.position='none')
  # scale_x_continuous(limits=c(0,100))+
  # scale_y_continuous(limits=c(-100, 0))
heatmap





data_join <- test_heat_map %>% 
  full_join(data_classes, by = c("tester_id","tes")) %>% 
  select(tester_id, stimulus, aoi_name, starts_with("aoi_fixation"), classe)
