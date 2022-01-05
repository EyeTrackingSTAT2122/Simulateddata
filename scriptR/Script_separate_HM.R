library(tidyverse)
library(ggplot2)
library(tidyr)
library(jpeg)
library(ggpubr)


#### Définition ----

survey <- read_csv("study/survey.csv")

res <- read.csv("study/split.csv")

img_Brazil <- readJPEG("Plateaux_monde/1_Brazil.jpg")

img_Finland <- readJPEG("Plateaux_monde/2_Finland.jpg")

img_France <- readJPEG("Plateaux_monde/3_France.jpg")

img_Greece <- readJPEG("Plateaux_monde/4_Greece.jpg")

img_Italy <- readJPEG("Plateaux_monde/5_Italy.jpg")

img_Spain <- readJPEG("Plateaux_monde/6_Spain.jpg")

img_South_Korea <- readJPEG("Plateaux_monde/7_South Korea.jpg")

img_Ukraine <- readJPEG("Plateaux_monde/8_Ukraine.jpg")

img_USA <- readJPEG("Plateaux_monde/9_USA.jpg")


Brazil <- "adbb04fc-43bd-40fc-863b-0f8f2a8f972d"

Finland <- "4ca78282-cbb6-4b2c-9aaa-3e04ad552a10"

France <-"debe005d-261b-4e5c-978d-b19fa197105f"

Greece <- "3a090752-22ae-4508-afb2-e82dfbad9e3b"

Italy <- "c90150d0-c09f-4777-9ab0-968dda180141"

Spain <- "55fcdff2-f6dc-4699-9e38-4ae5ae4ec7c3"

South_Korea <- "7f2095d0-5950-4e65-b992-109de9f39dbb"

Ukraine <- "5ee44e4f-0eb1-4fa1-b8f9-ea942660dd2a"

USA <- "210f7889-a059-414b-819d-4fdfd854b606"

#### Code ----

# Code pour join

Classe <- rep(NA,nrow(res))

res <- cbind(res,Classe)



for (i in 1:length(liste1)){
  for (j in 1:nrow(survey)){
    if (res$tester_id[liste1[i]] == survey$tester_id[j]) {
      if(res$item_id[liste1[i]]==Brazil){
        res$Classe[liste1[i]] <- survey$`D'après toi, ce plateau est-il équilibré ? - 1. Brazil.jpg`[j]
      }
      else if(res$item_id[liste1[i]]==Finland){
        res$Classe[liste1[i]] <- survey$`D'après toi, ce plateau est-il équilibré ? - 2. Finland.jpg`[j]
      }
      else if(res$item_id[liste1[i]]==France){
        res$Classe[liste1[i]] <- survey$`D'après toi, ce plateau est-il équilibré ? - 3. France.jpg`[j]
      }
      else if(res$item_id[liste1[i]]==Greece){
        res$Classe[liste1[i]] <- survey$`D'après toi, ce plateau est-il équilibré ? - 4. Greece.jpg`[j]
      }
      else if(res$item_id[liste1[i]]==Italy){
        res$Classe[liste1[i]] <- survey$`D'après toi, ce plateau est-il équilibré ? - 5. Italy.jpg`[j]
      }
      else if(res$item_id[liste1[i]]==Spain){
        res$Classe[liste1[i]] <- survey$`D'après toi, ce plateau est-il équilibré ? - 6. Spain.jpg`[j]
      }
      else if(res$item_id[liste1[i]]==South_Korea){
        res$Classe[liste1[i]] <- survey$`D'après toi, ce plateau est-il équilibré ? - 7. South Korea.jpg`[j]
      }
      else if(res$item_id[liste1[i]]==Ukraine){
        res$Classe[liste1[i]] <- survey$`D'après toi, ce plateau est-il équilibré ? - 8. Ukraine.jpg`[j]
      }
      else {
        res$Classe[liste1[i]] <- survey$`D'après toi, ce plateau est-il équilibré ? - 9. USA.jpg`[j]
      }
    } 
  }
}

for (i in 1:length(liste1)){
  if (res$Classe[liste1[i]] == "Il est équilibré"){
    res$Classe[liste1[i]] <- "Equilibre"
  }
  else if (res$Classe[liste1[i]] == "Il n'est pas équilibré"){
    res$Classe[liste1[i]] <- "Pas_Equilibre"
  }
  else {
    res$Classe[liste1[i]] <- "Presque_Equilibre"
  }
}

res$Classe <- as.factor(res$Classe)




#Boucle pour séparer les blocs

i=1
liste1 =vector()
while (i<dim(res)){
  if (!is.na(res[i,2])){
    liste1 = c(liste1, i)
  }
  i= i+1
}


for (i in 1:length(liste1)){
  if (i < length(liste1)){
    data <- res[(liste1[i]+1):(liste1[i+1]-1),] %>%
      select(test_item_display_order,gaze_x_percents,gaze_y_percents) %>% 
      rename(y = gaze_x_percents) %>% 
      rename(x = test_item_display_order) %>% 
      rename(time = gaze_y_percents)
  }
  else {
    data <- res[(liste1[i]+1):nrow(res),] %>%
      select(test_item_display_order,gaze_x_percents,gaze_y_percents) %>% 
      rename(y = gaze_x_percents) %>% 
      rename(x = test_item_display_order) %>% 
      rename(time = gaze_y_percents)
  }
  
  if (res$Classe[liste1[i]] == "Equilibre"){
    if (res$item_id[liste1[i]]==Brazil){
      img <- img_Brazil
      heatmap <- ggplot(data, aes(x=x, y=y)) +
        background_image(img) +
        # theme_void()+
        stat_density_2d(aes(fill = ..density..*10e5, alpha = 1), 
                        geom = "raster", 
                        contour = FALSE) +
        coord_fixed(xlim = c(0,100),ylim = c(100,0))+
        scale_fill_gradient2(low = "transparent", mid = "yellow", high = "red",midpoint = 40)
      # theme(legend.title = element_blank()) +
      # theme(legend.position='none')
      # scale_x_continuous(limits=c(0,100))+
      # scale_y_continuous(limits=c(-100, 0))
      png(file = paste0("study/Test/Equilibre/",res$tester_id[liste1[i]],"Brazil.png"))
      plot(heatmap)
      dev.off()
    }
    
    else if (res$item_id[liste1[i]] == Finland){
      img <- img_Finland
      heatmap <- ggplot(data, aes(x=x, y=y)) +
        background_image(img) +
        # theme_void()+
        stat_density_2d(aes(fill = ..density..*10e5, alpha = 1), 
                        geom = "raster", 
                        contour = FALSE) +
        coord_fixed(xlim = c(0,100),ylim = c(100,0))+
        scale_fill_gradient2(low = "transparent", mid = "yellow", high = "red",midpoint = 40)
      # theme(legend.title = element_blank()) +
      # theme(legend.position='none')
      # scale_x_continuous(limits=c(0,100))+
      # scale_y_continuous(limits=c(-100, 0))
      png(file = paste0("study/Test/Equilibre/",res$tester_id[liste1[i]],"Finland.png"))
      plot(heatmap)
      dev.off()
    }
    else if (res$item_id[liste1[i]] == France){
      img <- img_France
      heatmap <- ggplot(data, aes(x=x, y=y)) +
        background_image(img) +
        # theme_void()+
        stat_density_2d(aes(fill = ..density..*10e5, alpha = 1), 
                        geom = "raster", 
                        contour = FALSE) +
        coord_fixed(xlim = c(0,100),ylim = c(100,0))+
        scale_fill_gradient2(low = "transparent", mid = "yellow", high = "red",midpoint = 40)
      # theme(legend.title = element_blank()) +
      # theme(legend.position='none')
      # scale_x_continuous(limits=c(0,100))+
      # scale_y_continuous(limits=c(-100, 0))
      png(file = paste0("study/Test/Equilibre/",res$tester_id[liste1[i]],"France.png"))
      plot(heatmap)
      dev.off()
    }
    else if (res$item_id[liste1[i]] == Greece){
      img <- img_Greece
      heatmap <- ggplot(data, aes(x=x, y=y)) +
        background_image(img) +
        # theme_void()+
        stat_density_2d(aes(fill = ..density..*10e5, alpha = 1), 
                        geom = "raster", 
                        contour = FALSE) +
        coord_fixed(xlim = c(0,100),ylim = c(100,0))+
        scale_fill_gradient2(low = "transparent", mid = "yellow", high = "red",midpoint = 40)
      # theme(legend.title = element_blank()) +
      # theme(legend.position='none')
      # scale_x_continuous(limits=c(0,100))+
      # scale_y_continuous(limits=c(-100, 0))
      png(file = paste0("study/Test/Equilibre/",res$tester_id[liste1[i]],"Greece.png"))
      plot(heatmap)
      dev.off()
    }
    else if (res$item_id[liste1[i]] == Italy){
      img <- img_Italy
      heatmap <- ggplot(data, aes(x=x, y=y)) +
        background_image(img) +
        # theme_void()+
        stat_density_2d(aes(fill = ..density..*10e5, alpha = 1), 
                        geom = "raster", 
                        contour = FALSE) +
        coord_fixed(xlim = c(0,100),ylim = c(100,0))+
        scale_fill_gradient2(low = "transparent", mid = "yellow", high = "red",midpoint = 40)
      # theme(legend.title = element_blank()) +
      # theme(legend.position='none')
      # scale_x_continuous(limits=c(0,100))+
      # scale_y_continuous(limits=c(-100, 0))
      png(file = paste0("study/Test/Equilibre/",res$tester_id[liste1[i]],"Italy.png"))
      plot(heatmap)
      dev.off()
    }
    else if (res$item_id[liste1[i]] == South_Korea){
      img <- img_South_Korea
      heatmap <- ggplot(data, aes(x=x, y=y)) +
        background_image(img) +
        # theme_void()+
        stat_density_2d(aes(fill = ..density..*10e5, alpha = 1), 
                        geom = "raster", 
                        contour = FALSE) +
        coord_fixed(xlim = c(0,100),ylim = c(100,0))+
        scale_fill_gradient2(low = "transparent", mid = "yellow", high = "red",midpoint = 40)
      # theme(legend.title = element_blank()) +
      # theme(legend.position='none')
      # scale_x_continuous(limits=c(0,100))+
      # scale_y_continuous(limits=c(-100, 0))
      png(file = paste0("study/Test/Equilibre/",res$tester_id[liste1[i]],"South_Korea.png"))
      plot(heatmap)
      dev.off()
    }
    else if (res$item_id[liste1[i]] == Spain){
      img <- img_Spain
      heatmap <- ggplot(data, aes(x=x, y=y)) +
        background_image(img) +
        # theme_void()+
        stat_density_2d(aes(fill = ..density..*10e5, alpha = 1), 
                        geom = "raster", 
                        contour = FALSE) +
        coord_fixed(xlim = c(0,100),ylim = c(100,0))+
        scale_fill_gradient2(low = "transparent", mid = "yellow", high = "red",midpoint = 40)
      # theme(legend.title = element_blank()) +
      # theme(legend.position='none')
      # scale_x_continuous(limits=c(0,100))+
      # scale_y_continuous(limits=c(-100, 0))
      png(file = paste0("study/Test/Equilibre/",res$tester_id[liste1[i]],"Spain.png"))
      plot(heatmap)
      dev.off()
    }
    else if (res$item_id[liste1[i]] == Ukraine){
      img <- img_Ukraine
      heatmap <- ggplot(data, aes(x=x, y=y)) +
        background_image(img) +
        # theme_void()+
        stat_density_2d(aes(fill = ..density..*10e5, alpha = 1), 
                        geom = "raster", 
                        contour = FALSE) +
        coord_fixed(xlim = c(0,100),ylim = c(100,0))+
        scale_fill_gradient2(low = "transparent", mid = "yellow", high = "red",midpoint = 40)
      # theme(legend.title = element_blank()) +
      # theme(legend.position='none')
      # scale_x_continuous(limits=c(0,100))+
      # scale_y_continuous(limits=c(-100, 0))
      png(file = paste0("study/Test/Equilibre/",res$tester_id[liste1[i]],"Ukraine.png"))
      plot(heatmap)
      dev.off()
    }
    else {
      img <- img_USA
      heatmap <- ggplot(data, aes(x=x, y=y)) +
        background_image(img) +
        # theme_void()+
        stat_density_2d(aes(fill = ..density..*10e5, alpha = 1), 
                        geom = "raster", 
                        contour = FALSE) +
        coord_fixed(xlim = c(0,100),ylim = c(100,0))+
        scale_fill_gradient2(low = "transparent", mid = "yellow", high = "red",midpoint = 40)
      # theme(legend.title = element_blank()) +
      # theme(legend.position='none')
      # scale_x_continuous(limits=c(0,100))+
      # scale_y_continuous(limits=c(-100, 0))
      png(file = paste0("study/Test/Equilibre/",res$tester_id[liste1[i]],"USA.png"))
      plot(heatmap)
      dev.off()
    }
  }
  else if (res$Classe[liste1[i]] == "Presque_Equilibre"){
    if (res$item_id[liste1[i]]==Brazil){
      img <- img_Brazil
      heatmap <- ggplot(data, aes(x=x, y=y)) +
        background_image(img) +
        # theme_void()+
        stat_density_2d(aes(fill = ..density..*10e5, alpha = 1), 
                        geom = "raster", 
                        contour = FALSE) +
        coord_fixed(xlim = c(0,100),ylim = c(100,0))+
        scale_fill_gradient2(low = "transparent", mid = "yellow", high = "red",midpoint = 40)
      # theme(legend.title = element_blank()) +
      # theme(legend.position='none')
      # scale_x_continuous(limits=c(0,100))+
      # scale_y_continuous(limits=c(-100, 0))
      png(file = paste0("study/Test/Presque_equilibre/",res$tester_id[liste1[i]],"Brazil.png"))
      plot(heatmap)
      dev.off()
    }
    
    else if (res$item_id[liste1[i]] == Finland){
      img <- img_Finland
      heatmap <- ggplot(data, aes(x=x, y=y)) +
        background_image(img) +
        # theme_void()+
        stat_density_2d(aes(fill = ..density..*10e5, alpha = 1), 
                        geom = "raster", 
                        contour = FALSE) +
        coord_fixed(xlim = c(0,100),ylim = c(100,0))+
        scale_fill_gradient2(low = "transparent", mid = "yellow", high = "red",midpoint = 40)
      # theme(legend.title = element_blank()) +
      # theme(legend.position='none')
      # scale_x_continuous(limits=c(0,100))+
      # scale_y_continuous(limits=c(-100, 0))
      png(file = paste0("study/Test/Presque_equilibre/",res$tester_id[liste1[i]],"Finland.png"))
      plot(heatmap)
      dev.off()
    }
    else if (res$item_id[liste1[i]] == France){
      img <- img_France
      heatmap <- ggplot(data, aes(x=x, y=y)) +
        background_image(img) +
        # theme_void()+
        stat_density_2d(aes(fill = ..density..*10e5, alpha = 1), 
                        geom = "raster", 
                        contour = FALSE) +
        coord_fixed(xlim = c(0,100),ylim = c(100,0))+
        scale_fill_gradient2(low = "transparent", mid = "yellow", high = "red",midpoint = 40)
      # theme(legend.title = element_blank()) +
      # theme(legend.position='none')
      # scale_x_continuous(limits=c(0,100))+
      # scale_y_continuous(limits=c(-100, 0))
      png(file = paste0("study/Test/Presque_equilibre/",res$tester_id[liste1[i]],"France.png"))
      plot(heatmap)
      dev.off()
    }
    else if (res$item_id[liste1[i]] == Greece){
      img <- img_Greece
      heatmap <- ggplot(data, aes(x=x, y=y)) +
        background_image(img) +
        # theme_void()+
        stat_density_2d(aes(fill = ..density..*10e5, alpha = 1), 
                        geom = "raster", 
                        contour = FALSE) +
        coord_fixed(xlim = c(0,100),ylim = c(100,0))+
        scale_fill_gradient2(low = "transparent", mid = "yellow", high = "red",midpoint = 40)
      # theme(legend.title = element_blank()) +
      # theme(legend.position='none')
      # scale_x_continuous(limits=c(0,100))+
      # scale_y_continuous(limits=c(-100, 0))
      png(file = paste0("study/Test/Presque_equilibre/",res$tester_id[liste1[i]],"Greece.png"))
      plot(heatmap)
      dev.off()
    }
    else if (res$item_id[liste1[i]] == Italy){
      img <- img_Italy
      heatmap <- ggplot(data, aes(x=x, y=y)) +
        background_image(img) +
        # theme_void()+
        stat_density_2d(aes(fill = ..density..*10e5, alpha = 1), 
                        geom = "raster", 
                        contour = FALSE) +
        coord_fixed(xlim = c(0,100),ylim = c(100,0))+
        scale_fill_gradient2(low = "transparent", mid = "yellow", high = "red",midpoint = 40)
      # theme(legend.title = element_blank()) +
      # theme(legend.position='none')
      # scale_x_continuous(limits=c(0,100))+
      # scale_y_continuous(limits=c(-100, 0))
      png(file = paste0("study/Test/Presque_equilibre/",res$tester_id[liste1[i]],"Italy.png"))
      plot(heatmap)
      dev.off()
    }
    else if (res$item_id[liste1[i]] == South_Korea){
      img <- img_South_Korea
      heatmap <- ggplot(data, aes(x=x, y=y)) +
        background_image(img) +
        # theme_void()+
        stat_density_2d(aes(fill = ..density..*10e5, alpha = 1), 
                        geom = "raster", 
                        contour = FALSE) +
        coord_fixed(xlim = c(0,100),ylim = c(100,0))+
        scale_fill_gradient2(low = "transparent", mid = "yellow", high = "red",midpoint = 40)
      # theme(legend.title = element_blank()) +
      # theme(legend.position='none')
      # scale_x_continuous(limits=c(0,100))+
      # scale_y_continuous(limits=c(-100, 0))
      png(file = paste0("study/Test/Presque_equilibre/",res$tester_id[liste1[i]],"South_Korea.png"))
      plot(heatmap)
      dev.off()
    }
    else if (res$item_id[liste1[i]] == Spain){
      img <- img_Spain
      heatmap <- ggplot(data, aes(x=x, y=y)) +
        background_image(img) +
        # theme_void()+
        stat_density_2d(aes(fill = ..density..*10e5, alpha = 1), 
                        geom = "raster", 
                        contour = FALSE) +
        coord_fixed(xlim = c(0,100),ylim = c(100,0))+
        scale_fill_gradient2(low = "transparent", mid = "yellow", high = "red",midpoint = 40)
      # theme(legend.title = element_blank()) +
      # theme(legend.position='none')
      # scale_x_continuous(limits=c(0,100))+
      # scale_y_continuous(limits=c(-100, 0))
      png(file = paste0("study/Test/Presque_equilibre/",res$tester_id[liste1[i]],"Spain.png"))
      plot(heatmap)
      dev.off()
    }
    else if (res$item_id[liste1[i]] == Ukraine){
      img <- img_Ukraine
      heatmap <- ggplot(data, aes(x=x, y=y)) +
        background_image(img) +
        # theme_void()+
        stat_density_2d(aes(fill = ..density..*10e5, alpha = 1), 
                        geom = "raster", 
                        contour = FALSE) +
        coord_fixed(xlim = c(0,100),ylim = c(100,0))+
        scale_fill_gradient2(low = "transparent", mid = "yellow", high = "red",midpoint = 40)
      # theme(legend.title = element_blank()) +
      # theme(legend.position='none')
      # scale_x_continuous(limits=c(0,100))+
      # scale_y_continuous(limits=c(-100, 0))
      png(file = paste0("study/Test/Presque_equilibre/",res$tester_id[liste1[i]],"Ukraine.png"))
      plot(heatmap)
      dev.off()
    }
    else {
      img <- img_USA
      heatmap <- ggplot(data, aes(x=x, y=y)) +
        background_image(img) +
        # theme_void()+
        stat_density_2d(aes(fill = ..density..*10e5, alpha = 1), 
                        geom = "raster", 
                        contour = FALSE) +
        coord_fixed(xlim = c(0,100),ylim = c(100,0))+
        scale_fill_gradient2(low = "transparent", mid = "yellow", high = "red",midpoint = 40)
      # theme(legend.title = element_blank()) +
      # theme(legend.position='none')
      # scale_x_continuous(limits=c(0,100))+
      # scale_y_continuous(limits=c(-100, 0))
      png(file = paste0("study/Test/Presque_equilibre/",res$tester_id[liste1[i]],"USA.png"))
      plot(heatmap)
      dev.off()
    }
  }
  else {
    if (res$item_id[liste1[i]]==Brazil){
      img <- img_Brazil
      heatmap <- ggplot(data, aes(x=x, y=y)) +
        background_image(img) +
        # theme_void()+
        stat_density_2d(aes(fill = ..density..*10e5, alpha = 1), 
                        geom = "raster", 
                        contour = FALSE) +
        coord_fixed(xlim = c(0,100),ylim = c(100,0))+
        scale_fill_gradient2(low = "transparent", mid = "yellow", high = "red",midpoint = 40)
      # theme(legend.title = element_blank()) +
      # theme(legend.position='none')
      # scale_x_continuous(limits=c(0,100))+
      # scale_y_continuous(limits=c(-100, 0))
      png(file = paste0("study/Test/Pas_equilibre/",res$tester_id[liste1[i]],"Brazil.png"))
      plot(heatmap)
      dev.off()
    }
    
    else if (res$item_id[liste1[i]] == Finland){
      img <- img_Finland
      heatmap <- ggplot(data, aes(x=x, y=y)) +
        background_image(img) +
        # theme_void()+
        stat_density_2d(aes(fill = ..density..*10e5, alpha = 1), 
                        geom = "raster", 
                        contour = FALSE) +
        coord_fixed(xlim = c(0,100),ylim = c(100,0))+
        scale_fill_gradient2(low = "transparent", mid = "yellow", high = "red",midpoint = 40)
      # theme(legend.title = element_blank()) +
      # theme(legend.position='none')
      # scale_x_continuous(limits=c(0,100))+
      # scale_y_continuous(limits=c(-100, 0))
      png(file = paste0("study/Test/Pas_equilibre/",res$tester_id[liste1[i]],"Finland.png"))
      plot(heatmap)
      dev.off()
    }
    else if (res$item_id[liste1[i]] == France){
      img <- img_France
      heatmap <- ggplot(data, aes(x=x, y=y)) +
        background_image(img) +
        # theme_void()+
        stat_density_2d(aes(fill = ..density..*10e5, alpha = 1), 
                        geom = "raster", 
                        contour = FALSE) +
        coord_fixed(xlim = c(0,100),ylim = c(100,0))+
        scale_fill_gradient2(low = "transparent", mid = "yellow", high = "red",midpoint = 40)
      # theme(legend.title = element_blank()) +
      # theme(legend.position='none')
      # scale_x_continuous(limits=c(0,100))+
      # scale_y_continuous(limits=c(-100, 0))
      png(file = paste0("study/Test/Pas_equilibre/",res$tester_id[liste1[i]],"France.png"))
      plot(heatmap)
      dev.off()
    }
    else if (res$item_id[liste1[i]] == Greece){
      img <- img_Greece
      heatmap <- ggplot(data, aes(x=x, y=y)) +
        background_image(img) +
        # theme_void()+
        stat_density_2d(aes(fill = ..density..*10e5, alpha = 1), 
                        geom = "raster", 
                        contour = FALSE) +
        coord_fixed(xlim = c(0,100),ylim = c(100,0))+
        scale_fill_gradient2(low = "transparent", mid = "yellow", high = "red",midpoint = 40)
      # theme(legend.title = element_blank()) +
      # theme(legend.position='none')
      # scale_x_continuous(limits=c(0,100))+
      # scale_y_continuous(limits=c(-100, 0))
      png(file = paste0("study/Test/Pas_equilibre/",res$tester_id[liste1[i]],"Greece.png"))
      plot(heatmap)
      dev.off()
    }
    else if (res$item_id[liste1[i]] == Italy){
      img <- img_Italy
      heatmap <- ggplot(data, aes(x=x, y=y)) +
        background_image(img) +
        # theme_void()+
        stat_density_2d(aes(fill = ..density..*10e5, alpha = 1), 
                        geom = "raster", 
                        contour = FALSE) +
        coord_fixed(xlim = c(0,100),ylim = c(100,0))+
        scale_fill_gradient2(low = "transparent", mid = "yellow", high = "red",midpoint = 40)
      # theme(legend.title = element_blank()) +
      # theme(legend.position='none')
      # scale_x_continuous(limits=c(0,100))+
      # scale_y_continuous(limits=c(-100, 0))
      png(file = paste0("study/Test/Pas_equilibre/",res$tester_id[liste1[i]],"Italy.png"))
      plot(heatmap)
      dev.off()
    }
    else if (res$item_id[liste1[i]] == South_Korea){
      img <- img_South_Korea
      heatmap <- ggplot(data, aes(x=x, y=y)) +
        background_image(img) +
        # theme_void()+
        stat_density_2d(aes(fill = ..density..*10e5, alpha = 1), 
                        geom = "raster", 
                        contour = FALSE) +
        coord_fixed(xlim = c(0,100),ylim = c(100,0))+
        scale_fill_gradient2(low = "transparent", mid = "yellow", high = "red",midpoint = 40)
      # theme(legend.title = element_blank()) +
      # theme(legend.position='none')
      # scale_x_continuous(limits=c(0,100))+
      # scale_y_continuous(limits=c(-100, 0))
      png(file = paste0("study/Test/Pas_equilibre/",res$tester_id[liste1[i]],"South_Korea.png"))
      plot(heatmap)
      dev.off()
    }
    else if (res$item_id[liste1[i]] == Spain){
      img <- img_Spain
      heatmap <- ggplot(data, aes(x=x, y=y)) +
        background_image(img) +
        # theme_void()+
        stat_density_2d(aes(fill = ..density..*10e5, alpha = 1), 
                        geom = "raster", 
                        contour = FALSE) +
        coord_fixed(xlim = c(0,100),ylim = c(100,0))+
        scale_fill_gradient2(low = "transparent", mid = "yellow", high = "red",midpoint = 40)
      # theme(legend.title = element_blank()) +
      # theme(legend.position='none')
      # scale_x_continuous(limits=c(0,100))+
      # scale_y_continuous(limits=c(-100, 0))
      png(file = paste0("study/Test/Pas_equilibre/",res$tester_id[liste1[i]],"Spain.png"))
      plot(heatmap)
      dev.off()
    }
    else if (res$item_id[liste1[i]] == Ukraine){
      img <- img_Ukraine
      heatmap <- ggplot(data, aes(x=x, y=y)) +
        background_image(img) +
        # theme_void()+
        stat_density_2d(aes(fill = ..density..*10e5, alpha = 1), 
                        geom = "raster", 
                        contour = FALSE) +
        coord_fixed(xlim = c(0,100),ylim = c(100,0))+
        scale_fill_gradient2(low = "transparent", mid = "yellow", high = "red",midpoint = 40)
      # theme(legend.title = element_blank()) +
      # theme(legend.position='none')
      # scale_x_continuous(limits=c(0,100))+
      # scale_y_continuous(limits=c(-100, 0))
      png(file = paste0("study/Test/Pas_equilibre/",res$tester_id[liste1[i]],"Ukraine.png"))
      plot(heatmap)
      dev.off()
    }
    else {
      img <- img_USA
      heatmap <- ggplot(data, aes(x=x, y=y)) +
        background_image(img) +
        # theme_void()+
        stat_density_2d(aes(fill = ..density..*10e5, alpha = 1), 
                        geom = "raster", 
                        contour = FALSE) +
        coord_fixed(xlim = c(0,100),ylim = c(100,0))+
        scale_fill_gradient2(low = "transparent", mid = "yellow", high = "red",midpoint = 40)
      # theme(legend.title = element_blank()) +
      # theme(legend.position='none')
      # scale_x_continuous(limits=c(0,100))+
      # scale_y_continuous(limits=c(-100, 0))
      png(file = paste0("study/Test/Pas_equilibre/",res$tester_id[liste1[i]],"USA.png"))
      plot(heatmap)
      dev.off()
    }
  }
  print(i)
}




