library(ggplot2)
library(imager)
library(MASS)
library(truncnorm)
library(png)
library(jpeg)
library(grid)

width_size <- 960
height_size <- 624

#Insertion des images ----
#1
img1 = "Plateaux_monde/1_Brazil"
img1 <- readJPEG(img1)
img1R <- rasterGrob(img1, interpolate=TRUE, x = 1, y = 0.5, hjust = 2)
#2
img2 = "Plateaux_monde/2_Finland"
img2 <- readJPEG(img2)
img2R <- rasterGrob(img2, interpolate=TRUE, x = 1, y = 0.5, hjust = 2)
#3
img3 = "Plateaux_monde/3_France"
img3 <- readJPEG(img3)
img3R <- rasterGrob(img3, interpolate=TRUE, x = 1, y = 0.5, hjust = 2)
#4
img4 = "Plateaux_monde/4_Greece"
img4 <- readJPEG(img4)
img4R <- rasterGrob(img4, interpolate=TRUE, x = 1, y = 0.5, hjust = 2)
#5
img5 = "Plateaux_monde/5_Italy"
img5 <- readJPEG(img5)
img5R <- rasterGrob(img5, interpolate=TRUE, x = 1, y = 0.5, hjust = 2)
#6
img6 = "Plateaux_monde/6_Spain"
img6 <- readJPEG(img6)
img6R <- rasterGrob(img6, interpolate=TRUE, x = 1, y = 0.5, hjust = 2)
#7
img7 = "Plateaux_monde/7_South Korea"
img7 <- readJPEG(img7)
img7R <- rasterGrob(img7, interpolate=TRUE, x = 1, y = 0.5, hjust = 2)
#8
img8 = "Plateaux_monde/8_Ukraine"
img8 <- readJPEG(img8)
img8R <- rasterGrob(img8, interpolate=TRUE, x = 1, y = 0.5, hjust = 2)
#9
img9 = "Plateaux_monde/9_USA"
img9 <- readJPEG(img9)
img9R <- rasterGrob(img9, interpolate=TRUE, x = 1, y = 0.5, hjust = 2)

# Fonctions ----

haut_G <- function(N=100, width_size=525, height_size=288){
  
  
  rho <- 0
  mu1 <- runif(1, 0.25, 0.3); s1 <- runif(1,0.06,0.06)
  mu2 <- runif(1, min = 0.61, max = 0.625); s2 <- runif(1,0.07,0.07)
  
  mu1 <- mu1 * width_size
  mu2 <- mu2 * height_size
  s1 <- s1 * width_size
  s2 <- s2 * height_size
  
  mu <- c(mu1,mu2)
  sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2)
  
  
  
  bvn1 <- mvrnorm(N, mu = mu, Sigma = sigma )
  colnames(bvn1) <- c("bvn1_X1","bvn1_X2")
  bvn1 <<- as.data.frame(bvn1)
}

dessert <- function(N=100,width_size=525,height_size=288){
  
  rho <- 0
  mu1 <- runif(1, 0.8, 0.85); s1 <- runif(1,0.04,0.04)
  mu2 <- runif(1, min = 0.54, max = 0.555); s2 <- runif(1,0.104,0.139)
  
  mu1 <- mu1 * width_size
  mu2 <- mu2 * height_size
  s1 <- s1 * width_size
  s2 <- s2 * height_size
  
  mu <- c(mu1,mu2)
  sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2)
  
  
  bvn1 <- mvrnorm(N, mu = mu, Sigma = sigma )
  colnames(bvn1) <- c("bvn1_X1","bvn1_X2")
  bvn1 <<- as.data.frame(bvn1)
}

assiette <- function(N=100,width_size = 525, height_size = 288){
  
  rho <- 0
  mu1 <- runif(1, 0.46, 0.63); s1 <- runif(1,0.047,0.076)
  mu2 <- runif(1, min = 0.451, max = 0.469); s2 <- runif(1,0.104,0.139)
  
  mu1 <- mu1 * width_size
  mu2 <- mu2 * height_size
  s1 <- s1 * width_size
  s2 <- s2 * height_size
  
  mu <- c(mu1,mu2)
  sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2)
  
  
  
  bvn1 <- mvrnorm(N, mu = mu, Sigma = sigma )
  colnames(bvn1) <- c("bvn1_X1","bvn1_X2")
  bvn1 <<- as.data.frame(bvn1)
}

width_size_final = (width_size*2)
height_size_final = height_size

plateau_knowed <- function(n_plateau = 1, N_juges = 100,p_regard_C = 0.33, p_regard_A = 0.66,width_size = 525, height_size = 288, width_size_final = 256, height_size_final = 201){
  if (n_plateau == 1){
    p_carrot = 0.95
    p_assiette = 0.95
    p_dessert = 0.95
    img <- img1
    imgR <- img1R
  }
  else if (n_plateau == 2){
    p_carrot = 0.95
    p_assiette = 0.5
    p_dessert = 0.95
    img <- img2
    imgR <- img2R
  }
  else if (n_plateau == 3){
    p_carrot = 0.25
    p_assiette = 0.75
    p_dessert = 0.95
    img <- img3
    imgR <- img3R
  }
  else if (n_plateau == 4){
    p_carrot = 0.25
    p_assiette = 0.25
    p_dessert = 0.95
    img <- img4
    imgR <- img4R
  }
  else if (n_plateau == 5){
    p_carrot = 0.95
    p_assiette = 0.5
    p_dessert = 0.25
    img <- img5
    imgR <- img5R
  }
  else if (n_plateau == 6){
    p_carrot = 0.95
    p_assiette = 0.5
    p_dessert = 0.25
    img <- img6
    imgR <- img6R
  }
  else if (n_plateau == 7){
    p_carrot = 0.25
    p_assiette = 0.75
    p_dessert = 0.25
    img <- img7
    imgR <- img7R
  }
  else if (n_plateau == 8){
    p_carrot = 0.25
    p_assiette = 0.75
    p_dessert = 0.25
    img <- img8
    imgR <- img8R
  }
  else {
    
  }
  
  
  
  for (i in 1:N_juges){
    tirage <- runif(1)
    ifelse(test = tirage < p_regard_C, yes = carrot(width_size = width_size, height_size = height_size), no = ifelse(test = tirage < p_regard_A, yes =  assiette(width_size = width_size, height_size = height_size), no = dessert(width_size = width_size, height_size = height_size)))
    
    cotcot <- ggplot(bvn1, aes(x = bvn1_X1, y = bvn1_X2))  + 
      # theme_void() +
      annotation_custom(imgR) +
      coord_fixed(xlim = c(-width_size,width_size),ylim = c(0,height_size)) +
      stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE, alpha = 0.7) +
      scale_fill_distiller(palette= "Spectral", direction=-1,guide = "none") + 
      scale_x_continuous(limits=c(0,width_size))+
      scale_y_continuous(limits=c(0, height_size)) +
      theme(legend.background = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank())
    
    tirage_2 <- runif(1)
    
    #Si il trouve le plateau équilibré
    if ((tirage_2 < p_carrot & tirage < p_regard_C)| (tirage_2 < p_assiette & tirage < p_regard_A & tirage > p_regard_C)|(tirage_2 < p_dessert & tirage > p_regard_A)){
      png(file = paste0("data/plateau/Connu/Balanced/","Juge_",i,"_plateau_n",n_plateau,".png"), 
          width = width_size_final, 
          height = height_size_final)
      plot(cotcot)
      dev.off()
    }
    
    #Si il trouve le plateau déséquilibré
    if ((tirage_2 > p_carrot & tirage < p_regard_C)| (tirage_2 > p_assiette & tirage < p_regard_A & tirage > p_regard_C)| (tirage_2 > p_dessert & tirage > p_regard_A)){
      png(file = paste0("data/plateau/Connu/Unbalanced/","Juge_",i,"_plateau_n",n_plateau,".png"), 
          width = width_size_final, 
          height = height_size_final)
      plot(cotcot)
      dev.off()
    }
    # save heatmaps in .png format
  }
}


plateau_knowed(n_plateau = 1, N_juges = 1, width_size = 5760, height_size = 3840, width_size_final = (5760)*2, height_size_final = 3840)

test <- Sys.time()
for (j in 1:8){
  plateau_knowed(n_plateau = j, N_juges = 20, width_size = 5760, height_size = 3840, width_size_final = (5760*2), height_size_final = 3840)
}
result <-  Sys.time() - test

plateau_unknowed <- function(n_plateau = 1, N_juges = 100,p_regard_C = 0.33, p_regard_A = 0.66, width_size = 256, height_size = 201){
  if (n_plateau == 1){
    img = "Repas/Plateau_redim/plateau1.png"
    img <- readPNG(img)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 2){
    img = "Repas/Plateau_redim/plateau2.png"
    img <- readPNG(img)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 3){
    img = "Repas/Plateau_redim/plateau3.png"
    img <- readPNG(img)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 4){
    img = "Repas/Plateau_redim/plateau4.png"
    img <- readPNG(img)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 5){
    img = "Repas/Plateau_redim/plateau5.png"
    img <- readPNG(img)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 6){
    img = "Repas/Plateau_redim/plateau6.png"
    img <- readPNG(img)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 7){
    img = "Repas/Plateau_redim/plateau7.png"
    img <- readPNG(img)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 1)
  }
  else{
    img = "Repas/Plateau_redim/plateau8.png"
    img <- readPNG(img)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 1)
  }
  for (i in 1:N_juges){
    tirage <- runif(1)
    ifelse(test = tirage < p_regard_C, yes = carrot(), no = ifelse(test = tirage < p_regard_A, yes =  assiette(), no = dessert()))
    
    cotcot <- ggplot(bvn1, aes(x = bvn1_X1, y = bvn1_X2))  + 
      theme_void() +
      annotation_custom(imgR) +
      stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE, alpha = 0.7) +
      scale_fill_distiller(palette= "Spectral", direction=-1,guide = "none") + 
      scale_x_continuous(limits=c(160,525))+
      scale_y_continuous(limits=c(23, 265))+
      coord_fixed(xlim = c(-300,525),ylim = c(0,288)) +
      theme(legend.background = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank())
    
    png(file = paste0("data/plateau/Inconnu/","Juge_",i,"_plateau_n",n_plateau,".png"), 
        width = width_size, 
        height = height_size)
    plot(cotcot)
    dev.off()
  }
  # save heatmaps in .png format
}


for (j in 1:8){
  plateau_unknowed(n_plateau = j)
}

plateau_sup_knowed <- function(n_plateau = 1, N_juges = 100, p_regard_C = 0.33, p_regard_A = 0.66, width_size = 256, height_size = 201){
  if (n_plateau == 1){
    p_carrot = 0.95
    p_assiette = 0.95
    p_dessert = 0.95
    img = "Repas/plateau_redim/plateau1.png"
    img <- readPNG(img)
    imgH <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 0)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 1, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 2){
    p_carrot = 0.95
    p_assiette = 0.5
    p_dessert = 0.95
    img = "Repas/Plateau_redim/plateau2.png"
    img <- readPNG(img)
    imgH <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 0)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 1, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 3){
    p_carrot = 0.25
    p_assiette = 0.75
    p_dessert = 0.95
    img = "Repas/Plateau_redim/plateau3.png"
    img <- readPNG(img)
    imgH <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 0)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 1, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 4){
    p_carrot = 0.25
    p_assiette = 0.25
    p_dessert = 0.95
    img = "Repas/Plateau_redim/plateau4.png"
    img <- readPNG(img)
    imgH <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 0)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 1, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 5){
    p_carrot = 0.95
    p_assiette = 0.5
    p_dessert = 0.25
    img = "Repas/Plateau_redim/plateau5.png"
    img <- readPNG(img)
    imgH <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 0)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 1, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 6){
    p_carrot = 0.95
    p_assiette = 0.5
    p_dessert = 0.25
    img = "Repas/Plateau_redim/plateau6.png"
    img <- readPNG(img)
    imgH <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 0)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 1, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 7){
    p_carrot = 0.25
    p_assiette = 0.75
    p_dessert = 0.25
    img = "Repas/Plateau_redim/plateau7.png"
    img <- readPNG(img)
    imgH <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 0)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 1, y = 0.5, hjust = 1)
  }
  else{
    p_carrot = 0.25
    p_assiette = 0.75
    p_dessert = 0.25
    img = "Repas/Plateau_redim/plateau8.png"
    img <- readPNG(img)
    imgH <- rasterGrob(img, interpolate=TRUE, hjust = 0.5)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 1, y = 0.5, hjust = 1)
  }
  for (i in 1:N_juges){
    tirage <- runif(1)
    ifelse(test = tirage < p_regard_C, yes = carrot(), no = ifelse(test = tirage < p_regard_A, yes =  assiette(), no = dessert()))
    
    cotcot <- ggplot(bvn1, aes(x = bvn1_X1, y = bvn1_X2))  + 
      theme_void() +
      # annotation_custom(imgH) +
      annotation_custom(imgR) +
      stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE, alpha = 0.7) +
      scale_fill_distiller(palette= "Spectral", direction=-1,guide = "none") + 
      scale_x_continuous(limits=c(198,530))+
      scale_y_continuous(limits=c(33, 255))+
      coord_fixed(xlim = c(160,555),ylim = c(0,288)) +
      theme(legend.background = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank())
    
    tirage_2 <- runif(1)
    
    #Si il trouve le plateau équilibré
    if ((tirage_2 < p_carrot & tirage < p_regard_C)| (tirage_2 < p_assiette & tirage < p_regard_A & tirage > p_regard_C)|(tirage_2 < p_dessert & tirage > p_regard_A)){
      png(file = paste0("data/plateau_superpose/Connu/Balanced/","Juge_",i,"_plateau_n",n_plateau,".png"), 
          width = width_size, 
          height = height_size)
      plot(cotcot)
      dev.off()
    }
    
    #Si il trouve le plateau déséquilibré
    if ((tirage_2 > p_carrot & tirage < p_regard_C)| (tirage_2 > p_assiette & tirage < p_regard_A & tirage > p_regard_C)| (tirage_2 > p_dessert & tirage > p_regard_A)){
      png(file = paste0("data/plateau_superpose/Connu/Unbalanced/","Juge_",i,"_plateau_n",n_plateau,".png"), 
          width = width_size, 
          height = height_size)
      plot(cotcot)
      dev.off()
    }
    # save heatmaps in .png format
  }
}


for (j in 1:8){
  plateau_sup_knowed(n_plateau = j)
}

plateau_sup_unknowed <- function(n_plateau = 1, N_juges = 100, p_regard_C = 0.33, p_regard_A = 0.66, width_size = 256, height_size = 201){
  if (n_plateau == 1){
    p_carrot = 0.95
    p_assiette = 0.95
    p_dessert = 0.95
    img = "Repas/plateau_redim/plateau1.png"
    img <- readPNG(img)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 1, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 2){
    p_carrot = 0.95
    p_assiette = 0.5
    p_dessert = 0.95
    img = "Repas/Plateau_redim/plateau2.png"
    img <- readPNG(img)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 1, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 3){
    p_carrot = 0.25
    p_assiette = 0.75
    p_dessert = 0.95
    img = "Repas/Plateau_redim/plateau3.png"
    img <- readPNG(img)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 1, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 4){
    p_carrot = 0.25
    p_assiette = 0.25
    p_dessert = 0.95
    img = "Repas/Plateau_redim/plateau4.png"
    img <- readPNG(img)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 1, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 5){
    p_carrot = 0.95
    p_assiette = 0.5
    p_dessert = 0.25
    img = "Repas/Plateau_redim/plateau5.png"
    img <- readPNG(img)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 1, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 6){
    p_carrot = 0.95
    p_assiette = 0.5
    p_dessert = 0.25
    img = "Repas/Plateau_redim/plateau6.png"
    img <- readPNG(img)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 1, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 7){
    p_carrot = 0.25
    p_assiette = 0.75
    p_dessert = 0.25
    img = "Repas/Plateau_redim/plateau7.png"
    img <- readPNG(img)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 1, y = 0.5, hjust = 1)
  }
  else{
    p_carrot = 0.25
    p_assiette = 0.75
    p_dessert = 0.25
    img = "Repas/Plateau_redim/plateau8.png"
    img <- readPNG(img)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 1, y = 0.5, hjust = 1)
  }
  for (i in 1:N_juges){
    tirage <- runif(1)
    ifelse(test = tirage < p_regard_C, yes = carrot(), no = ifelse(test = tirage < p_regard_A, yes =  assiette(), no = dessert()))
    
    cotcot <- ggplot(bvn1, aes(x = bvn1_X1, y = bvn1_X2))  + 
      theme_void() +
      # annotation_custom(imgH, xmin = 160, xmax = 525) +
      annotation_custom(imgR) +
      stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE, alpha = 0.7) +
      scale_fill_distiller(palette= "Spectral", direction=-1,guide = "none") + 
      scale_x_continuous(limits=c(198,530))+
      scale_y_continuous(limits=c(33, 255))+
      coord_fixed(xlim = c(160,555),ylim = c(0,288)) +
      theme(legend.background = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank())
    
    tirage_2 <- runif(1)
    
    #Si il trouve le plateau équilibré
    png(file = paste0("data/plateau_superpose/Inconnu/","Juge_",i,"_plateau_n",n_plateau,".png"), 
        width = width_size, 
        height = height_size)
    plot(cotcot)
    dev.off()
    # save heatmaps in .png format
  }
}

i = "test" 

for (j in 1:8){
  plateau_sup_unknowed(n_plateau = j)
}
