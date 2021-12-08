library(ggplot2)
library(imager)
library(MASS)
library(truncnorm)
library(png)
library(grid)

carrot <- function(N=100){
  
  
  rho <- 0
  mu1 <- runif(1, 245, 265); s1 <- runif(1,30,30)
  mu2 <- runif(1, min = 175, max = 180); s2 <- runif(1,20,20)
  
  mu <- c(mu1,mu2)
  sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2)
  
  
  
  bvn1 <- mvrnorm(N, mu = mu, Sigma = sigma )
  colnames(bvn1) <- c("bvn1_X1","bvn1_X2")
  bvn1 <<- as.data.frame(bvn1)
}

dessert <- function(N=100){
  
  rho <- 0
  mu1 <- runif(1, 470, 475); s1 <- runif(1,20,20)
  mu2 <- runif(1, min = 155, max = 160); s2 <- runif(1,30,40)
  
  mu <- c(mu1,mu2)
  sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2)
  
  
  
  bvn1 <- mvrnorm(N, mu = mu, Sigma = sigma )
  colnames(bvn1) <- c("bvn1_X1","bvn1_X2")
  bvn1 <<- as.data.frame(bvn1)
  
  return(bvn1)
}

assiette <- function(N=100){
  
  rho <- 0
  mu1 <- runif(1, 355, 390); s1 <- runif(1,30,40)
  mu2 <- runif(1, min = 130, max = 135); s2 <- runif(1,30,40)
  
  mu <- c(mu1,mu2)
  sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2)
  
  
  
  bvn1 <- mvrnorm(N, mu = mu, Sigma = sigma )
  colnames(bvn1) <- c("bvn1_X1","bvn1_X2")
  bvn1 <<- as.data.frame(bvn1)
  
  return(bvn1)
}

plateau_knowed <- function(n_plateau = 1, N_juges = 100,p_regard_C = 0.33, p_regard_A = 0.66, width_size = 256, height_size = 201){
  if (n_plateau == 1){
    p_carrot = 0.95
    p_assiette = 0.95
    p_dessert = 0.95
    img = "Repas/Plateau_redim/plateau1.png"
    img <- readPNG(img)
    imgH <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 0)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 2){
    p_carrot = 0.95
    p_assiette = 0.5
    p_dessert = 0.95
    img = "Repas/Plateau_redim/plateau2.png"
    img <- readPNG(img)
    imgH <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 0)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 3){
    p_carrot = 0.25
    p_assiette = 0.75
    p_dessert = 0.95
    img = "Repas/Plateau_redim/plateau3.png"
    img <- readPNG(img)
    imgH <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 0)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 4){
    p_carrot = 0.25
    p_assiette = 0.25
    p_dessert = 0.95
    img = "Repas/Plateau_redim/plateau4.png"
    img <- readPNG(img)
    imgH <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 0)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 5){
    p_carrot = 0.95
    p_assiette = 0.5
    p_dessert = 0.25
    img = "Repas/Plateau_redim/plateau5.png"
    img <- readPNG(img)
    imgH <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 0)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 6){
    p_carrot = 0.95
    p_assiette = 0.5
    p_dessert = 0.25
    img = "Repas/Plateau_redim/plateau6.png"
    img <- readPNG(img)
    imgH <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 0)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 7){
    p_carrot = 0.25
    p_assiette = 0.75
    p_dessert = 0.25
    img = "Repas/Plateau_redim/plateau7.png"
    img <- readPNG(img)
    imgH <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 0)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 1)
  }
  else{
    p_carrot = 0.25
    p_assiette = 0.75
    p_dessert = 0.25
    img = "Repas/Plateau_redim/plateau8.png"
    img <- readPNG(img)
    imgH <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 0)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 1)
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
      scale_x_continuous(limits=c(160,525))+
      scale_y_continuous(limits=c(23, 265))+
      coord_fixed(xlim = c(-300,525),ylim = c(0,288)) +
      theme(legend.background = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank())
    
    tirage_2 <- runif(1)
    
    #Si il trouve le plateau équilibré
    if ((tirage_2 < p_carrot & tirage < p_regard_C)| (tirage_2 < p_assiette & tirage < p_regard_A & tirage > p_regard_C)|(tirage_2 < p_dessert & tirage > p_regard_A)){
      png(file = paste0("data/plateau/Connu/Balanced/","Juge_",i,"_plateau_n",n_plateau,".png"), 
          width = width_size, 
          height = height_size)
      plot(cotcot)
      dev.off()
    }
    
    #Si il trouve le plateau déséquilibré
    if ((tirage_2 > p_carrot & tirage < p_regard_C)| (tirage_2 > p_assiette & tirage < p_regard_A & tirage > p_regard_C)| (tirage_2 > p_dessert & tirage > p_regard_A)){
      png(file = paste0("data/plateau/Connu/Unbalanced/","Juge_",i,"_plateau_n",n_plateau,".png"), 
          width = width_size, 
          height = height_size)
      plot(cotcot)
      dev.off()
    }
    # save heatmaps in .png format
  }
}

for (j in 1:8){
  plateau_knowed(n_plateau = j)
}


plateau_unknowed <- function(n_plateau = 1, N_juges = 100,p_regard_C = 0.33, p_regard_A = 0.66, width_size = 256, height_size = 201){
  if (n_plateau == 1){
    img = "Repas/Plateau_redim/plateau1.png"
    img <- readPNG(img)
    imgH <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 0)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 2){
    img = "Repas/Plateau_redim/plateau2.png"
    img <- readPNG(img)
    imgH <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 0)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 3){
    img = "Repas/Plateau_redim/plateau3.png"
    img <- readPNG(img)
    imgH <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 0)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 4){
    img = "Repas/Plateau_redim/plateau4.png"
    img <- readPNG(img)
    imgH <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 0)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 5){
    img = "Repas/Plateau_redim/plateau5.png"
    img <- readPNG(img)
    imgH <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 0)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 6){
    img = "Repas/Plateau_redim/plateau6.png"
    img <- readPNG(img)
    imgH <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 0)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 1)
  }
  else if (n_plateau == 7){
    img = "Repas/Plateau_redim/plateau7.png"
    img <- readPNG(img)
    imgH <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 0)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 1)
  }
  else{
    img = "Repas/Plateau_redim/plateau8.png"
    img <- readPNG(img)
    imgH <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 0)
    imgR <- rasterGrob(img, interpolate=TRUE, x = 0.5, y = 0.5, hjust = 1)
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
