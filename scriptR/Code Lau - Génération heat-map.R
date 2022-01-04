library(ggplot2)
library(imager)
library(MASS)

img <- load.image("https://raw.githubusercontent.com/EyeTrackingSTAT2122/Simulateddata/main/A.jpg")

#Gauche

N <- 100

rho <- 0
mu1 <- runif(1, 6.4, 28.8); s1 <- runif(1,1,5)
mu2 <- runif(1, min = 6.4, max = 57.6); s2 <- runif(1,1,5)

mu <- c(mu1,mu2)
sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2)



bvn1 <- mvrnorm(N, mu = mu, Sigma = sigma )
colnames(bvn1) <- c("bvn1_X1","bvn1_X2")
bvn1 <- as.data.frame(bvn1)

plot(bvn1,xlim=c(0,64),ylim=c(0,64))

juges <- 100
for (i in 1:juges){
  
  mu1 <- runif(1, 6.4, 28.8); s1 <- runif(1,1,5)
  mu2 <- runif(1, min = 6.4, max = 57.6); s2 <- runif(1,1,5)
  
  mu <- c(mu1,mu2)
  
  sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2)
  
  bvn1 <- mvrnorm(N, mu = mu, Sigma = sigma )
  colnames(bvn1) <- c("bvn1_X1","bvn1_X2")
  bvn1 <- as.data.frame(bvn1)
  
  ggplot(bvn1, aes(x = bvn1_X1, y = bvn1_X2))  + 
    annotation_raster(img, xmin=0, xmax=64, ymin=0, ymax=64)+
    # geom_point(size=0.5)+
    stat_density2d(geom = "polygon", aes(fill=..level..)) +
    scale_fill_gradient(low="green",high="red", guide = "none") + 
    scale_x_continuous(limits=c(0,dim(img)[2]),expand=c(0,0))+
    scale_y_continuous(limits=c(0,dim(img)[1]),expand=c(0,0))+
    coord_fixed() +
    theme(legend.background = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank())
  ggsave(filename = paste0("E:/Master 2/Simulateddata/Train/Gauche/Image_",i,".png", sep = ""), width = unit(64,units = "in") ,height = 64)
}
