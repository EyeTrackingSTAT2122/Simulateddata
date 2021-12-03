library(ggplot2)
library(imager)

juges <- 100
mu_droite <- 48
teta_droite <- 5

loi_droite <- rnorm(1, mean = mu_droite, sd = teta_droite)

mu_gauche <- 12
teta_gauche <- 5

loi_gauche <- rnorm(1, mean = mu_gauche, sd = teta_gauche)

mu_bas <- 12
teta_bas <- 5

loi_bas <- rnorm(1, mean = mu_bas, sd = teta_bas)

mu_haut <- 48
teta_haut <- 5

loi_haut <- rnorm(1, mean = mu_haut, sd = teta_haut)

proba_gauche <- 0.7
proba_droite <- 1 - proba_gauche
proba_bas <- 0.7
proba_haut <- 1 - proba_bas

regard_x <- rep(0,juges)
regard_y <- rep(0,juges)

for (i in 1:juges){
   if (runif(1)>proba_droite){
     regard_x[i] = rnorm(1, mean = mu_gauche, sd = teta_gauche)
   }
   else {
     regard_x[i] = rnorm(1, mean = mu_droite, sd = teta_droite)
   }
   if (runif(1)>proba_haut){
     regard_y[i] = rnorm(1, mean = mu_haut, sd = teta_haut)
   }
   else {
     regard_y[i] = rnorm(1, mean = mu_bas, sd = teta_bas)
   }
}

fixation <- rep(0,juges)
for (i in 1:juges){
  fixation[i] <- runif(1,min = 2, max = 7)
}

df <- data.frame(regard_x,regard_y,fixation)

for
ggplot() + geom_point(aes(x = df$regard_x, y = df$regard_y, fill = df$fixation), shape = 21) + xlim(0,100) + ylim(0,100) +
  coord_equal() + scale_fill_gradient(low = "blue", high = "red") + theme(legend.background = element_blank())

img <- load.image("E:/Master 2/Simulateddata/A.jpg")

for (i in 1:nrow(df)){
  ggplot(df, aes(regard_x[i],regard_y[i]))  + 
    annotation_raster(img, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
    geom_point(size=15, fill = fixation, shape = round(runif(1,21,25)))+
    scale_x_continuous(limits=c(0,dim(img)[2]),expand=c(0,0))+
    scale_y_continuous(limits=c(0,dim(img)[1]),expand=c(0,0))+
    coord_fixed()
  ggsave(filename = paste("Image",i,".png"))
}

  

