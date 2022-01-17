library(tidyverse)
library(ggplot2)
library(tidyr)
library(jpeg)
library(ggpubr)
library(grid)
library(sjmisc)

set.seed(123)

test_zone_couleurs <- read_delim("scriptR/study/test_zone_couleurs.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

test_zone_couleurs <- as.data.frame(test_zone_couleurs)
i <- 1

#On retire les données de calibration

while (str_contains(test_zone_couleurs$Pays[i], "Calibration") == TRUE){
  test_zone_couleurs <- test_zone_couleurs[-i,]
}

data <- test_zone_couleurs

tZ1 <- runif(nrow(test_zone_couleurs), 0, 30)
tZ2 <- runif(nrow(test_zone_couleurs), 0, 30)
tZ3 <- runif(nrow(test_zone_couleurs), 0, 30)
tZ4 <- runif(nrow(test_zone_couleurs), 0, 30)
tZ5 <- runif(nrow(test_zone_couleurs), 0, 30)

data$tz1 <- tZ1 #temps zone HG
data$tz2 <- tZ2 #temps zone HM
data$tz3 <- tZ3 #temps zone HD
data$tz4 <- tZ4 #temps zone BG
data$tz5 <- tZ5 #temps zone BD

tTot <- rep(0,nrow(data))

#Calcul du temps total

for (i in 1:nrow(data)){
  tTot[i] <- data$tz1[i] + data$tz2[i] + data$tz3[i] + data$tz4[i] + data$tz5[i]
}

data$tTot <- tTot #temps total passé sur l'image

# img_Spain <- readJPEG("Plateaux_monde/6_Spain.jpg")

# img <- img_Spain

get_img <- function(Pays, AlFec = 0, AlLeg = 0, AlProt = 0, AlFru = 0, AlProt_Fec = 0, AlProt_Leg = 0, AlFec_Fru = 0, AlLait_Fru = 0){
  colPlateau <- rgb(220/255,220/255,220/255)
  colFeculents <- rgb(1,0,0, AlFec)
  colLegumes <- rgb(0,1,0, AlLeg)
  colProteines <- rgb(0,0,1, AlProt)
  colFruits <- rgb(153/255,76/255,0, AlFru)
  
  colProt_Fec <- rgb(1,0,1, AlProt_Fec)
  colProt_Leg <- rgb(0,1,1, AlProt_Leg)
  colFec_Fru <- rgb(0,0,0, AlFec_Fru)
  colLait_Fru <- rgb(1,1,0, AlLait_Fru)
  
  x <- c(20,80)
  y <- c(10,80)
  
  if(Pays == "Ukraine"){
    
    plot(y~x,col = "white", ylim = rev(range(y)),axes = FALSE, xlab = NA, ylab = NA)            # Draw empty plot
    # rasterImage(img, 0, 100, 100, 0)
    # polygon(x = c(20.5,22,24,74,76,79,79.5,79,77,75.5,24,22,20.5), y = c(14.5,11.5,9,9,10,13.5,14.5,75,80,81,80,78,75),
    #         col = colPlateau) #Plateau
    polygon(x = c(21,24,35,40,40,38,27,21), y = c(15,11,11,15,40,43,43,37), col = colLegumes) #Compartiment HG
    polygon(x = c(41,57,57,55,43,41), y = c(12,12,36,39,39,36),col = colFeculents) #Compartiment HM
    polygon(x = c(58,62,75,78,78,78,60,58), y = c(13,11,11,15,40,44,44,37), col = colFeculents) #Compartiment HD
    polygon(x = c(23,25,43,45,39,37,35,25,23,22,22),y = c(49,45,44,48,65,75,77,77,75,73,71), col = colLegumes) #Compartiment BG
    polygon(x = c(48,56,58.5,59,74,76,76,74,43,41,40.5,40.5),y = c(42,42,44,46,46,48,74,77,77,73,71,70),col = colProt_Fec) #Compartiment BD
  }
  
  else if (data$Pays[i] == "Brazil"){
    
    plot(y~x,col = "white", ylim = rev(range(y)),axes = FALSE, xlab = NA, ylab = NA)            # Draw empty plot
    # rasterImage(img, 0, 100, 100, 0)
    # polygon(x = c(20.5,22,24,74,76,79,79.5,79,77,75.5,24,22,20.5), y = c(14.5,11.5,9,9,10,13.5,14.5,75,80,81,80,78,75),
    #         col = colPlateau) #Plateau
    polygon(x = c(21,24,35,40,40,38,27,21), y = c(15,11,11,15,40,43,43,37), col = colFeculents) #Compartiment HG
    polygon(x = c(41,57,57,55,43,41), y = c(12,12,36,39,39,36),col = colLegumes) #Compartiment HM
    polygon(x = c(58,62,75,78,78,78,60,58), y = c(13,11,11,15,40,44,44,37), col = colFruits) #Compartiment HD
    polygon(x = c(23,25,43,45,39,37,35,25,23,22,22),y = c(49,45,44,48,65,75,77,77,75,73,71), col = colProt_Leg) #Compartiment BG
    polygon(x = c(48,56,58.5,59,74,76,76,74,43,41,40.5,40.5),y = c(42,42,44,46,46,48,74,77,77,73,71,70),col = colFeculents) #Compartiment BD
    
  }
  else if (data$Pays[i] == "France"){
    
    plot(y~x,col = "white", ylim = rev(range(y)),axes = FALSE, xlab = NA, ylab = NA)            # Draw empty plot
    # rasterImage(img, 0, 100, 100, 0)
    # polygon(x = c(20.5,22,24,74,76,79,79.5,79,77,75.5,24,22,20.5), y = c(14.5,11.5,9,9,10,13.5,14.5,75,80,81,80,78,75),
    #         col = colPlateau) #Plateau
    polygon(x = c(21,24,35,40,40,38,27,21), y = c(15,11,11,15,40,43,43,37), col = colFruits) #Compartiment HG
    polygon(x = c(41,57,57,55,43,41), y = c(12,12,36,39,39,36),col = colLegumes) #Compartiment HM
    polygon(x = c(58,62,75,78,78,78,60,58), y = c(13,11,11,15,40,44,44,37), col = colProteines) #Compartiment HD
    polygon(x = c(23,25,43,45,39,37,35,25,23,22,22),y = c(49,45,44,48,65,75,77,77,75,73,71), col = colLegumes) #Compartiment BG
    polygon(x = c(48,56,58.5,59,74,76,76,74,43,41,40.5,40.5),y = c(42,42,44,46,46,48,74,77,77,73,71,70),col = colLait_Fru) #Compartiment BD
    
  }
  else if (data$Pays[i] == "Finland"){
    
    plot(y~x,col = "white", ylim = rev(range(y)),axes = FALSE, xlab = NA, ylab = NA)            # Draw empty plot
    # rasterImage(img, 0, 100, 100, 0)
    # polygon(x = c(20.5,22,24,74,76,79,79.5,79,77,75.5,24,22,20.5), y = c(14.5,11.5,9,9,10,13.5,14.5,75,80,81,80,78,75),
    #         col = colPlateau) #Plateau
    polygon(x = c(21,24,35,40,40,38,27,21), y = c(15,11,11,15,40,43,43,37), col = colFeculents) #Compartiment HG
    polygon(x = c(41,57,57,55,43,41), y = c(12,12,36,39,39,36),col = colLegumes) #Compartiment HM
    polygon(x = c(58,62,75,78,78,78,60,58), y = c(13,11,11,15,40,44,44,37), col = colLegumes) #Compartiment HD
    polygon(x = c(23,25,43,45,39,37,35,25,23,22,22),y = c(49,45,44,48,65,75,77,77,75,73,71), col = colLegumes) #Compartiment BG
    polygon(x = c(48,56,58.5,59,74,76,76,74,43,41,40.5,40.5),y = c(42,42,44,46,46,48,74,77,77,73,71,70),col = colFec_Fru) #Compartiment BD
  }
  else if (data$Pays[i] == "Greece"){
    
    plot(y~x,col = "white", ylim = rev(range(y)),axes = FALSE, xlab = NA, ylab = NA)            # Draw empty plot
    # rasterImage(img, 0, 100, 100, 0)
    # polygon(x = c(20.5,22,24,74,76,79,79.5,79,77,75.5,24,22,20.5), y = c(14.5,11.5,9,9,10,13.5,14.5,75,80,81,80,78,75),
    #         col = colPlateau) #Plateau
    polygon(x = c(21,24,35,40,40,38,27,21), y = c(15,11,11,15,40,43,43,37), col = colLait_Fru) #Compartiment HG
    polygon(x = c(41,57,57,55,43,41), y = c(12,12,36,39,39,36),col = colLegumes) #Compartiment HM
    polygon(x = c(58,62,75,78,78,78,60,58), y = c(13,11,11,15,40,44,44,37), col = colFeculents) #Compartiment HD
    polygon(x = c(23,25,43,45,39,37,35,25,23,22,22),y = c(49,45,44,48,65,75,77,77,75,73,71), col = colFruits) #Compartiment BG
    polygon(x = c(48,56,58.5,59,74,76,76,74,43,41,40.5,40.5),y = c(42,42,44,46,46,48,74,77,77,73,71,70),col = colProt_Fec) #Compartiment BD
  }
  else if (data$Pays[i] == "Italy"){
    
    plot(y~x,col = "white", ylim = rev(range(y)),axes = FALSE, xlab = NA, ylab = NA)            # Draw empty plot
    # rasterImage(img, 0, 100, 100, 0)
    # polygon(x = c(20.5,22,24,74,76,79,79.5,79,77,75.5,24,22,20.5), y = c(14.5,11.5,9,9,10,13.5,14.5,75,80,81,80,78,75),
    #         col = colPlateau) #Plateau
    polygon(x = c(21,24,35,40,40,38,27,21), y = c(15,11,11,15,40,43,43,37), col = colFruits) #Compartiment HG
    polygon(x = c(41,57,57,55,43,41), y = c(12,12,36,39,39,36),col = colLegumes) #Compartiment HM
    polygon(x = c(58,62,75,78,78,78,60,58), y = c(13,11,11,15,40,44,44,37), col = colFeculents) #Compartiment HD
    polygon(x = c(23,25,43,45,39,37,35,25,23,22,22),y = c(49,45,44,48,65,75,77,77,75,73,71), col = colFeculents) #Compartiment BG
    polygon(x = c(48,56,58.5,59,74,76,76,74,43,41,40.5,40.5),y = c(42,42,44,46,46,48,74,77,77,73,71,70),col = colProt_Leg) #Compartiment BD
  }
  else if (data$Pays[i] == "USA"){
    
    
    plot(y~x,col = "white", ylim = rev(range(y)),axes = FALSE, xlab = NA, ylab = NA)            # Draw empty plot
    # rasterImage(img, 0, 100, 100, 0)
    # polygon(x = c(20.5,22,24,74,76,79,79.5,79,77,75.5,24,22,20.5), y = c(14.5,11.5,9,9,10,13.5,14.5,75,80,81,80,78,75),
    #         col = colPlateau) #Plateau
    polygon(x = c(21,24,35,40,40,38,27,21), y = c(15,11,11,15,40,43,43,37), col = colFeculents) #Compartiment HG
    polygon(x = c(41,57,57,55,43,41), y = c(12,12,36,39,39,36),col = colLegumes) #Compartiment HM
    polygon(x = c(58,62,75,78,78,78,60,58), y = c(13,11,11,15,40,44,44,37), col = colFruits) #Compartiment HD
    polygon(x = c(23,25,43,45,39,37,35,25,23,22,22),y = c(49,45,44,48,65,75,77,77,75,73,71), col = colFeculents) #Compartiment BG
    polygon(x = c(48,56,58.5,59,74,76,76,74,43,41,40.5,40.5),y = c(42,42,44,46,46,48,74,77,77,73,71,70),col = colProteines) #Compartiment BD
  }
  else if (data$Pays[i] == "Spain"){
    
    plot(y~x,col = "white", ylim = rev(range(y)),axes = FALSE, xlab = NA, ylab = NA)            # Draw empty plot
    # rasterImage(img, 0, 100, 100, 0)
    # polygon(x = c(20.5,22,24,74,76,79,79.5,79,77,75.5,24,22,20.5), y = c(14.5,11.5,9,9,10,13.5,14.5,75,80,81,80,78,75),
    #         col = colPlateau) #Plateau
    polygon(x = c(21,24,35,40,40,38,27,21), y = c(15,11,11,15,40,43,43,37), col = colFruits) #Compartiment HG
    polygon(x = c(41,57,57,55,43,41), y = c(12,12,36,39,39,36),col = colLegumes) #Compartiment HM
    polygon(x = c(58,62,75,78,78,78,60,58), y = c(13,11,11,15,40,44,44,37), col = colLegumes) #Compartiment HD
    polygon(x = c(23,25,43,45,39,37,35,25,23,22,22),y = c(49,45,44,48,65,75,77,77,75,73,71), col = colFeculents) #Compartiment BG
    polygon(x = c(48,56,58.5,59,74,76,76,74,43,41,40.5,40.5),y = c(42,42,44,46,46,48,74,77,77,73,71,70),col = colProt_Fec) #Compartiment BD
  } else {
    
    plot(y~x,col = "white", ylim = rev(range(y)),axes = FALSE, xlab = NA, ylab = NA)            # Draw empty plot
    # rasterImage(img, 0, 100, 100, 0)
    # polygon(x = c(20.5,22,24,74,76,79,79.5,79,77,75.5,24,22,20.5), y = c(14.5,11.5,9,9,10,13.5,14.5,75,80,81,80,78,75),
    #         col = colPlateau) #Plateau
    polygon(x = c(21,24,35,40,40,38,27,21), y = c(15,11,11,15,40,43,43,37), col = colLegumes) #Compartiment HG
    polygon(x = c(41,57,57,55,43,41), y = c(12,12,36,39,39,36),col = colLegumes) #Compartiment HM
    polygon(x = c(58,62,75,78,78,78,60,58), y = c(13,11,11,15,40,44,44,37), col = colProt_Leg) #Compartiment HD
    polygon(x = c(23,25,43,45,39,37,35,25,23,22,22),y = c(49,45,44,48,65,75,77,77,75,73,71), col = colLegumes) #Compartiment BG
    polygon(x = c(48,56,58.5,59,74,76,76,74,43,41,40.5,40.5),y = c(42,42,44,46,46,48,74,77,77,73,71,70),col = colProt_Fec) #Compartiment BD
  }
  
}

i <- 1

for (i in 1:nrow(data)){
  choice <- runif(1)
  if (data$Pays[i] == "Ukraine"){
    
    Leg <- (data$tz1[i] + data$tz4[i]) / data$tTot[i]
    Fec <- (data$tz2[i] + data$tz3[i])/ data$tTot[i]
    Prot_Fec <- data$tz5[i] / data$tTot[i]
    
    if (choice < 0.33){
      png(file = paste0("scriptR/test_couleurs/Pas_equilibre/",data$tester_id[i],"_Ukraine.png"),width = 980, height = 980)
    } 
    else if (choice > 0.67){
      png(file = paste0("scriptR/test_couleurs/Equilibre/",data$tester_id[i],"_Ukraine.png"),width = 980, height = 980)
    } else {
      png(file = paste0("scriptR/test_couleurs/Presque_equilibre/",data$tester_id[i],"_Ukraine.png"),width = 980, height = 980)
    }
   
    get_img(Pays = "Ukraine", AlFec = Fec, AlLeg = Leg, AlProt_Fec = Prot_Fec)
    dev.off()
  }
  else if (data$Pays[i] == "Brazil"){
    
    Leg <- data$tz2[i] / data$tTot[i]
    Fec <- (data$tz1[i] + data$tz5[i]) / data$tTot[i]
    Fru <- data$tz3[i] / data$tTot[i]
    Prot_Leg <- data$tz4[i] / data$tTot[i]
    
    if (choice < 0.33){
      png(file = paste0("scriptR/test_couleurs/Pas_equilibre/",data$tester_id[i],"_Brazil.png"),width = 980, height = 980)
    } 
    else if (choice > 0.67){
      png(file = paste0("scriptR/test_couleurs/Equilibre/",data$tester_id[i],"_Brazil.png"),width = 980, height = 980)
    } else {
      png(file = paste0("scriptR/test_couleurs/Presque_equilibre/",data$tester_id[i],"_Brazil.png"),width = 980, height = 980)
    }
    
    get_img(Pays = "Brazil", AlFec = Fec, AlLeg = Leg, AlProt_Leg = Prot_Leg, AlFru = Fru)
    dev.off()
  }
  else if (data$Pays[i] == "France"){
    
    Leg <- (data$tz2[i] + data$tz4[i]) / data$tTot[i]
    Prot <- data$tz3[i] / data$tTot[i]
    Fru <- data$tz1[i] / data$tTot[i]
    Lait_Fru <- data$tz5[i] / data$tTot[i]
    
    if (choice < 0.33){
      png(file = paste0("scriptR/test_couleurs/Pas_equilibre/",data$tester_id[i],"_France.png"),width = 980, height = 980)
    } 
    else if (choice > 0.67){
      png(file = paste0("scriptR/test_couleurs/Equilibre/",data$tester_id[i],"_France.png"),width = 980, height = 980)
    } else {
      png(file = paste0("scriptR/test_couleurs/Presque_equilibre/",data$tester_id[i],"_France.png"),width = 980, height = 980)
    }
    
    get_img(Pays = "France", AlFru = Fru, AlProt = Prot, AlLeg = Leg, AlLait_Fru = Lait_Fru)
    dev.off()
  }
  else if (data$Pays[i] == "Finland"){
    
    Fec_Fru <- data$tz5[i] / data$tTot[i]
    Leg <- (data$tz2[i] + data$tz3[i] + data$tz4[i]) / data$tTot[i]
    Fec <- data$tz1[i] / data$tTot[i]
    
    if (choice < 0.33){
      png(file = paste0("scriptR/test_couleurs/Pas_equilibre/",data$tester_id[i],"_Finland.png"),width = 980, height = 980)
    } 
    else if (choice > 0.67){
      png(file = paste0("scriptR/test_couleurs/Equilibre/",data$tester_id[i],"_Finland.png"),width = 980, height = 980)
    } else {
      png(file = paste0("scriptR/test_couleurs/Presque_equilibre/",data$tester_id[i],"_Finland.png"),width = 980, height = 980)
    }
    
    get_img(Pays = "Finland", AlFec = Fec, AlLeg = Leg, AlFec_Fru = Fec_Fru)
    dev.off()
  }
  else if (data$Pays[i] == "Greece"){
    
    Lait_Fru <- data$tz1[i] / data$tTot[i]
    Leg <- data$tz2[i] / data$tTot[i]
    Fec <- data$tz3[i] / data$tTot[i]
    Fru <- data$tz4[i] / data$tTot[i]
    Prot_Fec <- data$tz5[i] / data$tTot[i]
    
    if (choice < 0.33){
      png(file = paste0("scriptR/test_couleurs/Pas_equilibre/",data$tester_id[i],"_Greece.png"),width = 980, height = 980)
    } 
    else if (choice > 0.67){
      png(file = paste0("scriptR/test_couleurs/Equilibre/",data$tester_id[i],"_Greece.png"),width = 980, height = 980)
    } else {
      png(file = paste0("scriptR/test_couleurs/Presque_equilibre/",data$tester_id[i],"_Greece.png"),width = 980, height = 980)
    }
    
    get_img(Pays = "Greece", AlFec = Fec, AlLeg = Leg, AlProt_Fec = Prot_Fec, AlLait_Fru = Lait_Fru, AlFru = Fru)
    dev.off()
  }
  else if (data$Pays[i] == "Italy"){
    
    Leg <- data$tz2[i] / data$tTot[i]
    Fec <- (data$tz3[i] + data$tz4[i]) / data$tTot[i]
    Fru <- data$tz1[i] / data$tTot[i]
    Prot_Leg <- data$tz5[i] / data$tTot[i]
    
    if (choice < 0.33){
      png(file = paste0("scriptR/test_couleurs/Pas_equilibre/",data$tester_id[i],"_Italy.png"),width = 980, height = 980)
    } 
    else if (choice > 0.67){
      png(file = paste0("scriptR/test_couleurs/Equilibre/",data$tester_id[i],"_Italy.png"),width = 980, height = 980)
    } else {
      png(file = paste0("scriptR/test_couleurs/Presque_equilibre/",data$tester_id[i],"_Italy.png"),width = 980, height = 980)
    }
    
    get_img(Pays = "Italy", AlFec = Fec, AlLeg = Leg, AlProt_Leg = Prot_Leg, AlFru = Fru)
    dev.off()
  }
  else if (data$Pays[i] == "USA"){
    
    Leg <- data$tz2[i] / data$tTot[i]
    Fec <- (data$tz1[i] + data$tz4[i]) / data$tTot[i]
    Fru <- data$tz3[i] / data$tTot[i]
    Prot <- data$tz5[i] / data$tTot[i]
    
    if (choice < 0.33){
      png(file = paste0("scriptR/test_couleurs/Pas_equilibre/",data$tester_id[i],"_USA.png"),width = 980, height = 980)
    } 
    else if (choice > 0.67){
      png(file = paste0("scriptR/test_couleurs/Equilibre/",data$tester_id[i],"_USA.png"),width = 980, height = 980)
    } else {
      png(file = paste0("scriptR/test_couleurs/Presque_equilibre/",data$tester_id[i],"_USA.png"),width = 980, height = 980)
    }
    
    get_img(Pays = "USA", AlFec = Fec, AlLeg = Leg, AlFru = Fru, AlProt = Prot)
    dev.off()
  }
  else if (data$Pays[i] == "Spain"){
    
    Leg <- (data$tz2[i] + data$tz3[i]) / data$tTot[i]
    Fec <- data$tz4[i] / data$tTot[i]
    Fru <- data$tz1[i] / data$tTot[i]
    Prot_Fec <- data$tz5[i] / data$tTot[i]
    
    if (choice < 0.33){
      png(file = paste0("scriptR/test_couleurs/Pas_equilibre/",data$tester_id[i],"_Spain.png"),width = 980, height = 980)
    } 
    else if (choice > 0.67){
      png(file = paste0("scriptR/test_couleurs/Equilibre/",data$tester_id[i],"_Spain.png"),width = 980, height = 980)
    } else {
      png(file = paste0("scriptR/test_couleurs/Presque_equilibre/",data$tester_id[i],"_Spain.png"),width = 980, height = 980)
    }
    
    get_img(Pays = "Spain", AlFec = Fec, AlLeg = Leg, AlProt_Fec = Prot_Fec, AlFru = Fru)
    dev.off()
  } else {
    
    Leg <- (data$tz1[i] + data$tz2[i] + data$tz4[i]) / data$tTot[i]
    Prot_Leg <- data$tz3[i] / data$tTot[i]
    Prot_Fec <- data$tz5[i] / data$tTot[i]
    
    if (choice < 0.33){
      png(file = paste0("scriptR/test_couleurs/Pas_equilibre/",data$tester_id[i],"_South_Korea.png"),width = 980, height = 980)
    } 
    else if (choice > 0.67){
      png(file = paste0("scriptR/test_couleurs/Equilibre/",data$tester_id[i],"_South_Korea.png"),width = 980, height = 980)
    } else {
      png(file = paste0("scriptR/test_couleurs/Presque_equilibre/",data$tester_id[i],"_South_Korea.png"),width = 980, height = 980)
    }
    
    get_img(Pays = "South_Korea", AlLeg = Leg, AlProt_Leg = Prot_Leg, AlProt_Fec = Prot_Fec)
    dev.off()
  }
}




