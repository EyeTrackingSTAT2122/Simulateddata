#### Code R Final ####

#### Libraries ####

library(randomForest) 
library(tidyverse)
library(ggplot2)
library(jpeg) #Pour les images en arrière plan
library(ggpubr) #Pour les images en arrière plan
library(grid) #Pour les images en arrière plan
library(sjmisc) #Pour rechercher un terme précis
library(readr) #Pour intégrer les jeu de données csv
library(FactoMineR) 
library(nnet) #Pour la regression multinomiale
library(Rcmdr)
library(factoextra) #
library(rpart) #Pour les arbres de décisions
library(readxl) #Pour intégrer les jeu de données xlsx
library(caTools)
library(tm)
library(SnowballC)

#####

#### Importation des données ####

implicites <- read_csv("E:/Master 2/Simulateddata/data/data_fix_finales/data_finales_supp_classes.csv") #données de regard
implicites <- implicites[-c(403:432),] #Retrait des individus avec données manquantes (données retirées parce qu'elles n'étaient pas traitables)

donnees_heatmap <- 

zone <- read.csv("E:/Master 2/Simulateddata/data/data_fix_finales/fixations.csv")

explicites <- read_xlsx("E:/Master 2/Simulateddata/data/data_explicites/Donnees_explicites.xlsx") #données de verbalisation

donnees_AFM <- read.csv("E:/Master 2/Simulateddata/data/data_fix_finales/data_AFM.csv")


#####

#### Fonctions ####

# Créé un jeu de données test et un jeu de données train

get_train_test <- function(data){
  
  tirage <- sample(nrow(data),1,replace = FALSE)
  
  train <<- data[-tirage,]
  test <<- data[tirage,]
  
}


# Effectue un random forest sur le jeu de données explicites

do_RF_explicit <- function(rep = 2, ntree = 200, mtry = 2) {
  acc <<- rep(0,rep)
  
  for (i in 1:rep){
    
    get_train_test(tSparse)
    
    RF_model <- randomForest(Classe ~ ., data=train)
    
    RF_test <- randomForest(as.data.frame(train[,-48]),y = train[,48] ,xtest = as.data.frame(test[,-48]), ytest = test[,48], ntree =  ntree, mtry= mtry)
    
    table <- RF_test$test$confusion
    
    acc[i] <<- sum(table[1,1], table[2,2], table[3,3])/nrow(test)
  }
  print(c(mean(acc)*100,"% accuracy"))
  return (mean(acc)*100)
}

do_RF_implicit <- function(nrep = 2, ntree = 200, mtry = 2){
  
  Pays <- c("Ukraine","France","Greece","Brazil","South Korea","Greece","Finland","Spain","Italy")

  accuracy_test <<- rep(0,nrep)
  
  
  for (k in 1:nrep){
    get_train_test(RF_implicite)
    
    test_RF <- randomForest(x = as.data.frame(train[,-5]),y = train[,5] ,xtest = as.data.frame(test[,-5]), ytest = test[,5], ntree = ntree, mtry= mtry)
    
    observed_test <- test[,5]
    predicted_test <- test_RF$test$predicted
    
    cont <- table(observed_test,predicted_test)
    
    accuracy_test[k] <<- sum(cont[1,1],cont[2,2],cont[3,3])/nrow(test)
  }
  
  plot(accuracy_test, type = 'l', ylim = c(0,1), xlab = "Essai N°", ylab = "Accuracy", xlim = c(1,nrep))
  # lines(accuracy_train, col = "red")
  # lines(1:100,rep(0.5,100), col = "blue")
  print(c(mean(accuracy_test),"% accuracy"))
}

# Obtenir une image avec les composantes nutritionnelles fixées

get_img <- function(Pays, AlFec = 0, AlLeg = 0, AlProt = 0, AlFru = 0, AlProt_Fec = 0, AlProt_Leg = 0, AlFec_Fru = 0, AlLait_Fru = 0){
  
  par(mar=c(0,0,0,0))
  
  x <- c(0,4)
  y <- c(0,2)
  
  colPlateau <- rgb(220/255,220/255,220/255)
  colFeculents <- rgb(1,0,0, AlFec)
  colLegumes <- rgb(0,1,0, AlLeg)
  colProteines <- rgb(0,0,1, AlProt)
  colFruits <- rgb(0,0,0, AlFru)
  
  colProt_Fec <- rgb(1,0,1, AlProt_Fec)
  colProt_Leg <- rgb(0,1,1, AlProt_Leg)
  colFec_Fru <- rgb(1,1,0, AlFec_Fru)
  # colLait_Fru <- rgb(1,1,0, AlLait_Fru)
  
  plot(y~x,col = "white",axes = FALSE, xlab = NA, ylab = NA, xlim= c(0,4), xaxs = "i", yaxs = "i")            # Draw empty plot
  
  rect(0,0,1,1, border = NA, col = colFeculents)
  rect(1,0,2,1, border = NA, col = colLegumes)
  rect(2,0,3,1, border = NA, col = colProteines)
  rect(3,0,4,1, border = NA, col = colFruits)
  
  rect(0,1,1.33,2, border = NA, col = colProt_Fec)
  rect(1.33,1,2.66,2, border = NA, col = colProt_Leg)
  rect(2.66,1,4,2, border = NA, col = colFec_Fru)
  
  # rect(0,1,1,2, border = NA, col = colProt_Fec)
  # rect(1,1,2,2, border = NA, col = colProt_Leg)
  # rect(2,1,3,2, border = NA, col = colFec_Fru)
  # rect(3,1,4,2, border = NA, col = colLait_Fru)
  
}

# Obtenir une image avec les compartiments du plateau représentés par leur composante nutritionnelle

get_img_plateau <- function(Pays, AlFec = 0, AlLeg = 0, AlProt = 0, AlFru = 0, AlProt_Fec = 0, AlProt_Leg = 0, AlFec_Fru = 0, AlLait_Fru = 0){
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



#####

#### Analyses ####

#AFM ----



## Données explicites ----

#Random forest----

t1 <- explicites[,c(2:3,5,10)]

glimpse(t1)  

corpus = Corpus(VectorSource(t1$Comm_net))

frequencies = DocumentTermMatrix(corpus)

sparse = removeSparseTerms(frequencies, 0.995)

tSparse = as.data.frame(as.matrix(sparse))

colnames(tSparse) = make.names(colnames(tSparse))

tSparse$Classe = t1$Classe ; tSparse$Classe <- as.factor(tSparse$Classe)

prop.table(table(tSparse$Classe))

Accuracy_explicit <- do_RF_explicit(10)

#Print accuracy du random forest avec un jeu de données train en n-1

#Régression logistique multinomiale


#####

## Données implicites ----

#Random forest

RF_implicite <- as.data.frame(implicites[,c(2,5,16:19,27)])

name <- paste0(RF_implicite$id,"_",RF_implicite$pays)

rownames(RF_implicite) <- name

RF_implicite$classe <- as.factor(RF_implicite$classe)

RF_implicite <- RF_implicite[,-c(1,2)]

do_RF_implicit(nrep = 10)

#Régression logistique multinomiale

#####

#### Visualisation ####
## Données explicites ----

#####

## Données implicites ----

#####

# Arbre de décision----

Decision_tree <- as.data.frame(implicites[,c(2,5,16:19,27)])

rownames(Decision_tree) <- name

Decision_tree <- Decision_tree[,-c(1,2)]

cart <- rpart(as.factor(Classe)~., data=train, method = 'class', minsplit = 2)

plot(cart, uniform=TRUE, main="Classification Tree", xlim = c(0,15), ylim = c(0,2))
text(cart, cex = 0.8)

#####

# Représentation des données implicites---- #Changer (datas) dans la visu

#Heatmap avec fond ----


#####

#Heatmap avec fond Uni ----


#####

#Plateau simplifié ----

for (i in 1:nrow(data)){
  if (data$Pays[i] == "Ukraine"){
    
    Leg <- (data$tz1[i] + data$tz4[i]) / data$tTot[i]
    Fec <- (data$tz2[i] + data$tz3[i])/ data$tTot[i]
    Prot_Fec <- data$tz5[i] / data$tTot[i]
    
    get_img_plateau(Leg = Leg, Fec = Fec, Prot_Fec = Prot_Fec)
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("zone_max/train/Pas_equilibre")) < 5){
        png(file = paste0("zone_max/train/Pas_equilibre/",data$id_tester[i],"_Ukraine.png"),width = 980, height = 980)
      } else {
        png(file = paste0("zone_max/test/Pas_equilibre/",data$id_tester[i],"_Ukraine.png"),width = 980, height = 980)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("zone_max/train/Equilibre")) < 5){
        png(file = paste0("zone_max/train/Equilibre/",data$id_tester[i],"_Ukraine.png"),width = 980, height = 980)
      } else {
        png(file = paste0("zone_max/test/Equilibre/",data$id_tester[i],"_Ukraine.png"),width = 980, height = 980)
      }
    } else {
      if (length(list.files("zone_max/train/Presque_equilibre")) < 5){
        png(file = paste0("zone_max/train/Presque_equilibre/",data$id_tester[i],"_Ukraine.png"),width = 980, height = 980)
      } else {
        png(file = paste0("zone_max/test/Presque_equilibre/",data$id_tester[i],"_Ukraine.png"),width = 980, height = 980)
      }
    }
    
    get_img(Pays = "Ukraine", AlFec = AlFec, AlLeg = AlLeg, AlProt_Fec = AlProt_Fec)
    dev.off()
  }
  else if (data$Pays[i] == "Brazil"){
    
    Leg <- data$tz2[i] / data$tTot[i]
    Fec <- (data$tz1[i] + data$tz5[i]) / data$tTot[i]
    Fru <- data$tz3[i] / data$tTot[i]
    Prot_Leg <- data$tz4[i] / data$tTot[i]
    
    get_img_plateau(Leg = Leg, Fec = Fec, Fru = Fru, Prot_Leg = Prot_Leg)
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("zone_max/train/Pas_equilibre")) < 40){
        png(file = paste0("zone_max/train/Pas_equilibre/",data$id_tester[i],"_Brazil.png"),width = 980, height = 980)
      } else {
        png(file = paste0("zone_max/test/Pas_equilibre/",data$id_tester[i],"_Brazil.png"),width = 980, height = 980)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("zone_max/train/Equilibre")) < 40){
        png(file = paste0("zone_max/train/Equilibre/",data$id_tester[i],"_Brazil.png"),width = 980, height = 980)
      } else {
        png(file = paste0("zone_max/test/Equilibre/",data$id_tester[i],"_Brazil.png"),width = 980, height = 980)
      }
    } else {
      if (length(list.files("zone_max/train/Presque_equilibre")) < 40){
        png(file = paste0("zone_max/train/Presque_equilibre/",data$id_tester[i],"_Brazil.png"),width = 980, height = 980)
      } else {
        png(file = paste0("zone_max/test/Presque_equilibre/",data$id_tester[i],"_Brazil.png"),width = 980, height = 980)
      }
    }
    
    get_img(Pays = "Brazil", AlFec = AlFec, AlLeg = AlLeg, AlProt_Leg = AlProt_Leg, AlFru = AlFru)
    dev.off()
  }
  else if (data$Pays[i] == "France"){
    
    Leg <- (data$tz2[i] + data$tz4[i]) / data$tTot[i]
    Prot <- data$tz3[i] / data$tTot[i]
    Fru <- data$tz1[i] / data$tTot[i]
    Lait_Fru <- data$tz5[i] / data$tTot[i]
    
    get_img_plateau(Leg = Leg, Fru = Fru, Prot = Prot, Lait_Fru = Lait_Fru)
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("zone_max/train/Pas_equilibre")) < 20){
        png(file = paste0("zone_max/train/Pas_equilibre/",data$id_tester[i],"_France.png"),width = 980, height = 980)
      } else {
        png(file = paste0("zone_max/test/Pas_equilibre/",data$id_tester[i],"_France.png"),width = 980, height = 980)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("zone_max/train/Equilibre")) < 20){
        png(file = paste0("zone_max/train/Equilibre/",data$id_tester[i],"_France.png"),width = 980, height = 980)
      } else {
        png(file = paste0("zone_max/test/Equilibre/",data$id_tester[i],"_France.png"),width = 980, height = 980)
      }
    } else {
      if (length(list.files("zone_max/train/Presque_equilibre")) < 20){
        png(file = paste0("zone_max/train/Presque_equilibre/",data$id_tester[i],"_France.png"),width = 980, height = 980)
      } else {
        png(file = paste0("zone_max/test/Presque_equilibre/",data$id_tester[i],"_France.png"),width = 980, height = 980)
      }
    }
    
    get_img(Pays = "France", AlFru = AlFru, AlProt = AlProt, AlLeg = AlLeg, AlLait_Fru = AlLait_Fru)
    dev.off()
  }
  else if (data$Pays[i] == "Finland"){
    
    Fec_Fru <- data$tz5[i] / data$tTot[i]
    Leg <- (data$tz2[i] + data$tz3[i] + data$tz4[i]) / data$tTot[i]
    Fec <- data$tz1[i] / data$tTot[i]
    
    get_img_plateau(Leg = Leg, Fec_Fru = Fec_Fru, Fec = Fec)
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("zone_max/train/Pas_equilibre")) < 10){
        png(file = paste0("zone_max/train/Pas_equilibre/",data$id_tester[i],"_Finland.png"),width = 980, height = 980)
      } else {
        png(file = paste0("zone_max/test/Pas_equilibre/",data$id_tester[i],"_Finland.png"),width = 980, height = 980)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("zone_max/train/Equilibre")) < 10){
        png(file = paste0("zone_max/train/Equilibre/",data$id_tester[i],"_Finland.png"),width = 980, height = 980)
      } else {
        png(file = paste0("zone_max/test/Equilibre/",data$id_tester[i],"_Finland.png"),width = 980, height = 980)
      }
    } else {
      if (length(list.files("zone_max/train/Presque_equilibre")) < 10){
        png(file = paste0("zone_max/train/Presque_equilibre/",data$id_tester[i],"_Finland.png"),width = 980, height = 980)
      } else {
        png(file = paste0("zone_max/test/Presque_equilibre/",data$id_tester[i],"_Finland.png"),width = 980, height = 980)
      }
    }
    
    get_img(Pays = "Finland", AlFec = AlFec, AlLeg = AlLeg, AlFec_Fru = AlFec_Fru)
    dev.off()
  }
  else if (data$Pays[i] == "Greece"){
    
    Lait_Fru <- data$tz1[i] / data$tTot[i]
    Leg <- data$tz2[i] / data$tTot[i]
    Fec <- data$tz3[i] / data$tTot[i]
    Fru <- data$tz4[i] / data$tTot[i]
    Prot_Fec <- data$tz5[i] / data$tTot[i]
    
    get_img_plateau(Leg = Leg, Fec = Fec, Prot_Fec = Prot_Fec, Lait_Fru = Lait_Fru, Fru = Fru)
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("zone_max/train/Pas_equilibre")) < 45){
        png(file = paste0("zone_max/train/Pas_equilibre/",data$id_tester[i],"_Greece.png"),width = 980, height = 980)
      } else {
        png(file = paste0("zone_max/test/Pas_equilibre/",data$id_tester[i],"_Greece.png"),width = 980, height = 980)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("zone_max/train/Equilibre")) < 45){
        png(file = paste0("zone_max/train/Equilibre/",data$id_tester[i],"_Greece.png"),width = 980, height = 980)
      } else {
        png(file = paste0("zone_max/test/Equilibre/",data$id_tester[i],"_Greece.png"),width = 980, height = 980)
      }
    } else {
      if (length(list.files("zone_max/train/Presque_equilibre")) < 45){
        png(file = paste0("zone_max/train/Presque_equilibre/",data$id_tester[i],"_Greece.png"),width = 980, height = 980)
      } else {
        png(file = paste0("zone_max/test/Presque_equilibre/",data$id_tester[i],"_Greece.png"),width = 980, height = 980)
      }
    }
    
    get_img(Pays = "Greece", AlFec = AlFec, AlLeg = AlLeg, AlProt_Fec = AlProt_Fec, AlLait_Fru = AlLait_Fru, AlFru = AlFru)
    dev.off()
  }
  else if (data$Pays[i] == "Italy"){
    
    Leg <- data$tz2[i] / data$tTot[i]
    Fec <- (data$tz3[i] + data$tz4[i]) / data$tTot[i]
    Fru <- data$tz1[i] / data$tTot[i]
    Prot_Leg <- data$tz5[i] / data$tTot[i]
    
    get_img_plateau(Leg = Leg, Fec = Fec, Fru = Fru, Prot_Leg)
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("zone_max/train/Pas_equilibre")) < 35){
        png(file = paste0("zone_max/train/Pas_equilibre/",data$id_tester[i],"_Italy.png"),width = 980, height = 980)
      } else {
        png(file = paste0("zone_max/test/Pas_equilibre/",data$id_tester[i],"_Italy.png"),width = 980, height = 980)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("zone_max/train/Equilibre")) < 35){
        png(file = paste0("zone_max/train/Equilibre/",data$id_tester[i],"_Italy.png"),width = 980, height = 980)
      } else {
        png(file = paste0("zone_max/test/Equilibre/",data$id_tester[i],"_Italy.png"),width = 980, height = 980)
      }
    } else {
      if (length(list.files("zone_max/train/Presque_equilibre")) < 35){
        png(file = paste0("zone_max/train/Presque_equilibre/",data$id_tester[i],"_Italy.png"),width = 980, height = 980)
      } else {
        png(file = paste0("zone_max/test/Presque_equilibre/",data$id_tester[i],"_Italy.png"),width = 980, height = 980)
      }
    }
    
    get_img(Pays = "Italy", AlFec = AlFec, AlLeg = AlLeg, AlProt_Leg = AlProt_Leg, AlFru = AlFru)
    dev.off()
  }
  else if (data$Pays[i] == "USA"){
    
    Leg <- data$tz2[i] / data$tTot[i]
    Fec <- (data$tz1[i] + data$tz4[i]) / data$tTot[i]
    Fru <- data$tz3[i] / data$tTot[i]
    Prot <- data$tz5[i] / data$tTot[i]
    
    get_img_plateau(Leg = Leg, Fec = Fec, Prot = Prot, Fru = Fru)
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("zone_max/train/Pas_equilibre")) < 30){
        png(file = paste0("zone_max/train/Pas_equilibre/",data$id_tester[i],"_USA.png"),width = 980, height = 980)
      } else {
        png(file = paste0("zone_max/test/Pas_equilibre/",data$id_tester[i],"_USA.png"),width = 980, height = 980)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("zone_max/train/Equilibre")) < 30){
        png(file = paste0("zone_max/train/Equilibre/",data$id_tester[i],"_USA.png"),width = 980, height = 980)
      } else {
        png(file = paste0("zone_max/test/Equilibre/",data$id_tester[i],"_USA.png"),width = 980, height = 980)
      }
    } else {
      if (length(list.files("zone_max/train/Presque_equilibre")) < 30){
        png(file = paste0("zone_max/train/Presque_equilibre/",data$id_tester[i],"_USA.png"),width = 980, height = 980)
      } else {
        png(file = paste0("zone_max/test/Presque_equilibre/",data$id_tester[i],"_USA.png"),width = 980, height = 980)
      }
    }
    
    get_img(Pays = "USA", AlFec = AlFec, AlLeg = AlLeg, AlFru = AlFru, AlProt = AlProt)
    dev.off()
  }
  else if (data$Pays[i] == "Spain"){
    
    Leg <- (data$tz2[i] + data$tz3[i]) / data$tTot[i]
    Fec <- data$tz4[i] / data$tTot[i]
    Fru <- data$tz1[i] / data$tTot[i]
    Prot_Fec <- data$tz5[i] / data$tTot[i]
    
    get_img_plateau(Leg = Leg, Fec = Fec, Prot_Fec = Prot_Fec, Fru = Fru)
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("zone_max/train/Pas_equilibre")) < 15){
        png(file = paste0("zone_max/train/Pas_equilibre/",data$id_tester[i],"_Spain.png"),width = 980, height = 980)
      } else {
        png(file = paste0("zone_max/test/Pas_equilibre/",data$id_tester[i],"_Spain.png"),width = 980, height = 980)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("zone_max/train/Equilibre")) < 15){
        png(file = paste0("zone_max/train/Equilibre/",data$id_tester[i],"_Spain.png"),width = 980, height = 980)
      } else {
        png(file = paste0("zone_max/test/Equilibre/",data$id_tester[i],"_Spain.png"),width = 980, height = 980)
      }
    } else {
      if (length(list.files("zone_max/train/Presque_equilibre")) < 15){
        png(file = paste0("zone_max/train/Presque_equilibre/",data$id_tester[i],"_Spain.png"),width = 980, height = 980)
      } else {
        png(file = paste0("zone_max/test/Presque_equilibre/",data$id_tester[i],"_Spain.png"),width = 980, height = 980)
      }
    }
    
    get_img(Pays = "Spain", AlFec = AlFec, AlLeg = AlLeg, AlProt_Fec = AlProt_Fec, AlFru = AlFru)
    dev.off()
  } else {
    
    Leg <- (data$tz1[i] + data$tz2[i] + data$tz4[i]) / data$tTot[i]
    Prot_Leg <- data$tz3[i] / data$tTot[i]
    Prot_Fec <- data$tz5[i] / data$tTot[i]
    
    get_img_plateau(Leg = Leg, Prot_Fec = Prot_Fec, Prot_Leg = Prot_Leg)
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("zone_max/train/Pas_equilibre")) < 25){
        png(file = paste0("zone_max/train/Pas_equilibre/",data$id_tester[i],"_South_Korea.png"),width = 980, height = 980)
      } else {
        png(file = paste0("zone_max/test/Pas_equilibre/",data$id_tester[i],"_South_Korea.png"),width = 980, height = 980)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("zone_max/train/Equilibre")) < 25){
        png(file = paste0("zone_max/train/Equilibre/",data$id_tester[i],"_South_Korea.png"),width = 980, height = 980)
      } else {
        png(file = paste0("zone_max/test/Equilibre/",data$id_tester[i],"_South_Korea.png"),width = 980, height = 980)
      }
    } else {
      if (length(list.files("zone_max/train/Presque_equilibre")) < 25){
        png(file = paste0("zone_max/train/Presque_equilibre/",data$id_tester[i],"_South_Korea.png"),width = 980, height = 980)
      } else {
        png(file = paste0("zone_max/test/Presque_equilibre/",data$id_tester[i],"_South_Korea.png"),width = 980, height = 980)
      }
    }
    
    get_img(Pays = "South_Korea", AlLeg = AlLeg, AlProt_Leg = AlProt_Leg, AlProt_Fec = AlProt_Fec)
    dev.off()
  }
}
 

#####

#Zone fixes avec plusieurs composantes nutritionnelles ----

for (i in 1:nrow(data)){
  if (data$Pays[i] == "Ukraine"){
    
    Leg <- (data$tz1[i] + data$tz4[i]) / data$tTot[i]
    Fec <- (data$tz2[i] + data$tz3[i])/ data$tTot[i]
    Prot_Fec <- data$tz5[i] / data$tTot[i]
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe/Couleurs/train/Pas_equilibre")) < 5){
        png(file = paste0("fixe/Couleurs/train/Pas_equilibre/",data$id_tester[i],"_Ukraine.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/Couleurs/test/Pas_equilibre/",data$id_tester[i],"_Ukraine.png"),width = 980, height = 490)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("fixe/Couleurs/train/Equilibre")) < 5){
        png(file = paste0("fixe/Couleurs/train/Equilibre/",data$id_tester[i],"_Ukraine.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/Couleurs/test/Equilibre/",data$id_tester[i],"_Ukraine.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe/Couleurs/train/Presque_equilibre")) < 5){
        png(file = paste0("fixe/Couleurs/train/Presque_equilibre/",data$id_tester[i],"_Ukraine.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/Couleurs/test/Presque_equilibre/",data$id_tester[i],"_Ukraine.png"),width = 980, height = 490)
      }
    }
    
    get_img(Pays = "Ukraine", AlFec = Fec, AlLeg = Leg, AlProt_Fec = Prot_Fec)
    dev.off()
  }
  else if (data$Pays[i] == "Brazil"){
    
    Leg <- data$tz2[i] / data$tTot[i]
    Fec <- (data$tz1[i] + data$tz5[i]) / data$tTot[i]
    Fru <- data$tz3[i] / data$tTot[i]
    Prot_Leg <- data$tz4[i] / data$tTot[i]
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe/Couleurs/train/Pas_equilibre")) < 40){
        png(file = paste0("fixe/Couleurs/train/Pas_equilibre/",data$id_tester[i],"_Brazil.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/Couleurs/test/Pas_equilibre/",data$id_tester[i],"_Brazil.png"),width = 980, height = 490)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("fixe/Couleurs/train/Equilibre")) < 40){
        png(file = paste0("fixe/Couleurs/train/Equilibre/",data$id_tester[i],"_Brazil.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/Couleurs/test/Equilibre/",data$id_tester[i],"_Brazil.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe/Couleurs/train/Presque_equilibre")) < 40){
        png(file = paste0("fixe/Couleurs/train/Presque_equilibre/",data$id_tester[i],"_Brazil.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/Couleurs/test/Presque_equilibre/",data$id_tester[i],"_Brazil.png"),width = 980, height = 490)
      }
    }
    
    get_img(Pays = "Brazil", AlFec = Fec, AlLeg = Leg, AlProt_Leg = Prot_Leg, AlFru = Fru)
    dev.off()
  }
  else if (data$Pays[i] == "France"){
    
    Leg <- (data$tz2[i] + data$tz4[i]) / data$tTot[i]
    Prot <- (data$tz3[i] + data$tz5[i]) / data$tTot[i]
    # Prot <- data$tz3[i] / data$tTot[i]
    Fru <- data$tz1[i] / data$tTot[i]
    # Lait_Fru <- data$tz5[i] / data$tTot[i]
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe/Couleurs/train/Pas_equilibre")) < 20){
        png(file = paste0("fixe/Couleurs/train/Pas_equilibre/",data$id_tester[i],"_France.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/Couleurs/test/Pas_equilibre/",data$id_tester[i],"_France.png"),width = 980, height = 490)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("fixe/Couleurs/train/Equilibre")) < 20){
        png(file = paste0("fixe/Couleurs/train/Equilibre/",data$id_tester[i],"_France.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/Couleurs/test/Equilibre/",data$id_tester[i],"_France.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe/Couleurs/train/Presque_equilibre")) < 20){
        png(file = paste0("fixe/Couleurs/train/Presque_equilibre/",data$id_tester[i],"_France.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/Couleurs/test/Presque_equilibre/",data$id_tester[i],"_France.png"),width = 980, height = 490)
      }
    }
    
    get_img(Pays = "France", AlFru = Fru, AlProt = Prot, AlLeg = Leg) #, AlLait_Fru = Lait_Fru)
    dev.off()
  }
  else if (data$Pays[i] == "Finland"){
    
    Fec_Fru <- data$tz5[i] / data$tTot[i]
    Leg <- (data$tz2[i] + data$tz3[i] + data$tz4[i]) / data$tTot[i]
    Fec <- data$tz1[i] / data$tTot[i]
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe/Couleurs/train/Pas_equilibre")) < 10){
        png(file = paste0("fixe/Couleurs/train/Pas_equilibre/",data$id_tester[i],"_Finland.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/Couleurs/test/Pas_equilibre/",data$id_tester[i],"_Finland.png"),width = 980, height = 490)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("fixe/Couleurs/train/Equilibre")) < 10){
        png(file = paste0("fixe/Couleurs/train/Equilibre/",data$id_tester[i],"_Finland.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/Couleurs/test/Equilibre/",data$id_tester[i],"_Finland.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe/Couleurs/train/Presque_equilibre")) < 10){
        png(file = paste0("fixe/Couleurs/train/Presque_equilibre/",data$id_tester[i],"_Finland.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/Couleurs/test/Presque_equilibre/",data$id_tester[i],"_Finland.png"),width = 980, height = 490)
      }
    }
    
    get_img(Pays = "Finland", AlFec = Fec, AlLeg = Leg, AlFec_Fru = Fec_Fru)
    dev.off()
  }
  else if (data$Pays[i] == "Greece"){
    
    # Lait_Fru <- data$tz1[i] / data$tTot[i]
    Prot <- data$tz1[i] / data$tTot[i]
    Leg <- data$tz2[i] / data$tTot[i]
    Fec <- data$tz3[i] / data$tTot[i]
    Fru <- data$tz4[i] / data$tTot[i]
    Prot_Fec <- data$tz5[i] / data$tTot[i]
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe/Couleurs/train/Pas_equilibre")) < 45){
        png(file = paste0("fixe/Couleurs/train/Pas_equilibre/",data$id_tester[i],"_Greece.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/Couleurs/test/Pas_equilibre/",data$id_tester[i],"_Greece.png"),width = 980, height = 490)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("fixe/Couleurs/train/Equilibre")) < 45){
        png(file = paste0("fixe/Couleurs/train/Equilibre/",data$id_tester[i],"_Greece.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/Couleurs/test/Equilibre/",data$id_tester[i],"_Greece.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe/Couleurs/train/Presque_equilibre")) < 45){
        png(file = paste0("fixe/Couleurs/train/Presque_equilibre/",data$id_tester[i],"_Greece.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/Couleurs/test/Presque_equilibre/",data$id_tester[i],"_Greece.png"),width = 980, height = 490)
      }
    }
    
    get_img(Pays = "Greece", AlFec = Fec, AlLeg = Leg, AlProt_Fec = Prot_Fec, AlFru = Fru, AlProt = Prot)#, AlLait_Fru = Lait_Fru)
    dev.off()
  }
  else if (data$Pays[i] == "Italy"){
    
    Leg <- data$tz2[i] / data$tTot[i]
    Fec <- (data$tz3[i] + data$tz4[i]) / data$tTot[i]
    Fru <- data$tz1[i] / data$tTot[i]
    Prot_Leg <- data$tz5[i] / data$tTot[i]
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe/Couleurs/train/Pas_equilibre")) < 35){
        png(file = paste0("fixe/Couleurs/train/Pas_equilibre/",data$id_tester[i],"_Italy.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/Couleurs/test/Pas_equilibre/",data$id_tester[i],"_Italy.png"),width = 980, height = 490)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("fixe/Couleurs/train/Equilibre")) < 35){
        png(file = paste0("fixe/Couleurs/train/Equilibre/",data$id_tester[i],"_Italy.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/Couleurs/test/Equilibre/",data$id_tester[i],"_Italy.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe/Couleurs/train/Presque_equilibre")) < 35){
        png(file = paste0("fixe/Couleurs/train/Presque_equilibre/",data$id_tester[i],"_Italy.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/Couleurs/test/Presque_equilibre/",data$id_tester[i],"_Italy.png"),width = 980, height = 490)
      }
    }
    
    get_img(Pays = "Italy", AlFec = Fec, AlLeg = Leg, AlProt_Leg = Prot_Leg, AlFru = Fru)
    dev.off()
  }
  else if (data$Pays[i] == "USA"){
    
    Leg <- data$tz2[i] / data$tTot[i]
    Fec <- (data$tz1[i] + data$tz4[i]) / data$tTot[i]
    Fru <- data$tz3[i] / data$tTot[i]
    Prot <- data$tz5[i] / data$tTot[i]
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe/Couleurs/train/Pas_equilibre")) < 30){
        png(file = paste0("fixe/Couleurs/train/Pas_equilibre/",data$id_tester[i],"_USA.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/Couleurs/test/Pas_equilibre/",data$id_tester[i],"_USA.png"),width = 980, height = 490)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("fixe/Couleurs/train/Equilibre")) < 30){
        png(file = paste0("fixe/Couleurs/train/Equilibre/",data$id_tester[i],"_USA.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/Couleurs/test/Equilibre/",data$id_tester[i],"_USA.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe/Couleurs/train/Presque_equilibre")) < 30){
        png(file = paste0("fixe/Couleurs/train/Presque_equilibre/",data$id_tester[i],"_USA.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/Couleurs/test/Presque_equilibre/",data$id_tester[i],"_USA.png"),width = 980, height = 490)
      }
    }
    
    get_img(Pays = "USA", AlFec = Fec, AlLeg = Leg, AlFru = Fru, AlProt = Prot)
    dev.off()
  }
  else if (data$Pays[i] == "Spain"){
    
    Leg <- (data$tz2[i] + data$tz3[i]) / data$tTot[i]
    Fec <- data$tz4[i] / data$tTot[i]
    Fru <- data$tz1[i] / data$tTot[i]
    Prot_Fec <- data$tz5[i] / data$tTot[i]
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe/Couleurs/train/Pas_equilibre")) < 15){
        png(file = paste0("fixe/Couleurs/train/Pas_equilibre/",data$id_tester[i],"_Spain.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/Couleurs/test/Pas_equilibre/",data$id_tester[i],"_Spain.png"),width = 980, height = 490)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("fixe/Couleurs/train/Equilibre")) < 15){
        png(file = paste0("fixe/Couleurs/train/Equilibre/",data$id_tester[i],"_Spain.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/Couleurs/test/Equilibre/",data$id_tester[i],"_Spain.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe/Couleurs/train/Presque_equilibre")) < 15){
        png(file = paste0("fixe/Couleurs/train/Presque_equilibre/",data$id_tester[i],"_Spain.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/Couleurs/test/Presque_equilibre/",data$id_tester[i],"_Spain.png"),width = 980, height = 490)
      }
    }
    
    get_img(Pays = "Spain", AlFec = Fec, AlLeg = Leg, AlProt_Fec = Prot_Fec, AlFru = Fru)
    dev.off()
  } else {
    
    Leg <- (data$tz1[i] + data$tz2[i] + data$tz4[i]) / data$tTot[i]
    Prot_Leg <- data$tz3[i] / data$tTot[i]
    Prot_Fec <- data$tz5[i] / data$tTot[i]
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe/Couleurs/train/Pas_equilibre")) < 25){
        png(file = paste0("fixe/Couleurs/train/Pas_equilibre/",data$id_tester[i],"_South_Korea.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/Couleurs/test/Pas_equilibre/",data$id_tester[i],"_South_Korea.png"),width = 980, height = 490)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("fixe/Couleurs/train/Equilibre")) < 25){
        png(file = paste0("fixe/Couleurs/train/Equilibre/",data$id_tester[i],"_South_Korea.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/Couleurs/test/Equilibre/",data$id_tester[i],"_South_Korea.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe/Couleurs/train/Presque_equilibre")) < 25){
        png(file = paste0("fixe/Couleurs/train/Presque_equilibre/",data$id_tester[i],"_South_Korea.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/Couleurs/test/Presque_equilibre/",data$id_tester[i],"_South_Korea.png"),width = 980, height = 490)
      }
    }
    
    get_img(Pays = "South_Korea", AlLeg = Leg, AlProt_Leg = Prot_Leg, AlProt_Fec = Prot_Fec)
    dev.off()
  }
}


#####

#Zone fixes avec composantes nutritionnelles simples ----

for (i in 1:nrow(data_fin)){
  if (data_fin$Pays[i] == "Ukraine"){
    
    if (data_fin$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe_solo/Couleurs/train/Pas_equilibre")) < 5){
        png(file = paste0("fixe_solo/Couleurs/train/Pas_equilibre/",data_fin$id_tester[i],"_Ukraine.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/Couleurs/test/Pas_equilibre/",data_fin$id_tester[i],"_Ukraine.png"),width = 980, height = 490)
      }
    } 
    else if (data_fin$Classe[i] == "Equilibre"){
      if (length(list.files("fixe_solo/Couleurs/train/Equilibre")) < 5){
        png(file = paste0("fixe_solo/Couleurs/train/Equilibre/",data_fin$id_tester[i],"_Ukraine.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/Couleurs/test/Equilibre/",data_fin$id_tester[i],"_Ukraine.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe_solo/Couleurs/train/Presque_equilibre")) < 5){
        png(file = paste0("fixe_solo/Couleurs/train/Presque_equilibre/",data_fin$id_tester[i],"_Ukraine.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/Couleurs/test/Presque_equilibre/",data_fin$id_tester[i],"_Ukraine.png"),width = 980, height = 490)
      }
    }
    
    par(mar=c(0,0,0,0))
    x <- c(0,4)
    y <- c(0,1)
    
    colFeculents <- rgb(1,0,0, data_fin$pctF[i]/100)
    colLegumes <- rgb(0,1,0, data_fin$pctL[i]/100)
    colProteines <- rgb(0,0,1, (data_fin$pctP[i] + data_fin$pctPL[i])/100)
    colFruits <- rgb(0,0,0, data_fin$pctFr[i]/100)
    
    
    plot(y~x,col = "white",axes = FALSE, xlab = NA, ylab = NA, xlim= c(0,4), xaxs = "i", yaxs = "i")            # Draw empty plot
    
    rect(0,0,1,1, border = NA, col = colFeculents)
    rect(1,0,2,1, border = NA, col = colLegumes)
    rect(2,0,3,1, border = NA, col = colProteines)
    rect(3,0,4,1, border = NA, col = colFruits)
    
    
    
    dev.off()
  }
  else if (data_fin$Pays[i] == "Brazil"){
    
    if (data_fin$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe_solo/Couleurs/train/Pas_equilibre")) < 40){
        png(file = paste0("fixe_solo/Couleurs/train/Pas_equilibre/",data_fin$id_tester[i],"_Brazil.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/Couleurs/test/Pas_equilibre/",data_fin$id_tester[i],"_Brazil.png"),width = 980, height = 490)
      }
    } 
    else if (data_fin$Classe[i] == "Equilibre"){
      if (length(list.files("fixe_solo/Couleurs/train/Equilibre")) < 40){
        png(file = paste0("fixe_solo/Couleurs/train/Equilibre/",data_fin$id_tester[i],"_Brazil.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/Couleurs/test/Equilibre/",data_fin$id_tester[i],"_Brazil.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe_solo/Couleurs/train/Presque_equilibre")) < 40){
        png(file = paste0("fixe_solo/Couleurs/train/Presque_equilibre/",data_fin$id_tester[i],"_Brazil.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/Couleurs/test/Presque_equilibre/",data_fin$id_tester[i],"_Brazil.png"),width = 980, height = 490)
      }
    }
    
    par(mar=c(0,0,0,0))
    x <- c(0,4)
    y <- c(0,1)
    
    colFeculents <- rgb(1,0,0, data_fin$pctF[i]/100)
    colLegumes <- rgb(0,1,0, data_fin$pctL[i]/100)
    colProteines <- rgb(0,0,1, (data_fin$pctP[i] + data_fin$pctPL[i])/100)
    colFruits <- rgb(0,0,0, data_fin$pctFr[i]/100)
    
    
    plot(y~x,col = "white",axes = FALSE, xlab = NA, ylab = NA, xlim= c(0,4), xaxs = "i", yaxs = "i")            # Draw empty plot
    
    rect(0,0,1,1, border = NA, col = colFeculents)
    rect(1,0,2,1, border = NA, col = colLegumes)
    rect(2,0,3,1, border = NA, col = colProteines)
    rect(3,0,4,1, border = NA, col = colFruits)
    
    
    
    dev.off()
  }
  else if (data_fin$Pays[i] == "France"){
    
    if (data_fin$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe_solo/Couleurs/train/Pas_equilibre")) < 20){
        png(file = paste0("fixe_solo/Couleurs/train/Pas_equilibre/",data_fin$id_tester[i],"_France.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/Couleurs/test/Pas_equilibre/",data_fin$id_tester[i],"_France.png"),width = 980, height = 490)
      }
    } 
    else if (data_fin$Classe[i] == "Equilibre"){
      if (length(list.files("fixe_solo/Couleurs/train/Equilibre")) < 20){
        png(file = paste0("fixe_solo/Couleurs/train/Equilibre/",data_fin$id_tester[i],"_France.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/Couleurs/test/Equilibre/",data_fin$id_tester[i],"_France.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe_solo/Couleurs/train/Presque_equilibre")) < 20){
        png(file = paste0("fixe_solo/Couleurs/train/Presque_equilibre/",data_fin$id_tester[i],"_France.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/Couleurs/test/Presque_equilibre/",data_fin$id_tester[i],"_France.png"),width = 980, height = 490)
      }
    }
    
    par(mar=c(0,0,0,0))
    x <- c(0,4)
    y <- c(0,1)
    
    colFeculents <- rgb(1,0,0, data_fin$pctF[i]/100)
    colLegumes <- rgb(0,1,0, data_fin$pctL[i]/100)
    colProteines <- rgb(0,0,1, (data_fin$pctP[i] + data_fin$pctPL[i])/100)
    colFruits <- rgb(0,0,0, data_fin$pctFr[i]/100)
    
    
    plot(y~x,col = "white",axes = FALSE, xlab = NA, ylab = NA, xlim= c(0,4), xaxs = "i", yaxs = "i")            # Draw empty plot
    
    rect(0,0,1,1, border = NA, col = colFeculents)
    rect(1,0,2,1, border = NA, col = colLegumes)
    rect(2,0,3,1, border = NA, col = colProteines)
    rect(3,0,4,1, border = NA, col = colFruits)
    
    
    
    dev.off()
  }
  else if (data_fin$Pays[i] == "Finland"){
    
    
    if (data_fin$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe_solo/Couleurs/train/Pas_equilibre")) < 10){
        png(file = paste0("fixe_solo/Couleurs/train/Pas_equilibre/",data_fin$id_tester[i],"_Finland.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/Couleurs/test/Pas_equilibre/",data_fin$id_tester[i],"_Finland.png"),width = 980, height = 490)
      }
    } 
    else if (data_fin$Classe[i] == "Equilibre"){
      if (length(list.files("fixe_solo/Couleurs/train/Equilibre")) < 10){
        png(file = paste0("fixe_solo/Couleurs/train/Equilibre/",data_fin$id_tester[i],"_Finland.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/Couleurs/test/Equilibre/",data_fin$id_tester[i],"_Finland.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe_solo/Couleurs/train/Presque_equilibre")) < 10){
        png(file = paste0("fixe_solo/Couleurs/train/Presque_equilibre/",data_fin$id_tester[i],"_Finland.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/Couleurs/test/Presque_equilibre/",data_fin$id_tester[i],"_Finland.png"),width = 980, height = 490)
      }
    }
    
    par(mar=c(0,0,0,0))
    x <- c(0,4)
    y <- c(0,1)
    
    colFeculents <- rgb(1,0,0, data_fin$pctF[i]/100)
    colLegumes <- rgb(0,1,0, data_fin$pctL[i]/100)
    colProteines <- rgb(0,0,1, (data_fin$pctP[i] + data_fin$pctPL[i])/100)
    colFruits <- rgb(0,0,0, data_fin$pctFr[i]/100)
    
    
    plot(y~x,col = "white",axes = FALSE, xlab = NA, ylab = NA, xlim= c(0,4), xaxs = "i", yaxs = "i")            # Draw empty plot
    
    rect(0,0,1,1, border = NA, col = colFeculents)
    rect(1,0,2,1, border = NA, col = colLegumes)
    rect(2,0,3,1, border = NA, col = colProteines)
    rect(3,0,4,1, border = NA, col = colFruits)
    
    
    
    
    dev.off()
  }
  else if (data_fin$Pays[i] == "Greece"){
    
    
    if (data_fin$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe_solo/Couleurs/train/Pas_equilibre")) < 45){
        png(file = paste0("fixe_solo/Couleurs/train/Pas_equilibre/",data_fin$id_tester[i],"_Greece.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/Couleurs/test/Pas_equilibre/",data_fin$id_tester[i],"_Greece.png"),width = 980, height = 490)
      }
    } 
    else if (data_fin$Classe[i] == "Equilibre"){
      if (length(list.files("fixe_solo/Couleurs/train/Equilibre")) < 45){
        png(file = paste0("fixe_solo/Couleurs/train/Equilibre/",data_fin$id_tester[i],"_Greece.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/Couleurs/test/Equilibre/",data_fin$id_tester[i],"_Greece.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe_solo/Couleurs/train/Presque_equilibre")) < 45){
        png(file = paste0("fixe_solo/Couleurs/train/Presque_equilibre/",data_fin$id_tester[i],"_Greece.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/Couleurs/test/Presque_equilibre/",data_fin$id_tester[i],"_Greece.png"),width = 980, height = 490)
      }
    }
    
    par(mar=c(0,0,0,0))
    x <- c(0,4)
    y <- c(0,1)
    
    colFeculents <- rgb(1,0,0, data_fin$pctF[i]/100)
    colLegumes <- rgb(0,1,0, data_fin$pctL[i]/100)
    colProteines <- rgb(0,0,1, (data_fin$pctP[i] + data_fin$pctPL[i])/100)
    colFruits <- rgb(0,0,0, data_fin$pctFr[i]/100)
    
    
    plot(y~x,col = "white",axes = FALSE, xlab = NA, ylab = NA, xlim= c(0,4), xaxs = "i", yaxs = "i")            # Draw empty plot
    
    rect(0,0,1,1, border = NA, col = colFeculents)
    rect(1,0,2,1, border = NA, col = colLegumes)
    rect(2,0,3,1, border = NA, col = colProteines)
    rect(3,0,4,1, border = NA, col = colFruits)
    
    
    dev.off()
  }
  else if (data_fin$Pays[i] == "Italy"){
    
    if (data_fin$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe_solo/Couleurs/train/Pas_equilibre")) < 35){
        png(file = paste0("fixe_solo/Couleurs/train/Pas_equilibre/",data_fin$id_tester[i],"_Italy.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/Couleurs/test/Pas_equilibre/",data_fin$id_tester[i],"_Italy.png"),width = 980, height = 490)
      }
    } 
    else if (data_fin$Classe[i] == "Equilibre"){
      if (length(list.files("fixe_solo/Couleurs/train/Equilibre")) < 35){
        png(file = paste0("fixe_solo/Couleurs/train/Equilibre/",data_fin$id_tester[i],"_Italy.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/Couleurs/test/Equilibre/",data_fin$id_tester[i],"_Italy.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe_solo/Couleurs/train/Presque_equilibre")) < 35){
        png(file = paste0("fixe_solo/Couleurs/train/Presque_equilibre/",data_fin$id_tester[i],"_Italy.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/Couleurs/test/Presque_equilibre/",data_fin$id_tester[i],"_Italy.png"),width = 980, height = 490)
      }
    }
    
    par(mar=c(0,0,0,0))
    x <- c(0,4)
    y <- c(0,1)
    
    colFeculents <- rgb(1,0,0, data_fin$pctF[i]/100)
    colLegumes <- rgb(0,1,0, data_fin$pctL[i]/100)
    colProteines <- rgb(0,0,1, (data_fin$pctP[i] + data_fin$pctPL[i])/100)
    colFruits <- rgb(0,0,0, data_fin$pctFr[i]/100)
    
    
    plot(y~x,col = "white",axes = FALSE, xlab = NA, ylab = NA, xlim= c(0,4), xaxs = "i", yaxs = "i")            # Draw empty plot
    
    rect(0,0,1,1, border = NA, col = colFeculents)
    rect(1,0,2,1, border = NA, col = colLegumes)
    rect(2,0,3,1, border = NA, col = colProteines)
    rect(3,0,4,1, border = NA, col = colFruits)
    
    
    dev.off()
  }
  else if (data_fin$Pays[i] == "USA"){
    
    if (data_fin$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe_solo/Couleurs/train/Pas_equilibre")) < 30){
        png(file = paste0("fixe_solo/Couleurs/train/Pas_equilibre/",data_fin$id_tester[i],"_USA.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/Couleurs/test/Pas_equilibre/",data_fin$id_tester[i],"_USA.png"),width = 980, height = 490)
      }
    } 
    else if (data_fin$Classe[i] == "Equilibre"){
      if (length(list.files("fixe_solo/Couleurs/train/Equilibre")) < 30){
        png(file = paste0("fixe_solo/Couleurs/train/Equilibre/",data_fin$id_tester[i],"_USA.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/Couleurs/test/Equilibre/",data_fin$id_tester[i],"_USA.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe_solo/Couleurs/train/Presque_equilibre")) < 30){
        png(file = paste0("fixe_solo/Couleurs/train/Presque_equilibre/",data_fin$id_tester[i],"_USA.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/Couleurs/test/Presque_equilibre/",data_fin$id_tester[i],"_USA.png"),width = 980, height = 490)
      }
    }
    
    par(mar=c(0,0,0,0))
    x <- c(0,4)
    y <- c(0,1)
    
    colFeculents <- rgb(1,0,0, data_fin$pctF[i]/100)
    colLegumes <- rgb(0,1,0, data_fin$pctL[i]/100)
    colProteines <- rgb(0,0,1, (data_fin$pctP[i] + data_fin$pctPL[i])/100)
    colFruits <- rgb(0,0,0, data_fin$pctFr[i]/100)
    
    
    plot(y~x,col = "white",axes = FALSE, xlab = NA, ylab = NA, xlim= c(0,4), xaxs = "i", yaxs = "i")            # Draw empty plot
    
    rect(0,0,1,1, border = NA, col = colFeculents)
    rect(1,0,2,1, border = NA, col = colLegumes)
    rect(2,0,3,1, border = NA, col = colProteines)
    rect(3,0,4,1, border = NA, col = colFruits)
    
    
    dev.off()
  }
  else if (data_fin$Pays[i] == "Spain"){
    
    if (data_fin$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe_solo/Couleurs/train/Pas_equilibre")) < 15){
        png(file = paste0("fixe_solo/Couleurs/train/Pas_equilibre/",data_fin$id_tester[i],"_Spain.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/Couleurs/test/Pas_equilibre/",data_fin$id_tester[i],"_Spain.png"),width = 980, height = 490)
      }
    } 
    else if (data_fin$Classe[i] == "Equilibre"){
      if (length(list.files("fixe_solo/Couleurs/train/Equilibre")) < 15){
        png(file = paste0("fixe_solo/Couleurs/train/Equilibre/",data_fin$id_tester[i],"_Spain.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/Couleurs/test/Equilibre/",data_fin$id_tester[i],"_Spain.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe_solo/Couleurs/train/Presque_equilibre")) < 15){
        png(file = paste0("fixe_solo/Couleurs/train/Presque_equilibre/",data_fin$id_tester[i],"_Spain.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/Couleurs/test/Presque_equilibre/",data_fin$id_tester[i],"_Spain.png"),width = 980, height = 490)
      }
    }
    
    par(mar=c(0,0,0,0))
    x <- c(0,4)
    y <- c(0,1)
    
    colFeculents <- rgb(1,0,0, data_fin$pctF[i]/100)
    colLegumes <- rgb(0,1,0, data_fin$pctL[i]/100)
    colProteines <- rgb(0,0,1, (data_fin$pctP[i] + data_fin$pctPL[i])/100)
    colFruits <- rgb(0,0,0, data_fin$pctFr[i]/100)
    
    
    plot(y~x,col = "white",axes = FALSE, xlab = NA, ylab = NA, xlim= c(0,4), xaxs = "i", yaxs = "i")            # Draw empty plot
    
    rect(0,0,1,1, border = NA, col = colFeculents)
    rect(1,0,2,1, border = NA, col = colLegumes)
    rect(2,0,3,1, border = NA, col = colProteines)
    rect(3,0,4,1, border = NA, col = colFruits)
    
    
    dev.off()
  } else {
    
    if (data_fin$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe_solo/Couleurs/train/Pas_equilibre")) < 25){
        png(file = paste0("fixe_solo/Couleurs/train/Pas_equilibre/",data_fin$id_tester[i],"_South_Korea.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/Couleurs/test/Pas_equilibre/",data_fin$id_tester[i],"_South_Korea.png"),width = 980, height = 490)
      }
    } 
    else if (data_fin$Classe[i] == "Equilibre"){
      if (length(list.files("fixe_solo/Couleurs/train/Equilibre")) < 25){
        png(file = paste0("fixe_solo/Couleurs/train/Equilibre/",data_fin$id_tester[i],"_South_Korea.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/Couleurs/test/Equilibre/",data_fin$id_tester[i],"_South_Korea.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe_solo/Couleurs/train/Presque_equilibre")) < 25){
        png(file = paste0("fixe_solo/Couleurs/train/Presque_equilibre/",data_fin$id_tester[i],"_South_Korea.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/Couleurs/test/Presque_equilibre/",data_fin$id_tester[i],"_South_Korea.png"),width = 980, height = 490)
      }
    }
    
    par(mar=c(0,0,0,0))
    x <- c(0,4)
    y <- c(0,1)
    
    colFeculents <- rgb(1,0,0, data_fin$pctF[i]/100)
    colLegumes <- rgb(0,1,0, data_fin$pctL[i]/100)
    colProteines <- rgb(0,0,1, (data_fin$pctP[i] + data_fin$pctPL[i])/100)
    colFruits <- rgb(0,0,0, data_fin$pctFr[i]/100)
    
    
    plot(y~x,col = "white",axes = FALSE, xlab = NA, ylab = NA, xlim= c(0,4), xaxs = "i", yaxs = "i")            # Draw empty plot
    
    rect(0,0,1,1, border = NA, col = colFeculents)
    rect(1,0,2,1, border = NA, col = colLegumes)
    rect(2,0,3,1, border = NA, col = colProteines)
    rect(3,0,4,1, border = NA, col = colFruits)
    
    
    dev.off()
  }
  
  print(i)
}


#####