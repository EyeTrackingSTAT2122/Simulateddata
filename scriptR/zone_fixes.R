library(tidyverse)
library(ggplot2)
library(tidyr)
library(jpeg)
library(ggpubr)
library(grid)
library(sjmisc)
library(readr)

survey <- read_csv("study/survey_study.csv")
survey_2 <- read_csv("study/survey_study2.csv")

surv <- rbind(survey,survey_2)

data <- read_csv("E:/Master 2/Simulateddata/data/data_fixation_fin.csv")

Classe <- rep(NA,nrow(data))

data <- cbind(data,Classe)

#----
for (i in 1:nrow(data)){
  for (j in 1:nrow(surv)){
    if (data$id_tester[i] == surv$tester_id[j]) {
      if(data$pays[i]=="Brazil"){
        data$Classe[i] <- surv$`D'après vous, le plateau est-il équilibré ? - 352.jpg`[j]
      }
      else if(data$pays[i]=="Finland"){
        data$Classe[i] <- surv$`D'après vous, le plateau est-il équilibré ?  - 672.jpg`[j]
      }
      else if(data$pays[i]=="France"){
        data$Classe[i] <- surv$`D'après vous, le plateau est-il équilibré ?  - 421.jpg`[j]
      }
      else if(data$pays[i]=="Greece"){
        data$Classe[i] <- surv$`D'après vous, le plateau est-il équilibré ?  - 980.jpg`[j]
      }
      else if(data$pays[i]=="Italy"){
        data$Classe[i] <- surv$`D'après vous, le plateau est-il équilibré ? - 238.jpg`[j]
      }
      else if(data$pays[i]=="Spain"){
        data$Classe[i] <- surv$`D'après vous, le plateau est-il équilibré ?  - 756.jpg`[j]
      }
      else if(data$pays[i]=="South Korea"){
        data$Classe[i] <- surv$`D'après vous, le plateau est-il équilibré ?  - 489.jpg`[j]
      }
      else if(data$pays[i]=="Ukraine"){
        data$Classe[i] <- surv$`D'après vous, le plateau est-il équilibré ?  - 156.jpg`[j]
      }
      else {
        data$Classe[i] <- surv$`D'après vous, le plateau est-il équilbré ?  - 327.jpg`[j]
      }
    } 
  }
}

for (i in 1:nrow(data)){
  if (data$Classe[i] == "Il est équilibré"){
    data$Classe[i] <- "Equilibre"
  }
  else if (data$Classe[i] == "Il n'est pas équilibré"){
    data$Classe[i] <- "Pas_Equilibre"
  }
  else {
    data$Classe[i] <- "Presque_Equilibre"
  }
}

data$Classe <- as.factor(data$Classe)


colnames(data) <- c("id_tester","id_item","Pays","num_stim","tz1","tz2","tz3","tz4","tz5","tTot","Classe")

# data$tz1  #temps zone HG
# data$tz2  #temps zone HM
# data$tz3  #temps zone HD
# data$tz4  #temps zone BG
# data$tz5  #temps zone BD
# 
# data$tTot #temps total passé sur l'image


get_img <- function(Pays, AlFec = 0, AlLeg = 0, AlProt = 0, AlFru = 0, AlProt_Fec = 0, AlProt_Leg = 0, AlFec_Fru = 0, AlLait_Fru = 0){

  par(mar=c(0,0,0,0))
  
  x <- c(0,4)
  y <- c(0,2)
  
  colPlateau <- rgb(220/255,220/255,220/255)
  colFeculents <- rgb(1,0,0, AlFec)
  colLegumes <- rgb(0,1,0, AlLeg)
  colProteines <- rgb(0,0,1, AlProt)
  colFruits <- rgb(153/255,76/255,0, AlFru)
  
  colProt_Fec <- rgb(1,0,1, AlProt_Fec)
  colProt_Leg <- rgb(0,1,1, AlProt_Leg)
  colFec_Fru <- rgb(0,0,0, AlFec_Fru)
  colLait_Fru <- rgb(1,1,0, AlLait_Fru)
  
  plot(y~x,col = "white",axes = FALSE, xlab = NA, ylab = NA, xlim= c(0,4), xaxs = "i", yaxs = "i")            # Draw empty plot
  
  rect(0,0,1,1, border = NA, col = colFeculents)
  rect(1,0,2,1, border = NA, col = colLegumes)
  rect(2,0,3,1, border = NA, col = colProteines)
  rect(3,0,4,1, border = NA, col = colFruits)
  
  rect(0,1,1,2, border = NA, col = colProt_Fec)
  rect(1,1,2,2, border = NA, col = colProt_Leg)
  rect(2,1,3,2, border = NA, col = colFec_Fru)
  rect(3,1,4,2, border = NA, col = colLait_Fru)
  
}

i <- 1

for (i in 1:nrow(data)){
  if (data$Pays[i] == "Ukraine"){
    
    Leg <- (data$tz1[i] + data$tz4[i]) / data$tTot[i]
    Fec <- (data$tz2[i] + data$tz3[i])/ data$tTot[i]
    Prot_Fec <- data$tz5[i] / data$tTot[i]
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe/train/Pas_equilibre")) < 5){
        png(file = paste0("fixe/train/Pas_equilibre/",data$id_tester[i],"_Ukraine.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/test/Pas_equilibre/",data$id_tester[i],"_Ukraine.png"),width = 980, height = 490)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("fixe/train/Equilibre")) < 5){
        png(file = paste0("fixe/train/Equilibre/",data$id_tester[i],"_Ukraine.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/test/Equilibre/",data$id_tester[i],"_Ukraine.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe/train/Presque_equilibre")) < 5){
        png(file = paste0("fixe/train/Presque_equilibre/",data$id_tester[i],"_Ukraine.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/test/Presque_equilibre/",data$id_tester[i],"_Ukraine.png"),width = 980, height = 490)
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
      if (length(list.files("fixe/train/Pas_equilibre")) < 40){
        png(file = paste0("fixe/train/Pas_equilibre/",data$id_tester[i],"_Brazil.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/test/Pas_equilibre/",data$id_tester[i],"_Brazil.png"),width = 980, height = 490)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("fixe/train/Equilibre")) < 40){
        png(file = paste0("fixe/train/Equilibre/",data$id_tester[i],"_Brazil.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/test/Equilibre/",data$id_tester[i],"_Brazil.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe/train/Presque_equilibre")) < 40){
        png(file = paste0("fixe/train/Presque_equilibre/",data$id_tester[i],"_Brazil.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/test/Presque_equilibre/",data$id_tester[i],"_Brazil.png"),width = 980, height = 490)
      }
    }
    
    get_img(Pays = "Brazil", AlFec = Fec, AlLeg = Leg, AlProt_Leg = Prot_Leg, AlFru = Fru)
    dev.off()
  }
  else if (data$Pays[i] == "France"){
    
    Leg <- (data$tz2[i] + data$tz4[i]) / data$tTot[i]
    Prot <- data$tz3[i] / data$tTot[i]
    Fru <- data$tz1[i] / data$tTot[i]
    Lait_Fru <- data$tz5[i] / data$tTot[i]
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe/train/Pas_equilibre")) < 20){
        png(file = paste0("fixe/train/Pas_equilibre/",data$id_tester[i],"_France.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/test/Pas_equilibre/",data$id_tester[i],"_France.png"),width = 980, height = 490)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("fixe/train/Equilibre")) < 20){
        png(file = paste0("fixe/train/Equilibre/",data$id_tester[i],"_France.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/test/Equilibre/",data$id_tester[i],"_France.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe/train/Presque_equilibre")) < 20){
        png(file = paste0("fixe/train/Presque_equilibre/",data$id_tester[i],"_France.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/test/Presque_equilibre/",data$id_tester[i],"_France.png"),width = 980, height = 490)
      }
    }
    
    get_img(Pays = "France", AlFru = Fru, AlProt = Prot, AlLeg = Leg, AlLait_Fru = Lait_Fru)
    dev.off()
  }
  else if (data$Pays[i] == "Finland"){
    
    Fec_Fru <- data$tz5[i] / data$tTot[i]
    Leg <- (data$tz2[i] + data$tz3[i] + data$tz4[i]) / data$tTot[i]
    Fec <- data$tz1[i] / data$tTot[i]
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe/train/Pas_equilibre")) < 10){
        png(file = paste0("fixe/train/Pas_equilibre/",data$id_tester[i],"_Finland.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/test/Pas_equilibre/",data$id_tester[i],"_Finland.png"),width = 980, height = 490)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("fixe/train/Equilibre")) < 10){
        png(file = paste0("fixe/train/Equilibre/",data$id_tester[i],"_Finland.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/test/Equilibre/",data$id_tester[i],"_Finland.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe/train/Presque_equilibre")) < 10){
        png(file = paste0("fixe/train/Presque_equilibre/",data$id_tester[i],"_Finland.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/test/Presque_equilibre/",data$id_tester[i],"_Finland.png"),width = 980, height = 490)
      }
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
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe/train/Pas_equilibre")) < 45){
        png(file = paste0("fixe/train/Pas_equilibre/",data$id_tester[i],"_Greece.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/test/Pas_equilibre/",data$id_tester[i],"_Greece.png"),width = 980, height = 490)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("fixe/train/Equilibre")) < 45){
        png(file = paste0("fixe/train/Equilibre/",data$id_tester[i],"_Greece.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/test/Equilibre/",data$id_tester[i],"_Greece.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe/train/Presque_equilibre")) < 45){
        png(file = paste0("fixe/train/Presque_equilibre/",data$id_tester[i],"_Greece.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/test/Presque_equilibre/",data$id_tester[i],"_Greece.png"),width = 980, height = 490)
      }
    }
    
    get_img(Pays = "Greece", AlFec = Fec, AlLeg = Leg, AlProt_Fec = Prot_Fec, AlLait_Fru = Lait_Fru, AlFru = Fru)
    dev.off()
  }
  else if (data$Pays[i] == "Italy"){
    
    Leg <- data$tz2[i] / data$tTot[i]
    Fec <- (data$tz3[i] + data$tz4[i]) / data$tTot[i]
    Fru <- data$tz1[i] / data$tTot[i]
    Prot_Leg <- data$tz5[i] / data$tTot[i]
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe/train/Pas_equilibre")) < 35){
        png(file = paste0("fixe/train/Pas_equilibre/",data$id_tester[i],"_Italy.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/test/Pas_equilibre/",data$id_tester[i],"_Italy.png"),width = 980, height = 490)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("fixe/train/Equilibre")) < 35){
        png(file = paste0("fixe/train/Equilibre/",data$id_tester[i],"_Italy.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/test/Equilibre/",data$id_tester[i],"_Italy.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe/train/Presque_equilibre")) < 35){
        png(file = paste0("fixe/train/Presque_equilibre/",data$id_tester[i],"_Italy.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/test/Presque_equilibre/",data$id_tester[i],"_Italy.png"),width = 980, height = 490)
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
      if (length(list.files("fixe/train/Pas_equilibre")) < 30){
        png(file = paste0("fixe/train/Pas_equilibre/",data$id_tester[i],"_USA.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/test/Pas_equilibre/",data$id_tester[i],"_USA.png"),width = 980, height = 490)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("fixe/train/Equilibre")) < 30){
        png(file = paste0("fixe/train/Equilibre/",data$id_tester[i],"_USA.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/test/Equilibre/",data$id_tester[i],"_USA.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe/train/Presque_equilibre")) < 30){
        png(file = paste0("fixe/train/Presque_equilibre/",data$id_tester[i],"_USA.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/test/Presque_equilibre/",data$id_tester[i],"_USA.png"),width = 980, height = 490)
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
      if (length(list.files("fixe/train/Pas_equilibre")) < 15){
        png(file = paste0("fixe/train/Pas_equilibre/",data$id_tester[i],"_Spain.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/test/Pas_equilibre/",data$id_tester[i],"_Spain.png"),width = 980, height = 490)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("fixe/train/Equilibre")) < 15){
        png(file = paste0("fixe/train/Equilibre/",data$id_tester[i],"_Spain.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/test/Equilibre/",data$id_tester[i],"_Spain.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe/train/Presque_equilibre")) < 15){
        png(file = paste0("fixe/train/Presque_equilibre/",data$id_tester[i],"_Spain.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/test/Presque_equilibre/",data$id_tester[i],"_Spain.png"),width = 980, height = 490)
      }
    }
    
    get_img(Pays = "Spain", AlFec = Fec, AlLeg = Leg, AlProt_Fec = Prot_Fec, AlFru = Fru)
    dev.off()
  } else {
    
    Leg <- (data$tz1[i] + data$tz2[i] + data$tz4[i]) / data$tTot[i]
    Prot_Leg <- data$tz3[i] / data$tTot[i]
    Prot_Fec <- data$tz5[i] / data$tTot[i]
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe/train/Pas_equilibre")) < 25){
        png(file = paste0("fixe/train/Pas_equilibre/",data$id_tester[i],"_South_Korea.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/test/Pas_equilibre/",data$id_tester[i],"_South_Korea.png"),width = 980, height = 490)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("fixe/train/Equilibre")) < 25){
        png(file = paste0("fixe/train/Equilibre/",data$id_tester[i],"_South_Korea.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/test/Equilibre/",data$id_tester[i],"_South_Korea.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe/train/Presque_equilibre")) < 25){
        png(file = paste0("fixe/train/Presque_equilibre/",data$id_tester[i],"_South_Korea.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe/test/Presque_equilibre/",data$id_tester[i],"_South_Korea.png"),width = 980, height = 490)
      }
    }
    
    get_img(Pays = "South_Korea", AlLeg = Leg, AlProt_Leg = Prot_Leg, AlProt_Fec = Prot_Fec)
    dev.off()
  }
}




