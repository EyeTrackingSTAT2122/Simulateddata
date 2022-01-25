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

data <- read_csv("E:/Master 2/Simulateddata/data/data_dim_pct_fin.csv")

#On retire les données de calibration

Classe <- rep(NA,nrow(data))

data <- cbind(data,Classe)

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


colnames(data) <- c("id_tester","id_item","Pays","num_stim","tz1","tz2","tz3","tz4","tz5","tTot","pctF","pctP","pctFr","pctL","pctPL","max","cat_max","Classe")

# data$tz1  #temps zone HG
# data$tz2  #temps zone HM
# data$tz3  #temps zone HD
# data$tz4  #temps zone BG
# data$tz5  #temps zone BD
# 
# data$tTot #temps total passé sur l'image


i <- 1

for (i in 1:nrow(data)){
  if (data$Pays[i] == "Ukraine"){
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe_solo/train/Pas_equilibre")) < 5){
        png(file = paste0("fixe_solo/train/Pas_equilibre/",data$id_tester[i],"_Ukraine.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/test/Pas_equilibre/",data$id_tester[i],"_Ukraine.png"),width = 980, height = 490)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("fixe_solo/train/Equilibre")) < 5){
        png(file = paste0("fixe_solo/train/Equilibre/",data$id_tester[i],"_Ukraine.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/test/Equilibre/",data$id_tester[i],"_Ukraine.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe_solo/train/Presque_equilibre")) < 5){
        png(file = paste0("fixe_solo/train/Presque_equilibre/",data$id_tester[i],"_Ukraine.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/test/Presque_equilibre/",data$id_tester[i],"_Ukraine.png"),width = 980, height = 490)
      }
    }
    
    par(mar=c(0,0,0,0))
    x <- c(0,5)
    y <- c(0,1)
    
    colFeculents <- rgb(1,0,0, data$pctF[i]/100)
    colLegumes <- rgb(0,1,0, data$pctL[i]/100)
    colProteines <- rgb(0,0,1, data$pctP[i]/100)
    colFruits <- rgb(153/255,76/255,0, data$pctFr[i]/100)
    colLait <- rgb(1,1,0, data$pctPL[i]/100)
    
    plot(y~x,col = "white",axes = FALSE, xlab = NA, ylab = NA, xlim= c(0,5), xaxs = "i", yaxs = "i")            # Draw empty plot
    
    rect(0,0,1,1, border = NA, col = colFeculents)
    rect(1,0,2,1, border = NA, col = colLegumes)
    rect(2,0,3,1, border = NA, col = colProteines)
    rect(3,0,4,1, border = NA, col = colFruits)
    rect(4,0,5,1, border = NA, col = colLait)
    

    dev.off()
  }
  else if (data$Pays[i] == "Brazil"){
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe_solo/train/Pas_equilibre")) < 40){
        png(file = paste0("fixe_solo/train/Pas_equilibre/",data$id_tester[i],"_Brazil.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/test/Pas_equilibre/",data$id_tester[i],"_Brazil.png"),width = 980, height = 490)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("fixe_solo/train/Equilibre")) < 40){
        png(file = paste0("fixe_solo/train/Equilibre/",data$id_tester[i],"_Brazil.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/test/Equilibre/",data$id_tester[i],"_Brazil.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe_solo/train/Presque_equilibre")) < 40){
        png(file = paste0("fixe_solo/train/Presque_equilibre/",data$id_tester[i],"_Brazil.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/test/Presque_equilibre/",data$id_tester[i],"_Brazil.png"),width = 980, height = 490)
      }
    }
    
    par(mar=c(0,0,0,0))
    x <- c(0,5)
    y <- c(0,1)
    
    colFeculents <- rgb(1,0,0, data$pctF[i]/100)
    colLegumes <- rgb(0,1,0, data$pctL[i]/100)
    colProteines <- rgb(0,0,1, data$pctP[i]/100)
    colFruits <- rgb(153/255,76/255,0, data$pctFr[i]/100)
    colLait <- rgb(1,1,0, data$pctPL[i]/100)
    
    plot(y~x,col = "white",axes = FALSE, xlab = NA, ylab = NA, xlim= c(0,5), xaxs = "i", yaxs = "i")            # Draw empty plot
    
    rect(0,0,1,1, border = NA, col = colFeculents)
    rect(1,0,2,1, border = NA, col = colLegumes)
    rect(2,0,3,1, border = NA, col = colProteines)
    rect(3,0,4,1, border = NA, col = colFruits)
    rect(4,0,5,1, border = NA, col = colLait)
    

    dev.off()
  }
  else if (data$Pays[i] == "France"){
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe_solo/train/Pas_equilibre")) < 20){
        png(file = paste0("fixe_solo/train/Pas_equilibre/",data$id_tester[i],"_France.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/test/Pas_equilibre/",data$id_tester[i],"_France.png"),width = 980, height = 490)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("fixe_solo/train/Equilibre")) < 20){
        png(file = paste0("fixe_solo/train/Equilibre/",data$id_tester[i],"_France.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/test/Equilibre/",data$id_tester[i],"_France.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe_solo/train/Presque_equilibre")) < 20){
        png(file = paste0("fixe_solo/train/Presque_equilibre/",data$id_tester[i],"_France.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/test/Presque_equilibre/",data$id_tester[i],"_France.png"),width = 980, height = 490)
      }
    }
    
    par(mar=c(0,0,0,0))
    x <- c(0,5)
    y <- c(0,1)
    
    colFeculents <- rgb(1,0,0, data$pctF[i]/100)
    colLegumes <- rgb(0,1,0, data$pctL[i]/100)
    colProteines <- rgb(0,0,1, data$pctP[i]/100)
    colFruits <- rgb(153/255,76/255,0, data$pctFr[i]/100)
    colLait <- rgb(1,1,0, data$pctPL[i]/100)
    
    plot(y~x,col = "white",axes = FALSE, xlab = NA, ylab = NA, xlim= c(0,5), xaxs = "i", yaxs = "i")            # Draw empty plot
    
    rect(0,0,1,1, border = NA, col = colFeculents)
    rect(1,0,2,1, border = NA, col = colLegumes)
    rect(2,0,3,1, border = NA, col = colProteines)
    rect(3,0,4,1, border = NA, col = colFruits)
    rect(4,0,5,1, border = NA, col = colLait)
    

    dev.off()
  }
  else if (data$Pays[i] == "Finland"){
  
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe_solo/train/Pas_equilibre")) < 10){
        png(file = paste0("fixe_solo/train/Pas_equilibre/",data$id_tester[i],"_Finland.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/test/Pas_equilibre/",data$id_tester[i],"_Finland.png"),width = 980, height = 490)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("fixe_solo/train/Equilibre")) < 10){
        png(file = paste0("fixe_solo/train/Equilibre/",data$id_tester[i],"_Finland.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/test/Equilibre/",data$id_tester[i],"_Finland.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe_solo/train/Presque_equilibre")) < 10){
        png(file = paste0("fixe_solo/train/Presque_equilibre/",data$id_tester[i],"_Finland.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/test/Presque_equilibre/",data$id_tester[i],"_Finland.png"),width = 980, height = 490)
      }
    }
    
    par(mar=c(0,0,0,0))
    x <- c(0,5)
    y <- c(0,1)
    
    colFeculents <- rgb(1,0,0, data$pctF[i]/100)
    colLegumes <- rgb(0,1,0, data$pctL[i]/100)
    colProteines <- rgb(0,0,1, data$pctP[i]/100)
    colFruits <- rgb(153/255,76/255,0, data$pctFr[i]/100)
    colLait <- rgb(1,1,0, data$pctPL[i]/100)
    
    plot(y~x,col = "white",axes = FALSE, xlab = NA, ylab = NA, xlim= c(0,5), xaxs = "i", yaxs = "i")            # Draw empty plot
    
    rect(0,0,1,1, border = NA, col = colFeculents)
    rect(1,0,2,1, border = NA, col = colLegumes)
    rect(2,0,3,1, border = NA, col = colProteines)
    rect(3,0,4,1, border = NA, col = colFruits)
    rect(4,0,5,1, border = NA, col = colLait)
    

    
    dev.off()
  }
  else if (data$Pays[i] == "Greece"){
    
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe_solo/train/Pas_equilibre")) < 45){
        png(file = paste0("fixe_solo/train/Pas_equilibre/",data$id_tester[i],"_Greece.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/test/Pas_equilibre/",data$id_tester[i],"_Greece.png"),width = 980, height = 490)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("fixe_solo/train/Equilibre")) < 45){
        png(file = paste0("fixe_solo/train/Equilibre/",data$id_tester[i],"_Greece.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/test/Equilibre/",data$id_tester[i],"_Greece.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe_solo/train/Presque_equilibre")) < 45){
        png(file = paste0("fixe_solo/train/Presque_equilibre/",data$id_tester[i],"_Greece.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/test/Presque_equilibre/",data$id_tester[i],"_Greece.png"),width = 980, height = 490)
      }
    }
    
    par(mar=c(0,0,0,0))
    x <- c(0,5)
    y <- c(0,1)
    
    colFeculents <- rgb(1,0,0, data$pctF[i]/100)
    colLegumes <- rgb(0,1,0, data$pctL[i]/100)
    colProteines <- rgb(0,0,1, data$pctP[i]/100)
    colFruits <- rgb(153/255,76/255,0, data$pctFr[i]/100)
    colLait <- rgb(1,1,0, data$pctPL[i]/100)
    
    plot(y~x,col = "white",axes = FALSE, xlab = NA, ylab = NA, xlim= c(0,5), xaxs = "i", yaxs = "i")            # Draw empty plot
    
    rect(0,0,1,1, border = NA, col = colFeculents)
    rect(1,0,2,1, border = NA, col = colLegumes)
    rect(2,0,3,1, border = NA, col = colProteines)
    rect(3,0,4,1, border = NA, col = colFruits)
    rect(4,0,5,1, border = NA, col = colLait)

    dev.off()
  }
  else if (data$Pays[i] == "Italy"){
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe_solo/train/Pas_equilibre")) < 35){
        png(file = paste0("fixe_solo/train/Pas_equilibre/",data$id_tester[i],"_Italy.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/test/Pas_equilibre/",data$id_tester[i],"_Italy.png"),width = 980, height = 490)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("fixe_solo/train/Equilibre")) < 35){
        png(file = paste0("fixe_solo/train/Equilibre/",data$id_tester[i],"_Italy.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/test/Equilibre/",data$id_tester[i],"_Italy.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe_solo/train/Presque_equilibre")) < 35){
        png(file = paste0("fixe_solo/train/Presque_equilibre/",data$id_tester[i],"_Italy.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/test/Presque_equilibre/",data$id_tester[i],"_Italy.png"),width = 980, height = 490)
      }
    }

    par(mar=c(0,0,0,0))
    x <- c(0,5)
    y <- c(0,1)
    
    colFeculents <- rgb(1,0,0, data$pctF[i]/100)
    colLegumes <- rgb(0,1,0, data$pctL[i]/100)
    colProteines <- rgb(0,0,1, data$pctP[i]/100)
    colFruits <- rgb(153/255,76/255,0, data$pctFr[i]/100)
    colLait <- rgb(1,1,0, data$pctPL[i]/100)
    
    plot(y~x,col = "white",axes = FALSE, xlab = NA, ylab = NA, xlim= c(0,5), xaxs = "i", yaxs = "i")            # Draw empty plot
    
    rect(0,0,1,1, border = NA, col = colFeculents)
    rect(1,0,2,1, border = NA, col = colLegumes)
    rect(2,0,3,1, border = NA, col = colProteines)
    rect(3,0,4,1, border = NA, col = colFruits)
    rect(4,0,5,1, border = NA, col = colLait)
    
    dev.off()
  }
  else if (data$Pays[i] == "USA"){
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe_solo/train/Pas_equilibre")) < 30){
        png(file = paste0("fixe_solo/train/Pas_equilibre/",data$id_tester[i],"_USA.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/test/Pas_equilibre/",data$id_tester[i],"_USA.png"),width = 980, height = 490)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("fixe_solo/train/Equilibre")) < 30){
        png(file = paste0("fixe_solo/train/Equilibre/",data$id_tester[i],"_USA.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/test/Equilibre/",data$id_tester[i],"_USA.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe_solo/train/Presque_equilibre")) < 30){
        png(file = paste0("fixe_solo/train/Presque_equilibre/",data$id_tester[i],"_USA.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/test/Presque_equilibre/",data$id_tester[i],"_USA.png"),width = 980, height = 490)
      }
    }
    
    par(mar=c(0,0,0,0))
    x <- c(0,5)
    y <- c(0,1)
    
    colFeculents <- rgb(1,0,0, data$pctF[i]/100)
    colLegumes <- rgb(0,1,0, data$pctL[i]/100)
    colProteines <- rgb(0,0,1, data$pctP[i]/100)
    colFruits <- rgb(153/255,76/255,0, data$pctFr[i]/100)
    colLait <- rgb(1,1,0, data$pctPL[i]/100)
    
    plot(y~x,col = "white",axes = FALSE, xlab = NA, ylab = NA, xlim= c(0,5), xaxs = "i", yaxs = "i")            # Draw empty plot
    
    rect(0,0,1,1, border = NA, col = colFeculents)
    rect(1,0,2,1, border = NA, col = colLegumes)
    rect(2,0,3,1, border = NA, col = colProteines)
    rect(3,0,4,1, border = NA, col = colFruits)
    rect(4,0,5,1, border = NA, col = colLait)
    
    dev.off()
  }
  else if (data$Pays[i] == "Spain"){
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe_solo/train/Pas_equilibre")) < 15){
        png(file = paste0("fixe_solo/train/Pas_equilibre/",data$id_tester[i],"_Spain.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/test/Pas_equilibre/",data$id_tester[i],"_Spain.png"),width = 980, height = 490)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("fixe_solo/train/Equilibre")) < 15){
        png(file = paste0("fixe_solo/train/Equilibre/",data$id_tester[i],"_Spain.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/test/Equilibre/",data$id_tester[i],"_Spain.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe_solo/train/Presque_equilibre")) < 15){
        png(file = paste0("fixe_solo/train/Presque_equilibre/",data$id_tester[i],"_Spain.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/test/Presque_equilibre/",data$id_tester[i],"_Spain.png"),width = 980, height = 490)
      }
    }
    
    par(mar=c(0,0,0,0))
    x <- c(0,5)
    y <- c(0,1)
    
    colFeculents <- rgb(1,0,0, data$pctF[i]/100)
    colLegumes <- rgb(0,1,0, data$pctL[i]/100)
    colProteines <- rgb(0,0,1, data$pctP[i]/100)
    colFruits <- rgb(153/255,76/255,0, data$pctFr[i]/100)
    colLait <- rgb(1,1,0, data$pctPL[i]/100)
    
    plot(y~x,col = "white",axes = FALSE, xlab = NA, ylab = NA, xlim= c(0,5), xaxs = "i", yaxs = "i")            # Draw empty plot
    
    rect(0,0,1,1, border = NA, col = colFeculents)
    rect(1,0,2,1, border = NA, col = colLegumes)
    rect(2,0,3,1, border = NA, col = colProteines)
    rect(3,0,4,1, border = NA, col = colFruits)
    rect(4,0,5,1, border = NA, col = colLait)
    
    dev.off()
  } else {
    
    if (data$Classe[i] == "Pas_Equilibre"){
      if (length(list.files("fixe_solo/train/Pas_equilibre")) < 25){
        png(file = paste0("fixe_solo/train/Pas_equilibre/",data$id_tester[i],"_South_Korea.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/test/Pas_equilibre/",data$id_tester[i],"_South_Korea.png"),width = 980, height = 490)
      }
    } 
    else if (data$Classe[i] == "Equilibre"){
      if (length(list.files("fixe_solo/train/Equilibre")) < 25){
        png(file = paste0("fixe_solo/train/Equilibre/",data$id_tester[i],"_South_Korea.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/test/Equilibre/",data$id_tester[i],"_South_Korea.png"),width = 980, height = 490)
      }
    } else {
      if (length(list.files("fixe_solo/train/Presque_equilibre")) < 25){
        png(file = paste0("fixe_solo/train/Presque_equilibre/",data$id_tester[i],"_South_Korea.png"),width = 980, height = 490)
      } else {
        png(file = paste0("fixe_solo/test/Presque_equilibre/",data$id_tester[i],"_South_Korea.png"),width = 980, height = 490)
      }
    }
    
    par(mar=c(0,0,0,0))
    x <- c(0,5)
    y <- c(0,1)
    
    colFeculents <- rgb(1,0,0, data$pctF[i]/100)
    colLegumes <- rgb(0,1,0, data$pctL[i]/100)
    colProteines <- rgb(0,0,1, data$pctP[i]/100)
    colFruits <- rgb(153/255,76/255,0, data$pctFr[i]/100)
    colLait <- rgb(1,1,0, data$pctPL[i]/100)
    
    plot(y~x,col = "white",axes = FALSE, xlab = NA, ylab = NA, xlim= c(0,5), xaxs = "i", yaxs = "i")            # Draw empty plot
    
    rect(0,0,1,1, border = NA, col = colFeculents)
    rect(1,0,2,1, border = NA, col = colLegumes)
    rect(2,0,3,1, border = NA, col = colProteines)
    rect(3,0,4,1, border = NA, col = colFruits)
    rect(4,0,5,1, border = NA, col = colLait)
    
    dev.off()
  }
  
  print(i)
}
