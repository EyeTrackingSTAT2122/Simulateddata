library(tidyverse)
library(ggplot2)
library(tidyr)
library(jpeg)
library(ggpubr)
library(grid)
library(sjmisc)
library(readr)
library(FactoMineR)
library(nnet)
library(Rcmdr)
library(factoextra)

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

clas <- rep(0, nrow(data))

for (i in 1:nrow(data)){
  clas[i] <- paste0(data$Pays[i], data$Classe[i])
}


data <- data.frame(data, clas)

data$clas <- as.factor(data$clas)

tLeg <- rep(0, nrow(data))
tFec <- rep(0, nrow(data))
tProt <- rep(0, nrow(data))
tFru <- rep(0, nrow(data))

tLait_Fru <- rep(0, nrow(data))
tFec_Fru <- rep(0, nrow(data))
tProt_Leg <- rep(0, nrow(data))
tProt_Fec <- rep(0, nrow(data))

Classe <- clas


newtab <- data.frame(tLeg, tFec, tProt, tFru, tLait_Fru, tFec_Fru, tProt_Leg, tProt_Fec, Classe)
  
newtab$Classe <- as.factor(newtab$Classe)

for (i in 1:nrow(data)){
  if (data$Pays[i] == "Ukraine"){
    
    Leg <- (data$tz1[i] + data$tz4[i]) / data$tTot[i] * 100
    Fec <- (data$tz2[i] + data$tz3[i])/ data$tTot[i] * 100
    Fru <- 0
    Prot <- 0
    
    Prot_Fec <- data$tz5[i] / data$tTot[i] * 100
    Lait_Fru <- 0
    Prot_Leg <- 0
    Fec_Fru <- 0
    
    newtab$tLeg[i] <- Leg
    newtab$tFec[i] <- Fec
    newtab$tProt[i] <- Prot
    newtab$tFru[i] <- Fru
    
    newtab$tProt_Fec[i] <- Prot_Fec
    newtab$tLait_Fru[i] <- Lait_Fru
    newtab$tProt_Leg[i] <- Prot_Leg
    newtab$tFec_Fru[i] <- Fec_Fru
  }
  else if (data$Pays[i] == "Brazil"){
    
    Leg <- 0
    Fec <- 0
    Fru <- 0
    Prot <- 0
    
    Prot_Fec <- 0
    Lait_Fru <- 0
    Prot_Leg <- 0
    Fec_Fru <- 0
    
    Leg <- data$tz2[i] / data$tTot[i] * 100
    Fec <- (data$tz1[i] + data$tz5[i]) / data$tTot[i] * 100
    Fru <- data$tz3[i] / data$tTot[i] * 100
    Prot_Leg <- data$tz4[i] / data$tTot[i] * 100
    
    
    newtab$tLeg[i] <- Leg
    newtab$tFec[i] <- Fec
    newtab$tProt[i] <- Prot
    newtab$tFru[i] <- Fru
    
    newtab$tProt_Fec[i] <- Prot_Fec
    newtab$tLait_Fru[i] <- Lait_Fru
    newtab$tProt_Leg[i] <- Prot_Leg
    newtab$tFec_Fru[i] <- Fec_Fru
  }
  else if (data$Pays[i] == "France"){
    Leg <- 0
    Fec <- 0
    Fru <- 0
    Prot <- 0
    
    Prot_Fec <- 0
    Lait_Fru <- 0
    Prot_Leg <- 0
    Fec_Fru <- 0
    
    Leg <- (data$tz2[i] + data$tz4[i]) / data$tTot[i] * 100
    Prot <- data$tz3[i] / data$tTot[i] * 100
    Fru <- data$tz1[i] / data$tTot[i] * 100
    Lait_Fru <- data$tz5[i] / data$tTot[i] * 100
    
    newtab$tLeg[i] <- Leg
    newtab$tFec[i] <- Fec
    newtab$tProt[i] <- Prot
    newtab$tFru[i] <- Fru
    
    newtab$tProt_Fec[i] <- Prot_Fec
    newtab$tLait_Fru[i] <- Lait_Fru
    newtab$tProt_Leg[i] <- Prot_Leg
    newtab$tFec_Fru[i] <- Fec_Fru
  }
  else if (data$Pays[i] == "Finland"){
    Leg <- 0
    Fec <- 0
    Fru <- 0
    Prot <- 0
    
    Prot_Fec <- 0
    Lait_Fru <- 0
    Prot_Leg <- 0
    Fec_Fru <- 0
    
    Fec_Fru <- data$tz5[i] / data$tTot[i] * 100
    Leg <- (data$tz2[i] + data$tz3[i] + data$tz4[i]) / data$tTot[i] * 100
    Fec <- data$tz1[i] / data$tTot[i] * 100
    
    newtab$tLeg[i] <- Leg
    newtab$tFec[i] <- Fec
    newtab$tProt[i] <- Prot
    newtab$tFru[i] <- Fru
    
    newtab$tProt_Fec[i] <- Prot_Fec
    newtab$tLait_Fru[i] <- Lait_Fru
    newtab$tProt_Leg[i] <- Prot_Leg
    newtab$tFec_Fru[i] <- Fec_Fru
  }
  else if (data$Pays[i] == "Greece"){
    Leg <- 0
    Fec <- 0
    Fru <- 0
    Prot <- 0
    
    Prot_Fec <- 0
    Lait_Fru <- 0
    Prot_Leg <- 0
    Fec_Fru <- 0
    
    Lait_Fru <- data$tz1[i] / data$tTot[i] * 100
    Leg <- data$tz2[i] / data$tTot[i] * 100
    Fec <- data$tz3[i] / data$tTot[i] * 100
    Fru <- data$tz4[i] / data$tTot[i] * 100
    Prot_Fec <- data$tz5[i] / data$tTot[i] * 100
    
    newtab$tLeg[i] <- Leg
    newtab$tFec[i] <- Fec
    newtab$tProt[i] <- Prot
    newtab$tFru[i] <- Fru
    
    newtab$tProt_Fec[i] <- Prot_Fec
    newtab$tLait_Fru[i] <- Lait_Fru
    newtab$tProt_Leg[i] <- Prot_Leg
    newtab$tFec_Fru[i] <- Fec_Fru
  }
  else if (data$Pays[i] == "Italy"){
    Leg <- 0
    Fec <- 0
    Fru <- 0
    Prot <- 0
    
    Prot_Fec <- 0
    Lait_Fru <- 0
    Prot_Leg <- 0
    Fec_Fru <- 0
    
    Leg <- data$tz2[i] / data$tTot[i] * 100
    Fec <- (data$tz3[i] + data$tz4[i]) / data$tTot[i] * 100
    Fru <- data$tz1[i] / data$tTot[i] * 100
    Prot_Leg <- data$tz5[i] / data$tTot[i] * 100
    
    newtab$tLeg[i] <- Leg
    newtab$tFec[i] <- Fec
    newtab$tProt[i] <- Prot
    newtab$tFru[i] <- Fru
    
    newtab$tProt_Fec[i] <- Prot_Fec
    newtab$tLait_Fru[i] <- Lait_Fru
    newtab$tProt_Leg[i] <- Prot_Leg
    newtab$tFec_Fru[i] <- Fec_Fru
  }
  else if (data$Pays[i] == "USA"){
    Leg <- 0
    Fec <- 0
    Fru <- 0
    Prot <- 0
    
    Prot_Fec <- 0
    Lait_Fru <- 0
    Prot_Leg <- 0
    Fec_Fru <- 0
    
    Leg <- data$tz2[i] / data$tTot[i] * 100
    Fec <- (data$tz1[i] + data$tz4[i]) / data$tTot[i] * 100
    Fru <- data$tz3[i] / data$tTot[i] * 100
    Prot <- data$tz5[i] / data$tTot[i] * 100
    
    newtab$tLeg[i] <- Leg
    newtab$tFec[i] <- Fec
    newtab$tProt[i] <- Prot
    newtab$tFru[i] <- Fru
    
    newtab$tProt_Fec[i] <- Prot_Fec
    newtab$tLait_Fru[i] <- Lait_Fru
    newtab$tProt_Leg[i] <- Prot_Leg
    newtab$tFec_Fru[i] <- Fec_Fru
  }
  else if (data$Pays[i] == "Spain"){
    Leg <- 0
    Fec <- 0
    Fru <- 0
    Prot <- 0
    
    Prot_Fec <- 0
    Lait_Fru <- 0
    Prot_Leg <- 0
    Fec_Fru <- 0
    
    Leg <- (data$tz2[i] + data$tz3[i]) / data$tTot[i] * 100
    Fec <- data$tz4[i] / data$tTot[i] * 100
    Fru <- data$tz1[i] / data$tTot[i] * 100
    Prot_Fec <- data$tz5[i] / data$tTot[i] * 100
    
    newtab$tLeg[i] <- Leg
    newtab$tFec[i] <- Fec
    newtab$tProt[i] <- Prot
    newtab$tFru[i] <- Fru
    
    newtab$tProt_Fec[i] <- Prot_Fec
    newtab$tLait_Fru[i] <- Lait_Fru
    newtab$tProt_Leg[i] <- Prot_Leg
    newtab$tFec_Fru[i] <- Fec_Fru
  } else {
    Leg <- 0
    Fec <- 0
    Fru <- 0
    Prot <- 0
    
    Prot_Fec <- 0
    Lait_Fru <- 0
    Prot_Leg <- 0
    Fec_Fru <- 0
    
    Leg <- (data$tz1[i] + data$tz2[i] + data$tz4[i]) / data$tTot[i] * 100
    Prot_Leg <- data$tz3[i] / data$tTot[i] * 100
    Prot_Fec <- data$tz5[i] / data$tTot[i] * 100
    
    newtab$tLeg[i] <- Leg
    newtab$tFec[i] <- Fec
    newtab$tProt[i] <- Prot
    newtab$tFru[i] <- Fru
    
    newtab$tProt_Fec[i] <- Prot_Fec
    newtab$tLait_Fru[i] <- Lait_Fru
    newtab$tProt_Leg[i] <- Prot_Leg
    newtab$tFec_Fru[i] <- Fec_Fru
  }
}

res.pca <- PCA(newtab, quali.sup = 9)
res.fma <- MFA(newtab, group = c(8,1),type = c("c","n"))
plot.PCA(res.pca, invisible = c("ind"))

mod1 <- multinom(Classe ~ , data = newtab, maxit = 1000)
coef(mod1)

mod0 = multinom(Classe~1,data=newtab) # ML fit of the null model
deviance(mod0)-deviance(mod1)                # Explained deviance

proba = fitted(mod1)            # Estimated probabilities of each class
observed_class = newtab$Classe
head(data.frame(round(proba,3),observed_class))


fitted_class = predict(mod1,type="class")  # Bayes rule
head(fitted_class)

confusion = table(observed_class,fitted_class)
confusion

rowSums(confusion)
confusion_percentage = 100*confusion/outer(rowSums(confusion),rep(1,26))
round(confusion_percentage,3)

select = stepwise(mod1,direction="forward/backward",criterion="AIC")
select = stepwise(mod1,direction="forward/backward",criterion="BIC")

observed = newtab$Classe

acc = rep(0,5) # Initialize a vector of accuracy values
for (k in 1:5) {
  select = stepwise(mod1,direction="forward/backward",criterion="AIC",steps=k,trace=0)
  predictions = predict(select,type="class")
  acc[k] = mean(predictions==observed)
} 




