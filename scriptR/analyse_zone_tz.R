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


pct_tz1 <- data$tz1/data$tTot * 100
pct_tz2 <- data$tz2/data$tTot * 100
pct_tz3 <- data$tz3/data$tTot * 100
pct_tz4 <- data$tz4/data$tTot * 100
pct_tz5 <- data$tz5/data$tTot * 100

new <- data.frame(pct_tz1,pct_tz2,pct_tz3,pct_tz4,pct_tz5,data$clas)

res.pca <- PCA(new, quali.sup = 6)
plot.PCA(res.pca, invisible = c("ind"))

res <- cbind(res.pca$ind$coord, new)
mod1 <- lm(Dim.1 ~ data.Classe*(pct_tz1 + pct_tz2+ pct_tz3+ pct_tz4 + pct_tz5), data = res)
mod2 <- lm(Dim.2 ~ data.Classe*(pct_tz1 + pct_tz2+ pct_tz3+ pct_tz4 + pct_tz5), data = res)
res.anova <- anova(mod1); res.anova
res.anova2 <- anova(mod2) ; res.anova2
coefficients(mod1)
summary(mod1)
summary(mod2)

mod <- multinom(data.Classe ~., data = new, maxit = 200)
select = stepwise(mod,direction="forward/backward",criterion="BIC")

coef(select)

proba = fitted(mod)            # Estimated probabilities of each class
observed_class = new$data.Classe
head(data.frame(round(proba,3),observed_class))

proba2 = fitted(select)
head(data.frame(round(proba2,3),observed_class))

fitted_class = predict(mod,type="class")  # Bayes rule
head(fitted_class)

confusion = table(observed_class,fitted_class)
confusion

#### ----

count.eq <- 0
count.pas_eq <- 0
count.prsq_eq <- 0

tz1.tot.eq <- 0
tz2.tot.eq <- 0
tz3.tot.eq <- 0
tz4.tot.eq <- 0
tz5.tot.eq <- 0

tz1.tot.pas_eq <- 0
tz2.tot.pas_eq <- 0
tz3.tot.pas_eq <- 0
tz4.tot.pas_eq <- 0
tz5.tot.pas_eq <- 0

tz1.tot.prsq_eq <- 0
tz2.tot.prsq_eq <- 0
tz3.tot.prsq_eq <- 0
tz4.tot.prsq_eq <- 0
tz5.tot.prsq_eq <- 0

for (i in 1:nrow(new)){
  if (new$data.Classe[i] == "Equilibre"){
    tz1.tot.eq <- tz1.tot.eq + new$pct_tz1[i]
    tz2.tot.eq <- tz2.tot.eq + new$pct_tz2[i]
    tz3.tot.eq <- tz3.tot.eq + new$pct_tz3[i]
    tz4.tot.eq <- tz4.tot.eq + new$pct_tz4[i]
    tz5.tot.eq <- tz5.tot.eq + new$pct_tz5[i]
    
    count.eq <- count.eq + 1
  }
  else if (new$data.Classe[i] == "Pas_Equilibre"){
    tz1.tot.pas_eq <- tz1.tot.pas_eq + new$pct_tz1[i]
    tz2.tot.pas_eq <- tz2.tot.pas_eq + new$pct_tz2[i]
    tz3.tot.pas_eq <- tz3.tot.pas_eq + new$pct_tz3[i]
    tz4.tot.pas_eq <- tz4.tot.pas_eq + new$pct_tz4[i]
    tz5.tot.pas_eq <- tz5.tot.pas_eq + new$pct_tz5[i]
    
    count.pas_eq <- count.pas_eq + 1
  } else {
    tz1.tot.prsq_eq <- tz1.tot.prsq_eq+ new$pct_tz1[i]
    tz2.tot.prsq_eq <- tz2.tot.prsq_eq+ new$pct_tz2[i]
    tz3.tot.prsq_eq <- tz3.tot.prsq_eq+ new$pct_tz3[i]
    tz4.tot.prsq_eq <- tz4.tot.prsq_eq+ new$pct_tz4[i]
    tz5.tot.prsq_eq <- tz5.tot.prsq_eq+ new$pct_tz5[i]
    
    count.prsq_eq <- count.prsq_eq + 1
  }
  
}


count.eq <- 0
count.pas_eq <- 0
count.prsq_eq <- 0

tz1.tot.eq <- tz1.tot.eq / count.eq
tz2.tot.eq <- tz2.tot.eq / count.eq
tz3.tot.eq <- tz3.tot.eq / count.eq
tz4.tot.eq <- tz4.tot.eq / count.eq
tz5.tot.eq <- tz5.tot.eq / count.eq

tz1.tot.pas_eq <- tz1.tot.pas_eq / count.pas_eq
tz2.tot.pas_eq <- tz2.tot.pas_eq / count.pas_eq
tz3.tot.pas_eq <- tz3.tot.pas_eq / count.pas_eq
tz4.tot.pas_eq <- tz4.tot.pas_eq / count.pas_eq
tz5.tot.pas_eq <- tz5.tot.pas_eq / count.pas_eq

tz1.tot.prsq_eq <- tz1.tot.prsq_eq / count.prsq_eq
tz2.tot.prsq_eq <- tz2.tot.prsq_eq / count.prsq_eq
tz3.tot.prsq_eq <- tz3.tot.prsq_eq / count.prsq_eq
tz4.tot.prsq_eq <- tz4.tot.prsq_eq / count.prsq_eq
tz5.tot.prsq_eq <- tz5.tot.prsq_eq / count.prsq_eq

####----

#Z1

tz1.tot.eq - tz1.tot.pas_eq # - 4.6
tz1.tot.eq - tz1.tot.prsq_eq # - 3.6
tz1.tot.pas_eq - tz1.tot.prsq_eq # 0.8

#Z2

tz2.tot.eq - tz2.tot.pas_eq # - 0.2
tz2.tot.eq - tz2.tot.prsq_eq # - 3.4
tz2.tot.pas_eq - tz2.tot.prsq_eq # - 3.2

#Z3

tz3.tot.eq - tz3.tot.pas_eq # - 1.613369
tz3.tot.eq - tz3.tot.prsq_eq # - 2.698564
tz3.tot.pas_eq - tz3.tot.prsq_eq # - 1.085195

#Z4

tz4.tot.eq - tz4.tot.pas_eq # 0.6581158
tz4.tot.eq - tz4.tot.prsq_eq # 0.6324555
tz4.tot.pas_eq - tz4.tot.prsq_eq # - 0.02566029

#Z5
 
tz5.tot.eq - tz5.tot.pas_eq # 5.757406
tz5.tot.eq - tz5.tot.prsq_eq # 9.22534
tz5.tot.pas_eq - tz5.tot.prsq_eq # 3.467934

####----

mod.new <- multinom(data.clas ~ pct_tz1 * pct_tz2 + pct_tz3 + pct_tz5, data = new, maxit = 1000)
select = stepwise(mod.new,direction="forward/backward",criterion="AIC")

coef(select)

proba = fitted(mod.new)            # Estimated probabilities of each class
observed_class = new$data.clas
head(data.frame(round(proba,3),observed_class))

proba2 = fitted(select)
head(data.frame(round(proba2,3),observed_class))

fitted_class = predict(mod,type="class")  # Bayes rule
head(fitted_class)

confusion = table(observed_class,fitted_class)
confusion

