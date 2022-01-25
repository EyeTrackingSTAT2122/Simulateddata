library(readr)
library(FactoMineR)

data_AFM <- read_csv("E:/Master 2/Simulateddata/data/data_AFM.csv")

data_AFM <- as.data.frame(data_AFM)

data_AFM$num_stimulus <- as.factor(data_AFM$num_stimulus)

for (i in 2:49){
  data_AFM[,i] <- as.factor(data_AFM[,i])
  levels(data_AFM[,i]) <- c("Equilibre","PasEquilibre","PresqueEquilibre")
}

data_AFM <- data_AFM[, -c(55:57)]

Equilibre <- rep(0,9)
PasEquilibre <- rep(0,9)
PresqueEquilibre <- rep(0,9)

data_join <- cbind(data_AFM,Equilibre,PasEquilibre,PresqueEquilibre)

for (j in 1:nrow(data_AFM)){
  for (i in 2:49){
    if (data_join[j,i] == "Equilibre"){
      data_join$Equilibre[j] <- data_join$Equilibre[j] + 1 
    }
    else if (data_join[j,i] == "PasEquilibre"){
      data_join$PasEquilibre[j] <-  data_join$PasEquilibre[j] + 1
    } else {
      data_join$PresqueEquilibre[j] <- data_join$PresqueEquilibre[j] + 1
    }
  }
}

data_join <- read_csv("data_join.csv")


data_SPL <- as.data.frame(data_join)

data_SPL$pct_proteines_tps <- data_SPL$pct_produits_laitiers_tps + data_SPL$pct_proteines_tps

rownames(data_SPL) <- data_SPL$num_stimulus

data_SPL <- data_SPL[,-c(1,31,54)]

MFA(data_SPL, group = c(47,4,3), type = c("n","s","f"))

data_SPL2 <- as.data.frame(data_join)

rownames(data_SPL2) <- data_SPL2$num_stimulus

data_SPL2 <- data_SPL2[,-c(1,31)]

MFA(data_SPL2, group = c(47,5,3), type = c("n","s","f"))

