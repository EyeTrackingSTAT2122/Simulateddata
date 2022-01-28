setwd("C:/Users/aluc/OneDrive - STRATEGIR/Bureau/Conferences  Congres/11 - Pangborn 2021/DATA/Regroupement synonymes_pretraitement")


###############################################################################################
## restructuration data from LIME - Spyder
##############################################################################################

poids_mots_com <- read.csv2("res_poids_LIME_Recodage2_colle_sansunique.csv")
#poids_mots_com <- read.csv2("res_poids_LIME_Recodage3_colle.csv")

poids_mots_com_reform <- data.frame(matrix(ncol = 6, nrow = 1))
n <- dim(poids_mots_com)[1]

a=1
for(i in 1:n){
  mot_na <- which(poids_mots_com[i,]=="")
  poids_mots_com[i,mot_na] <- NA
  nb_mot <- (length(which(!is.na(poids_mots_com[i,])))-6)
  seq_mot <- seq(from=1, to=nb_mot, by=2)
  for(j in seq_mot){
    poids_mots_com_reform[a,1] <- poids_mots_com[i,1]
    poids_mots_com_reform[a,2] <- poids_mots_com[i,2]
    poids_mots_com_reform[a,3] <- poids_mots_com[i,5]
    poids_mots_com_reform[a,4] <- poids_mots_com[i,3]
    poids_mots_com_reform[a,5] <- poids_mots_com[i,(6+j)]
    poids_mots_com_reform[a,6] <- poids_mots_com[i,(6+j+1)]
    a=a+1
  }
}

colnames(poids_mots_com_reform) <- c("ID_juge","Produit","Text","Classe","Mot","Poids")

##
vecteur_mot_poid <- poids_mots_com_reform[,c(2,5:6)]
vecteur_mot_poid <- vecteur_mot_poid[-which(vecteur_mot_poid$Mot=="ras"),]

##########################################################################################
## ttest pour regarder la significativite des poids par rapport à 0

summary(vecteur_mot_poid)
vecteur_mot_poid$Mot <- as.factor(vecteur_mot_poid$Mot)
vecteur_mot_poid$Produit <- as.factor(vecteur_mot_poid$Produit)
nmot <- levels(vecteur_mot_poid$Mot)

# ## qualite
# 
# mot_pvalue_qualite <- data.frame(matrix(ncol=3,nrow = length(nmot)))
# a=1
# for(i in nmot){
#   vec_test <- vecteur_mot_poid[which(vecteur_mot_poid$Mot==i),]
#   if(dim(vec_test)[1]<5){
#     mot_pvalue_qualite[a,1] <- i
#     mot_pvalue_qualite[a,2] <- NA
#     mot_pvalue_qualite[a,3] <- NA
#   }
#   else{
#     res_ttest <- t.test(vec_test$Poids, alternative = "greater")
#     mot_pvalue_qualite[a,1] <- i
#     mot_pvalue_qualite[a,2] <- res_ttest$p.value
#     mot_pvalue_qualite[a,3] <- res_ttest$estimate
#   }
#   a=a+1
# }
# 
# ## defauts
# 
# mot_pvalue_defaut <- data.frame(matrix(ncol=3,nrow = 268))
# a=1
# for(i in nmot){
#   vec_test <- vecteur_mot_poid[which(vecteur_mot_poid$Mot==i),]
#   if(dim(vec_test)[1]<5){
#     mot_pvalue_defaut[a,1] <- i
#     mot_pvalue_defaut[a,2] <- NA
#     mot_pvalue_defaut[a,3] <- NA
#   }
#   else{
#     res_ttest <- t.test(vec_test$Poids, alternative = "less")
#     mot_pvalue_defaut[a,1] <- i
#     mot_pvalue_defaut[a,2] <- res_ttest$p.value
#     mot_pvalue_defaut[a,3] <- res_ttest$estimate
#   }
#   a=a+1
# }

## par produit

prod <- levels(vecteur_mot_poid$Produit)
name_files <- paste("res_signif_poids_recodage2_sansunique_prod",prod, sep="")
#name_files <- paste("res_signif_poids_recodage3_prod",prod, sep="")

for(i in 1:length(prod)){
  mot_pvalue <- data.frame(matrix(ncol=8,nrow = 349))
  a=1
  vecteur_mot_poid_pdt <- vecteur_mot_poid[which(vecteur_mot_poid$Produit==prod[i]),]
  nmot <- as.character(unique(vecteur_mot_poid_pdt$Mot))
  for(j in nmot){
    vec_test <- vecteur_mot_poid_pdt[which(vecteur_mot_poid_pdt$Mot==j),]
    if(dim(vec_test)[1]<2){
      mot_pvalue[a,1] <- j
      mot_pvalue[a,2] <- NA
      mot_pvalue[a,3] <- mean(vec_test$Poids)
      mot_pvalue[a,4] <- dim(vec_test)[1]
      mot_pvalue[a,5] <- j
      mot_pvalue[a,6] <- NA
      mot_pvalue[a,7] <- mean(vec_test$Poids)
      mot_pvalue[a,8] <- dim(vec_test)[1]
    }
    else{
      if(mean(vec_test$Poids)==vec_test$Poids[1]){
        mot_pvalue[a,1] <- j
        mot_pvalue[a,2] <- NA
        mot_pvalue[a,3] <- mean(vec_test$Poids)
        mot_pvalue[a,4] <- dim(vec_test)[1]
        mot_pvalue[a,5] <- j
        mot_pvalue[a,6] <- NA
        mot_pvalue[a,7] <- mean(vec_test$Poids)
        mot_pvalue[a,8] <- dim(vec_test)[1]
      }
      else{
        res_ttest_quali <- t.test(vec_test$Poids, alternative = "greater")
        res_ttest_defaut <- t.test(vec_test$Poids, alternative = "less")
        
        mot_pvalue[a,1] <- j
        mot_pvalue[a,2] <- res_ttest_quali$p.value
        mot_pvalue[a,3] <- res_ttest_quali$estimate
        mot_pvalue[a,4] <- dim(vec_test)[1]
        mot_pvalue[a,5] <- j
        mot_pvalue[a,6] <- res_ttest_defaut$p.value
        mot_pvalue[a,7] <- res_ttest_defaut$estimate
        mot_pvalue[a,8] <- dim(vec_test)[1]
      }
    }
    a=a+1
  }
  colnames(mot_pvalue) <- c("mot_quali","pvalue_quali","mean_quali","occu_mot","mot_defaut","pvalue_defaut","mean_defaut","occu_mot")
  mot_pvalue[,1:4] <- mot_pvalue[order(mot_pvalue$pvalue_quali),1:4]
  mot_pvalue[,5:8] <- mot_pvalue[order(mot_pvalue$pvalue_defaut),5:8]
  write.table(mot_pvalue, str_glue("{name_files[i]}.csv"), sep=";", dec = ",", row.names = FALSE)
}


### moyenne liking par produit

data <- read.csv2("pred_proba_fromage_recodage3_collé.csv")

aggregate(data$Liking, list(data$Produit), mean)


#######################################################################################################
#######################################################################################################
##
## POIDS DES MOTS EN FONCTION DU CLUSTERING CLV
##
#######################################################################################################
#######################################################################################################

library(stringr)

cluster_juge <- read.csv2("classe_juge_CLV_reco2.csv")
cluster1 <- cluster_juge[which(cluster_juge$class==1),1]
cluster2 <- cluster_juge[which(cluster_juge$class==2),1]
cluster3 <- cluster_juge[which(cluster_juge$class==3),1]


poids_mots_com <- read.csv2("res_poids_LIME_Recodage2_colle.csv")
#poids_mots_com <- read.csv2("res_poids_LIME_Recodage3_colle.csv")

poids_mots_com_C1 <- poids_mots_com[which(poids_mots_com$Numero.inter%in%cluster1),]
poids_mots_com_C2 <- poids_mots_com[which(poids_mots_com$Numero.inter%in%cluster2),]
poids_mots_com_C3 <- poids_mots_com[which(poids_mots_com$Numero.inter%in%cluster3),]

##

poids_mots_com_reform <- data.frame(matrix(ncol = 6, nrow = 1))

a=1
for(i in 1:616){
  mot_na <- which(poids_mots_com[i,]=="")
  poids_mots_com[i,mot_na] <- NA
  nb_mot <- (length(which(!is.na(poids_mots_com[i,])))-6)
  seq_mot <- seq(from=1, to=nb_mot, by=2)
  for(j in seq_mot){
    poids_mots_com_reform[a,1] <- poids_mots_com[i,2]
    poids_mots_com_reform[a,2] <- poids_mots_com[i,3]
    poids_mots_com_reform[a,3] <- poids_mots_com[i,4]
    poids_mots_com_reform[a,4] <- poids_mots_com[i,6]
    poids_mots_com_reform[a,5] <- poids_mots_com[i,(6+j)]
    poids_mots_com_reform[a,6] <- poids_mots_com[i,(6+j+1)]
    a=a+1
  }
}

colnames(poids_mots_com_reform) <- c("ID_juge","Produit","Text","Classe","Mot","Poids")
summary(poids_mots_com_reform)
poids_mots_com_reform$Produit <- as.factor(poids_mots_com_reform$Produit)

##

vecteur_mot_poid_C1 <- poids_mots_com_reform[which(poids_mots_com_reform$ID_juge%in%cluster1),]
vecteur_mot_poid_C2 <- poids_mots_com_reform[which(poids_mots_com_reform$ID_juge%in%cluster2),]
vecteur_mot_poid_C3 <- poids_mots_com_reform[which(poids_mots_com_reform$ID_juge%in%cluster3),]

vecteur_mot_poid_C1 <- vecteur_mot_poid_C1[-which(vecteur_mot_poid_C1$Mot=="ras"),]
vecteur_mot_poid_C2 <- vecteur_mot_poid_C2[-which(vecteur_mot_poid_C2$Mot=="ras"),]
vecteur_mot_poid_C3 <- vecteur_mot_poid_C3[-which(vecteur_mot_poid_C3$Mot=="ras"),]


################
## par produit
###############

#### CLUSTER 1

prod <- levels(vecteur_mot_poid_C1$Produit)
name_files <- paste("res_signif_poids_cluster1_recodage2_prod",prod, sep="")
#name_files <- paste("res_signif_poids_recodage3_prod",prod, sep="")

for(i in 1:length(prod)){
  mot_pvalue <- data.frame(matrix(ncol=8,nrow = 1))
  a=1
  vecteur_mot_poid_pdt <- vecteur_mot_poid_C1[which(vecteur_mot_poid_C1$Produit==prod[i]),]
  nmot <- as.character(unique(vecteur_mot_poid_pdt$Mot))
  for(j in nmot){
    vec_test <- vecteur_mot_poid_pdt[which(vecteur_mot_poid_pdt$Mot==j),]
    if(dim(vec_test)[1]<2){
      mot_pvalue[a,1] <- j
      mot_pvalue[a,2] <- NA
      mot_pvalue[a,3] <- mean(vec_test$Poids)
      mot_pvalue[a,4] <- dim(vec_test)[1]
      mot_pvalue[a,5] <- j
      mot_pvalue[a,6] <- NA
      mot_pvalue[a,7] <- mean(vec_test$Poids)
      mot_pvalue[a,8] <- dim(vec_test)[1]
    }
    else{
      if(mean(vec_test$Poids)==vec_test$Poids[1]){
        mot_pvalue[a,1] <- j
        mot_pvalue[a,2] <- NA
        mot_pvalue[a,3] <- mean(vec_test$Poids)
        mot_pvalue[a,4] <- dim(vec_test)[1]
        mot_pvalue[a,5] <- j
        mot_pvalue[a,6] <- NA
        mot_pvalue[a,7] <- mean(vec_test$Poids)
        mot_pvalue[a,8] <- dim(vec_test)[1]
      }
      else{
        res_ttest_quali <- t.test(vec_test$Poids, alternative = "greater")
        res_ttest_defaut <- t.test(vec_test$Poids, alternative = "less")
        
        mot_pvalue[a,1] <- j
        mot_pvalue[a,2] <- res_ttest_quali$p.value
        mot_pvalue[a,3] <- res_ttest_quali$estimate
        mot_pvalue[a,4] <- dim(vec_test)[1]
        mot_pvalue[a,5] <- j
        mot_pvalue[a,6] <- res_ttest_defaut$p.value
        mot_pvalue[a,7] <- res_ttest_defaut$estimate
        mot_pvalue[a,8] <- dim(vec_test)[1]
      }
    }
    a=a+1
  }
  colnames(mot_pvalue) <- c("mot_quali","pvalue_quali","mean_quali","occu_mot","mot_defaut","pvalue_defaut","mean_defaut","occu_mot")
  mot_pvalue[,1:4] <- mot_pvalue[order(mot_pvalue$pvalue_quali),1:4]
  mot_pvalue[,5:8] <- mot_pvalue[order(mot_pvalue$pvalue_defaut),5:8]
  write.table(mot_pvalue, str_glue("{name_files[i]}.csv"), sep=";", dec = ",", row.names = FALSE)
}

#### CLUSTER 2

prod <- levels(vecteur_mot_poid_C2$Produit)
name_files <- paste("res_signif_poids_cluster2_recodage2_prod",prod, sep="")
#name_files <- paste("res_signif_poids_recodage3_prod",prod, sep="")

for(i in 1:length(prod)){
  mot_pvalue <- data.frame(matrix(ncol=8,nrow = 1))
  a=1
  vecteur_mot_poid_pdt <- vecteur_mot_poid_C2[which(vecteur_mot_poid_C2$Produit==prod[i]),]
  nmot <- as.character(unique(vecteur_mot_poid_pdt$Mot))
  for(j in nmot){
    vec_test <- vecteur_mot_poid_pdt[which(vecteur_mot_poid_pdt$Mot==j),]
    if(dim(vec_test)[1]<2){
      mot_pvalue[a,1] <- j
      mot_pvalue[a,2] <- NA
      mot_pvalue[a,3] <- mean(vec_test$Poids)
      mot_pvalue[a,4] <- dim(vec_test)[1]
      mot_pvalue[a,5] <- j
      mot_pvalue[a,6] <- NA
      mot_pvalue[a,7] <- mean(vec_test$Poids)
      mot_pvalue[a,8] <- dim(vec_test)[1]
    }
    else{
      if(mean(vec_test$Poids)==vec_test$Poids[1]){
        mot_pvalue[a,1] <- j
        mot_pvalue[a,2] <- NA
        mot_pvalue[a,3] <- mean(vec_test$Poids)
        mot_pvalue[a,4] <- dim(vec_test)[1]
        mot_pvalue[a,5] <- j
        mot_pvalue[a,6] <- NA
        mot_pvalue[a,7] <- mean(vec_test$Poids)
        mot_pvalue[a,8] <- dim(vec_test)[1]
      }
      else{
        res_ttest_quali <- t.test(vec_test$Poids, alternative = "greater")
        res_ttest_defaut <- t.test(vec_test$Poids, alternative = "less")
        
        mot_pvalue[a,1] <- j
        mot_pvalue[a,2] <- res_ttest_quali$p.value
        mot_pvalue[a,3] <- res_ttest_quali$estimate
        mot_pvalue[a,4] <- dim(vec_test)[1]
        mot_pvalue[a,5] <- j
        mot_pvalue[a,6] <- res_ttest_defaut$p.value
        mot_pvalue[a,7] <- res_ttest_defaut$estimate
        mot_pvalue[a,8] <- dim(vec_test)[1]
      }
    }
    a=a+1
  }
  colnames(mot_pvalue) <- c("mot_quali","pvalue_quali","mean_quali","occu_mot","mot_defaut","pvalue_defaut","mean_defaut","occu_mot")
  mot_pvalue[,1:4] <- mot_pvalue[order(mot_pvalue$pvalue_quali),1:4]
  mot_pvalue[,5:8] <- mot_pvalue[order(mot_pvalue$pvalue_defaut),5:8]
  write.table(mot_pvalue, str_glue("{name_files[i]}.csv"), sep=";", dec = ",", row.names = FALSE)
}

#### CLUSTER 3

prod <- levels(vecteur_mot_poid_C3$Produit)
name_files <- paste("res_signif_poids_cluster3_recodage2_prod",prod, sep="")
#name_files <- paste("res_signif_poids_recodage3_prod",prod, sep="")

for(i in 1:length(prod)){
  mot_pvalue <- data.frame(matrix(ncol=8,nrow = 1))
  a=1
  vecteur_mot_poid_pdt <- vecteur_mot_poid_C3[which(vecteur_mot_poid_C3$Produit==prod[i]),]
  nmot <- as.character(unique(vecteur_mot_poid_pdt$Mot))
  for(j in nmot){
    vec_test <- vecteur_mot_poid_pdt[which(vecteur_mot_poid_pdt$Mot==j),]
    if(dim(vec_test)[1]<2){
      mot_pvalue[a,1] <- j
      mot_pvalue[a,2] <- NA
      mot_pvalue[a,3] <- mean(vec_test$Poids)
      mot_pvalue[a,4] <- dim(vec_test)[1]
      mot_pvalue[a,5] <- j
      mot_pvalue[a,6] <- NA
      mot_pvalue[a,7] <- mean(vec_test$Poids)
      mot_pvalue[a,8] <- dim(vec_test)[1]
    }
    else{
      if(mean(vec_test$Poids)==vec_test$Poids[1]){
        mot_pvalue[a,1] <- j
        mot_pvalue[a,2] <- NA
        mot_pvalue[a,3] <- mean(vec_test$Poids)
        mot_pvalue[a,4] <- dim(vec_test)[1]
        mot_pvalue[a,5] <- j
        mot_pvalue[a,6] <- NA
        mot_pvalue[a,7] <- mean(vec_test$Poids)
        mot_pvalue[a,8] <- dim(vec_test)[1]
      }
      else{
        res_ttest_quali <- t.test(vec_test$Poids, alternative = "greater")
        res_ttest_defaut <- t.test(vec_test$Poids, alternative = "less")
        
        mot_pvalue[a,1] <- j
        mot_pvalue[a,2] <- res_ttest_quali$p.value
        mot_pvalue[a,3] <- res_ttest_quali$estimate
        mot_pvalue[a,4] <- dim(vec_test)[1]
        mot_pvalue[a,5] <- j
        mot_pvalue[a,6] <- res_ttest_defaut$p.value
        mot_pvalue[a,7] <- res_ttest_defaut$estimate
        mot_pvalue[a,8] <- dim(vec_test)[1]
      }
    }
    a=a+1
  }
  colnames(mot_pvalue) <- c("mot_quali","pvalue_quali","mean_quali","occu_mot","mot_defaut","pvalue_defaut","mean_defaut","occu_mot")
  mot_pvalue[,1:4] <- mot_pvalue[order(mot_pvalue$pvalue_quali),1:4]
  mot_pvalue[,5:8] <- mot_pvalue[order(mot_pvalue$pvalue_defaut),5:8]
  write.table(mot_pvalue, str_glue("{name_files[i]}.csv"), sep=";", dec = ",", row.names = FALSE)
}



























