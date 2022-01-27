text <- read_excel("D:/Eye-tracking/Simulateddata/data/data_explicites/Donnees_explicites.xlsm")
t1 <- text[,c(2:3,5,11)]
glimpse(t1)  

corpus = Corpus(VectorSource(t1$Comm_net))
frequencies = DocumentTermMatrix(corpus)
data = as.data.frame(as.matrix(sparse))
colnames(data) = make.names(colnames(data))
data$Classe = t1$Classe


# Essai régression logistique multinomiale

for (i in 1:48){
  data[,i] <- as.factor(data[,i])
}

require(nnet) 
mod = multinom(Classe_3~.,data=data,maxit=200,trace=FALSE) 

require(RcmdrMisc)
select = stepwise(mod,direction="forward/backward",criterion="AIC")
summary(select)

observed = data$Classe_3

acc = rep(0,20) # Initialize a vector of accuracy values
for (k in 1:20) {
  select = stepwise(mod,direction="forward/backward",criterion="AIC",steps=k,trace=0)
  predictions = predict(select,type="class")
  acc[k] = mean(predictions==observed)
}   


require(groupdata2) 
folds = fold(data,k=10,cat_col="Classe_3")$".folds" # Create balanced segments
folds 

cvpredictions = rep("1",nrow(data)) # Initialize a vector of predicted classes

for (j in 1:10) {
  train = data[folds!=j,]
  test = data[folds==j,]
  mod = multinom(Classe_3~.,data=train,trace=FALSE,maxit=200) 
  select = stepwise(mod,direction="forward/backward",criterion="AIC",trace=0)
  cvpredictions[folds==j] = predict(select,newdata=test,type="class")
  cvtest <- as.factor(cvpredictions) ; levels(cvtest) <- levels(train$Classe_3)
  print(paste("Segment ",j,sep=""))
}
mean(cvtest==data$Classe_3)
table (data$Classe_3, cvpredictions) 
