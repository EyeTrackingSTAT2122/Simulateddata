library(readr)
library(dplyr)

#Text mining packages
library(tm)
library(SnowballC)


#loading the data
text <- read_excel("D:/Eye-tracking/Simulateddata/data/data_explicites/Donnees_explicites.xlsm")
t1 <- text[,c(2:3,5,11)]

glimpse(t1)  

corpus = Corpus(VectorSource(t1$Comm_net))

corpus[[1]][1]

t1$Classe_3[1]

frequencies = DocumentTermMatrix(corpus)

sparse = removeSparseTerms(frequencies, 0.995)

tSparse = as.data.frame(as.matrix(sparse))

colnames(tSparse) = make.names(colnames(tSparse))

tSparse$Classe_3 = t1$Classe_3

prop.table(table(tSparse$Classe_3))

library(caTools)

set.seed(100)

split = sample.split(tSparse$Classe_3, SplitRatio = 0.7)

trainSparse = subset(tSparse, split==TRUE)

testSparse = subset(tSparse, split==FALSE)

library(randomForest)

set.seed(100)

trainSparse$Classe_3 = as.factor(trainSparse$Classe_3)

testSparse$Classe_3 = as.factor(testSparse$Classe_3)

RF_model = randomForest(Classe_3 ~ ., data=trainSparse)

predictRF = predict(RF_model, newdata=testSparse)

table <- table(testSparse$Classe_3, predictRF)

accuracy <- (45+16+12)/(45+16+12+2+10+2+5+19+9)
