library(readr)
library(RcmdrMisc)       
library(nnet)            
library(dplyr)

data_step<- read_csv("data/data_fix_finales/data_step.csv")
mod = multinom(classe~.,data=data_step)
select = stepwise(mod,direction="forward/backward",criterion="AIC")



observed = data_step$classe

acc = rep(0,5) 
for (k in 1:5) {
  select = stepwise(mod,direction="forward/backward",criterion="AIC",steps=k,trace=0)
  predictions = predict(select,type="class")
  acc[k] = mean(predictions==observed)
}   

mean(acc)