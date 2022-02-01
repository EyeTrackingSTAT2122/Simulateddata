library(readr)
require(RcmdrMisc)       # For Stepwise
require(nnet)            # Multinomial logistic regression
require(leaps)           # For regsubsets
library(dplyr)
library(groupdata2)

data<- read_csv("data/data_fix_finales/data_finales_supp_classes.csv")

FL <- read_csv("data/data_fix_finales/last_first_supp.csv")


data1 <- data %>%
  full_join(FL, by = c("num_stimulus", "id", "id_item", "id_tester")) %>% 
  dplyr::select(classe, pays, starts_with("pct"), cat_max_fix, first_zone, last_zone) %>% 
  data.frame()


data2 <- data %>%
  dplyr::select(classe, pays, starts_with("pct"), cat_max_fix) %>% 
  data.frame()

data_step<- read_csv("data/data_fix_finales/data_step.csv")
mod = multinom(classe~.,data=data_step)
select = stepwise(mod,direction="forward/backward",criterion="AIC")

## Accuracy for the best sub-models

observed = data_step$classe

### Accuracy values for best submodels
acc = rep(0,5) # Initialize a vector of accuracy values
for (k in 1:5) {
  select = stepwise(mod,direction="forward/backward",criterion="AIC",steps=k,trace=0)
  predictions = predict(select,type="class")
  acc[k] = mean(predictions==observed)
}   

mean(acc)
