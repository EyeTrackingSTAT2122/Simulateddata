library(readr)
library(sjmisc)
library(tidyverse)
library(tidyr)
library(dplyr)

coord_split1 <- read.csv("data/Real_eye/data_finale.csv", sep=",")
coord_split2 <- read.csv("data/Real_eye/data_sauvetage.csv", sep=",")


for (i in 1: nrow(coord_split2)){
  if (coord_split2$item_id[i] ==  "6bfa3bbf-ff8f-4719-ade6-08308e2edfe8"){
    coord_split2$item_id[i]<- "34359006-f70f-438e-85d0-420cf2fe6944"
  }
  else if (coord_split2$item_id[i] ==  "2cce5dbf-5148-49e7-94fc-5b8f962a6aff"){
    coord_split2$item_id[i]<- "20f3c152-6ebf-49db-8e1a-00eebd99feb6"
  }
  else if (coord_split2$item_id[i] ==  "84c7b3b8-af92-4987-a4ed-a4e9075e2855"){
    coord_split2$item_id[i]<- "213479f7-39af-4b83-b0e9-4ba473d27429"
  }
  else if (coord_split2$item_id[i] ==  "6f741a6e-b15c-48da-b14d-89aa117da85f"){
    coord_split2$item_id[i]<- "7bdf6ed6-acf9-4a7b-84fe-7e4f96f9582a"
  }
  else if (coord_split2$item_id[i] ==  "37df9db8-beab-4ebe-8538-add6525037e6"){
    coord_split2$item_id[i]<- "cc2a8b56-732c-4e4c-a058-248783b68587"
  }
  else if (coord_split2$item_id[i] ==  "e74916b7-bce5-4833-baae-c57998764f8b"){
    coord_split2$item_id[i]<- "dd13fcba-0ae1-43d3-9d32-1672ed84f151"
  }
  else if (coord_split2$item_id[i] ==  "e88439ca-3bc1-490c-aa6d-93da2d85b083"){
    coord_split2$item_id[i]<- "6afda059-04a5-4308-b776-a532ed142284"
  }
  else if (coord_split2$item_id[i] ==  "1652d0e1-27ce-4743-9313-b303c97ad7c2"){
    coord_split2$item_id[i]<- "f61f9030-d814-495d-af95-0e643697de64"
  }
  else if (coord_split2$item_id[i] ==  "0ca872a3-4c92-4386-8cb2-ebf530c1950e"){
    coord_split2$item_id[i]<- "5445f170-12c1-48fc-9563-3709dd2993b0"
  }
  else if (coord_split2$item_id[i] ==  "35a479cb-716e-4da9-8ff9-2af4c7de6c12"){
    coord_split2$item_id[i]<- "fab315cc-9769-4e2b-bef7-ed998d3754ce"
  }

}


coord_split <- rbind(coord_split1,coord_split2)

#write.table(coord_split, "data/data_support/coord_split.csv", row.names=FALSE, sep=",",dec=".", na=" ")


#Séparation des blocs : un bloc correspondant à un ensemble de coordonées de point. 
#Un bloc correspond à un stimulus vu par une personne

num_test <- rep(0, 0)
tester_id <- rep(0, 0)
item_id <- rep (0, 0)
bloc <- data.frame(num_test, tester_id, item_id)

j =1
for (i in 1:nrow(coord_split)){
  if (!is.na(coord_split[i,"tester_quality_grade"])){
    bloc[j,"num_test"] <- i
    bloc[j,"tester_id"] <- coord_split[i,"tester_id"]
    bloc[j,"item_id"] <- coord_split[i,"item_id"]
    j = j+1
  }
  print(i)
}

num1<- rep(0, 0)
num2 <- rep(0,0)
tester_id <- rep(0, 0)
item_id <- rep (0, 0)
split <- data.frame(num1, num2, tester_id, item_id)

j=1
for (i in 1:(nrow(bloc))){
  if (bloc$item_id[i] != "fab315cc-9769-4e2b-bef7-ed998d3754ce"){
    split[j,"num1"] <- bloc$num_test[i]
    split[j,"num2"] <- bloc$num_test[i+1]
    split[j,"tester_id"] <- bloc$tester_id[i]
    split[j,"item_id"] <- bloc$item_id[i]
    j = j+1
  }
}
split[nrow(split),"num2"] = nrow(coord_split)



split[,"pays"]<- rep(0,nrow(split))
split[,"num_stimulus"]<- rep(0,nrow(split))
i=1
for (i in 1:nrow(split)){
  if(split$item_id[i] == "34359006-f70f-438e-85d0-420cf2fe6944"){
    split[i, "pays"] <- "Ukraine"
    split[i,"num_stimulus"] <- 156
  }
  else if(split$item_id[i] == "213479f7-39af-4b83-b0e9-4ba473d27429"){
    split[i, "pays"] <- "Spain"
    split[i,"num_stimulus"] <- 756
  }
  else if(split$item_id[i] == "7bdf6ed6-acf9-4a7b-84fe-7e4f96f9582a"){
    split[i, "pays"] <- "France"
    split[i,"num_stimulus"] <- 421
  }
  else if(split$item_id[i] == "cc2a8b56-732c-4e4c-a058-248783b68587"){
    split[i, "pays"] <- "South Korea"
    split[i,"num_stimulus"] <- 489
  }
  else if(split$item_id[i] == "dd13fcba-0ae1-43d3-9d32-1672ed84f151"){
    split[i, "pays"] <- "USA"
    split[i,"num_stimulus"] <- 327
  }
  else if(split$item_id[i] == "6afda059-04a5-4308-b776-a532ed142284"){
    split[i, "pays"] <- "Italy"
    split[i,"num_stimulus"] <- 238
  }
  else if(split$item_id[i] == "f61f9030-d814-495d-af95-0e643697de64"){
    split[i, "pays"] <- "Brazil"
    split[i,"num_stimulus"] <- 352
  }
  else if(split$item_id[i] == "5445f170-12c1-48fc-9563-3709dd2993b0"){
    split[i, "pays"] <- "Greece"
    split[i,"num_stimulus"] <- 980
  }
  else if(split$item_id[i] == "20f3c152-6ebf-49db-8e1a-00eebd99feb6"){
    split[i, "pays"] <- "Finland"
    split[i,"num_stimulus"] <- 672
  }
}

#write.table(split, "data/data_support/split.csv", row.names=FALSE, sep=",",dec=".", na=" ")