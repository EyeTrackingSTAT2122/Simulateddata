x <- rep(0,100)
y <- rep(0,100)
duration <- rep(0,100)


for (i in 1:50){
  x[i] <-  rnorm(1,25,10)
  y[i] <- rnorm(1,50,30)
}

for (i in 51:100){
  x[i] <-  rnorm(1,75,10)
  y[i] <- rnorm(1,50,30)
}

df <- data.frame(x,y,duration)

library(ggplot2)
ggplot(df[1:50,], aes(x=x,y=y,fill=duration)) + 
  geom_point() + 
  scale_x_continuous(limits=c(0,100)) +
  scale_y_continuous(limits=c(0,100)) + 
  theme_classic()

ggplot(df[51:100,], aes(x=x,y=y,fill=duration)) + 
  geom_point() + 
  scale_x_continuous(limits=c(0,100)) +
  scale_y_continuous(limits=c(0,100)) + 
  theme_classic()
