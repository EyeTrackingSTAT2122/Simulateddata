N <- 100

rho <- 0
mu1 <- 25; s1 <- 5
mu2 <- 75; s2 <- 5

mu <- c(mu1,mu2)
sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2)

library(MASS)
bvn1 <- mvrnorm(N, mu = mu, Sigma = sigma )
colnames(bvn1) <- c("bvn1_X1","bvn1_X2")

plot(bvn1,xlim=c(0,100),ylim=c(0,100))
