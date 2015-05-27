#!/usr/bin/Rscript

rm(list=ls())

rss_43 <- function(X, Y, m = 1000) {
  X <- X-mean(X)
  n <- length(X)
  
  Y <- Y-mean(Y)
  l <- length(Y)
  
  T <- (mean(X) - mean(Y))/sqrt(sd(X)/n+sd(Y)/l)
  sum <- 0
  
  for(k in 1:m) {
    Xk <- sample(X, replace = TRUE)
    Yk <- sample(Y, replace = TRUE)    
    Tk <- (mean(Xk) - mean(Yk))/sqrt(sd(Xk)/n+sd(Yk)/l)
    if(Tk >= T) {
      sum <- sum + 1
    }
  }
  p <- 1/m*sum
  return (p)
}

n <- 100
m <- 100
Pi <- 1:n
for(i in 1:n) {
  # probiere mit gleicher und unterschiedlichen mittelwerten/varianzen
  base_sample1 <- rnorm(n, mean = 17, sd = 1)
  base_sample2 <- rnorm(n, mean = 17, sd = 1)
  Pi[i] <- rss_43(base_sample1, base_sample2, m)
}

plot(ecdf(Pi), do.points = FALSE, main="RSS 4.3")
lines(c(0,1),c(0,1), col="red")