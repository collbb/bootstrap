#!/usr/bin/Rscript

rm(list=ls())

rss_44 <- function(X, m = 1000) {
  Dn <- ks.test(X, "pnorm", mean(X), sd(X))$statistic
  sum <- 0
  
  for(k in 1:m) {
    Xk <- rnorm(n, mean = mean(X), sd = sqrt(var(X)))
    Zk <- rnorm(n, mean = mean(Xk), sd = sqrt(var(Xk)))
    
    # Dn = Kolm.-Smirn.
    Dk <- ks.test(Xk, "pnorm", mean(Xk), sd(Xk))$statistic
    
    if(Dk >= Dn) {
      sum <- sum + 1
    }
  }
  p <- 1/m*sum
  return (p)
}

n <- 100 
m <- 1000
Pi <- 1:n
for(i in 1:n) {
  X <- rnorm(n)
  Pi[i] <- rss_44(X, m)
}

plot(ecdf(Pi), do.points = FALSE, main="RSS 4.4")
lines(c(0,1),c(0,1), col="red")
