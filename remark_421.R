#!/usr/bin/Rscript

xx <- function() {
  sample_size <- 100
  m <- 1000
  
  Xn <- rexp(sample_size)
  Xmean <- mean(Xn)
  
  sum <- 0;
  Ti <- 1:m
  for(i in 1:m) {
    Yn <- rexp(sample_size, 1.1)
    Ymean <- mean(Yn)
    
    Ti[i] <- Ymean
    if(mean(Yn) >= Xmean) {
      sum = sum + 1
    }
  }
  return (sum/m)
}

xxx <- function() {
  n <- 100
  Pi <- 1:n
  for(i in 1:n) {
    Pi[i] <- xx()
  }
  
  plot(ecdf(Pi), do.points = FALSE)
}

xxx()