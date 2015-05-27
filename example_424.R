#!/usr/bin/Rscript

rm(list=ls())
require(bootstrap)

x <- stamp$Thickness
# density bw = h
#plot(density(x))

gen_Xstar <- function(x, h) {
  Xstar <- sample(x, replace = TRUE)
  eps <- rnorm(1, mean = 0, sd = 1)
  Xstar <- Xstar + h*eps
  return (Xstar)
}

modes <- function(y) {
  sum <- 0
  for(i in 2:length(y)) {
    if(y[i] > y[i-1] && y[i] > y[i+1]) {
      sum <- sum +1
    }
    
    if(sum > 1) {
      return 2
    }
  }
  
  return (sum)
}