#!/usr/bin/Rscript

rm(list=ls())
require(bootstrap)

example_424 <- function(n = 100, m = 100) {
  Pi <- 1:n
  
  for(i in 1:n) {
    Pi[i] <- rss_424(stamp$Thickness, m)
  }
  
  plot(ecdf(Pi), do.points = FALSE, main="Example 4.2.4")
  lines(c(0,1),c(0,1), col="red")
}

rss_424 <- function(data, m = 1000, ho = .01) {
  sum <- 0
  
  To <- gen_hstar(ho, data)
  
  for(k in 1:m) {
    X <- gen_Xstar(data, .5)
    h <- gen_hstar(ho, X)
    
    if(h >= To) {
      sum <- sum + 1
    }
  }
  
  p <- 1/m*sum
  return (p)
}

gen_Xstar <- function(x, h) {
  Xstar <- sample(x, replace = TRUE)
  eps <- rnorm(1, mean = 0, sd = 1)
  Xstar <- Xstar + h*eps
  return (Xstar)
}


gen_hstar <- function(ho, Xstar) {
  hs <- seq(ho, .001, -.001)
  hstar <- hs[1]
  
  for(i in 1:length(hs)) {
    h <- hs[i]
    dens <- density(Xstar, bw = h)
    
    if(has_1mode(dens$y) && (h <= hstar)) {
      hstar <- h
    }
  }
  
  #plot(density(Xstar, bw = hstar))
  return (hstar)
}

has_1mode <- function(y) {
  sum <- 0
  for(i in 2:(length(y)-1)) {
    if(y[i] > y[i-1] && y[i] > y[i+1]) {
      sum <- sum +1
    }
    
    if(sum > 1) {
      return (FALSE)
    }
  }
  
  return (sum == 1)
}

example_424()
