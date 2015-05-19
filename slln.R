#!/usr/bin/Rscript

visualSLLN <- function() {
  
  n <- 1000
  rand_vars <- rnorm(n)
  
  means <- 1:n
  for(i in 1:n) {
    means[i] <- mean(rand_vars[1:i])
  }
  
  plot(means)
  abline(0, 0)
}

visualSLLN()