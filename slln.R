#!/usr/bin/Rscript

visualSLLN <- function(rand_vars, n) {
  
  n <- length(rand_vars)
  means <- 1:n
  
  for(i in 1:n) {
    means[i] <- mean(rand_vars[1:i])
  }
  
  plot(means, 
       main = "Graphical representation of the SLLN*",
       sub = "*Strong Law of Large Numbers",
       xlab = "# of random variables included in the m,ean calculation",
       ylab = "Mean of the random variables")
  abline(0, 0, col = "red")
}

n <- 1000
rand_vars <- rnorm(n)

visualSLLN(rand_vars)