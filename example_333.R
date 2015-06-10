#!/usr/bin/Rscript

example_333 <- function(Xn, m, alpha=.95) {
  n <- length(Xn)
  X_n_mean <- mean(Xn)

  Ti <- 1:m
  for (i in 1:m) {
    sample_i <- sample(Xn, n, replace=TRUE)
    
    Xstar_n_mean <- mean(sample_i)
    sd_star <- sd(sample_i)
    
    Ti[i] <- sort(sqrt(n)*(Xstar_n_mean - X_n_mean)/sd_star)
    
    Qa <- Ti[floor(alpha*i)]
    Qaa <- Ti[ceiling((1-alpha)*i)]
  }
  
  
}

univars <- function(n) {
  return (runif(n) * 6)
}

expvars <- function(n) {
  return (rexp(n))
}

example_333(univars(50), 1000)
example_333(expvars(50), 1000)