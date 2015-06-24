#!/usr/bin/Rscript

rm(list=ls())
require("R.utils", quietly = TRUE)

clt.bounds <- function(x, y, alpha=.1) {
  lin.mod  <- lm(y~x-1)
  est.beta <- as.numeric(lin.mod$coefficients[1])
  quant    <- qnorm(1-alpha/2)
  n        <- length(x)
  sn       <- sd(x)
  
  lower.bound <- est.beta - (quant*sn/sqrt(n))
  upper.bound <- est.beta + (quant*sn/sqrt(n))
  
  # print solution
  printf("   estimated beta = %.4f\n", est.beta)
  printf("CLT - lower bound = %.4f\n", lower.bound)
  printf("CLT - upper bound = %.4f\n", upper.bound)
  plot(main="X and Y data", x, y, xlab="X data", ylab="y data")
}

bootstrap.bounds <- function(x, y, m = 1000) {
  n        <- length(x)
  lin.mod  <- lm(y~x-1)
  est.beta <- as.numeric(lin.mod$coefficients[1])
  eps      <- lin.mod$residuals
  sn       <- my.sn(lin.mod$residuals, x, n)
  
  Ts  <- c(1:m)
  for(i in Ts) {
    # 1 resample
    
    ### must be modified!!!
    eps.star <- sample(eps.cent, replace = TRUE)
    Z        <- x * est.beta + eps.star
    
    # 2 calculate T's
    lin.mod.star  <- lm(Z~x-1)
    est.beta.star <- as.numeric(lin.mod.star$coefficients[1])
    sn.star       <- as.numeric(my.sn(lin.mod.star$residuals, x, n))
    
    Ts[i]         <- sqrt(n) * (est.beta.star - est.beta)/sn.star
  }
  
  # sort T's
  Ts <- sort(Ts)
  
  lower.bound <- est.beta - (Ts[950]*sn/sqrt(n))
  upper.bound <- est.beta - (Ts[50]*sn/sqrt(n))
  
  printf("Bootstrap - lower bound = %.4f\n", lower.bound)
  printf("Bootstrap - upper bound = %.4f\n", upper.bound)
}

my.sn <- function(x) {
  n               <- length(n)
  big.eps.inverse <- 1/((x%*%x)/n)
  big.m           <- 
    
  return (sqrt(big.eps.inverse * big.m * big.eps.inverse))
}

# creation of simulated observations
n          <- 50
beta.given <- 3

eps  <- rnorm(n, mean = 0, sd = 1)
x    <- runif(n, min = -2, max = 2)
y    <- beta.given * x + eps

printf("        real beta = %.4f\n", beta.given)
clt.bounds(x, y, alpha = .1)
bootstrap.bounds(x, y)
