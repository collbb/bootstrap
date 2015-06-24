#!/usr/bin/Rscript

rm(list=ls())
require("R.utils", quietly = TRUE)

# Theorem 5.2.2 - bound calculation
ex_5.1.i <- function(x, y) {
  alpha    <- .01
  lin.mod  <- lm(x~y)
  est.beta <- as.numeric(lin.mod$coefficients[1])
  quant    <- qnorm(1-alpha/2)
  n        <- length(x)
  sn       <- my.sn(lin.mod$residuals, x, n)

  lower.bound <- est.beta - (quant*sn/sqrt(n))
  upper.bound <- est.beta + (quant*sn/sqrt(n))
  
  # print solution
  printf("                   estimated beta = %.4f\n", est.beta)
  printf("Theorem 5.2.2 (CLT) - lower bound = %.4f\n", lower.bound)
  printf("Theorem 5.2.2 (CLT) - upper bound = %.4f\n", upper.bound)
}

failure <- function(bound, est.value) {
  return (100 - abs(bound/est.value) * 100)
}

# Theorem 5.3.3 - bound calculation
ex_5.1.ii <- function(x, y, m = 1000) {
  n        <- length(x)
  lin.mod  <- lm(y~x-1)
  est.beta <- as.numeric(lin.mod$coefficients[1])
  eps      <- lin.mod$residuals
  eps.cent <- eps - mean(eps)
  sn       <- my.sn(lin.mod$residuals, x, n)
  
  Ts  <- c(1:m)
  for(i in Ts) {
    # 1 resample
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
  
  printf("Theorem 5.3.3 - lower bound = %.4f\n", lower.bound)
  printf("Theorem 5.3.3 - upper bound = %.4f\n", upper.bound)
}

my.sn <- function(residuals, x, n) {
  emp.sigma.sq  <- var(residuals)
  V_inverse     <- 1/((x%*%x)/n)
  return (sqrt(emp.sigma.sq * V_inverse))
}

# creation of simulated observations
n          <- 50
beta.given <- .5
var.given  <- 4

eps  <- rnorm(n, mean = 0, sd = sqrt(var.given))
x    <- c(1:n)/n
y    <- x * beta.given + eps

printf("                  Real beta = %.4f\n\n", beta.given)

# exercise 5.1 i)
ex_5.1.i(x, y)

# exercise 5.1 ii)
ex_5.1.ii(x, y)
