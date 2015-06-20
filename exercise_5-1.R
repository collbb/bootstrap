#!/usr/bin/Rscript

rm(list=ls())
require("R.utils", quietly = TRUE)

ex_5.1.i <- function(x, y) {
  # Theorem 5.2.2 - bound calculation
  alpha    <- .01
  lin.mod  <- lm(x~y)
  est.beta <- as.numeric(lin.mod$coefficients[1])
  quant    <- qnorm(1-alpha/2)
  n        <- length(x)
  sn       <- my.sn(lin.mod$residuals, x, n)
  
  lower.bound <- est.beta - (quant*sn/sqrt(n))
  upper.bound <- est.beta + (quant*sn/sqrt(n))
  
  # print solution
  printf("             estimated beta = %.4f\n", est.beta)
  printf("Theorem 5.2.2 - lower bound = %.4f\n", lower.bound)
  printf("Theorem 5.2.2 - upper bound = %.4f\n", upper.bound)
}

ex_5.1.ii <- function() {
  # bootstrap - bound calculation
  m  <- 1000
  Ts <- c(1:m)
  
  for(i in 1:m) {
    # 1 resample
    eps      <- eps - mean(eps)
    eps.star <- sample(eps, length(eps), replace = TRUE)
    y.star   <- x * est.beta + eps.star
    
    # 2 calculate T
    proj.star     <- lm(x~y.star)
    est.beta.star <- as.numeric(proj.star$coefficients[1])
    sn.star       <- as.numeric(my.sn(proj.star$residuals, x, n))
    
    Ts[i]         <- sqrt(n) * (est.beta.star - est.beta)/sqrt(sn.star)
  }
  
  # sort T's
  Ts <- sort(Ts)
  
  printf("Bootstrap - lower bound = %.2f\n", Ts[990])
  printf("Bootstrap - upper bound = %.2f\n", Ts[10])
}

my.sn <- function(residuals, x, n) {
  emp.sigma.sq  <- var(residuals)
  V_inverse     <- solve(x%*%x)/n
  return (sqrt(emp.sigma.sq * V_inverse))
}

# creation of simulated observations
n          <- 50
beta.given <- .5
var.given  <- 4

eps  <- rnorm(n, mean = 0, sd = sqrt(var.given))
x    <- c(1:n)/n
y    <- x * beta.given + eps

printf("                  Real beta = %.4f\n", beta.given)

# exercise 5.1 i)
ex_5.1.i(x, y)

# exercise 5.1 ii)
#ex_5.1.ii(x, y)