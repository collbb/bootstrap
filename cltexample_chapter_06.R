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

# creation of simulated observations
n          <- 50
beta.given <- 3

eps  <- rnorm(n, mean = 0, sd = 1)
x    <- runif(n, min = -2, max = 2)
y    <- beta.given * x + eps

printf("        real beta = %.4f\n", beta.given)
clt.bounds(x, y, alpha = .1)
