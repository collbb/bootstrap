#!/usr/bin/Rscript

rm(list=ls())

# creation of simulated observations
n <- 50
beta <- .5
sigmasq <- 4
eps <- rnorm(n, mean = 0, sd = sqrt(sigmasq))
x <- c(1:n)/n

y <- x * beta + eps

proj <- lm(x~y)

plot(y)
abline(proj$coefficients[1], proj$coefficients[2], col = "red")

# bound calculation
alpha <- .01
est.beta <- proj$coefficients[1]
quant <- qnorm(alpha)

my.sn <- function(residuals, x, n) {
  return (sqrt(var(residuals)) - solve(x%*%x/n))
}

sn <- my.sn(proj$residuals, x, n)

upper.bound <- est.beta - (-quant*sn/sqrt(n))
lower.bound <- est.beta - (quant*sn/sqrt(n))
print(upper.bound)
print(lower.bound)

# bootstrap (HAS FAILURES!!!)
m <- 1000
Ts <- c(1:m)
for(i in 1:m) {
  # 1 resample
  eps <- eps-mean(eps)
  eps.star <- sample(eps, length(eps), replace = TRUE)
  y.star <- x * est.beta + eps.star
  # 2 calculate T
  proj.star <- lm(x~y.star)
  est.beta.star <- proj.star$coefficients[1]
  sn.star <- my.sn(proj.star$residuals, x, n)
  Ts[i] <- sqrt(n) * (est.beta.star - est.beta)/sqrt(sn.star)
}

# sort T's
Ts <- sort(Ts)
# lower
print(Ts[10])
# upper
print(Ts[990])
