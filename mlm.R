#!/usr/bin/Rscript

lssq.lm <- function(xi, yi)
{
  xm <- sum(xi) / length(xi)
  ym <- sum(yi) / length(yi)
  
  xd <- xi - xm
  yd <- yi - ym
  
  xq <- xd * xd
  yq <- yd * yd
  
  prod <- xd*yd
  
  a <- sum(prod) / sum(xq)
  b <- ym - a * xm
  
  return (create_returnvalue(xi, yi, b, a))
}

proj.lm <- function(x, y) {
  X           <- matrix(1, length(x), 2)
  X[,2]       <- x
  
  a_estimator <- solve(t(X) %*% X) %*% t(X) %*% y
  b           <- a_estimator[2,1]
  a           <- a_estimator[1,1]
  
  return (create_returnvalue(x, y, a, b))
}

create_returnvalue <- function(x, y, a, b) {
  fit       <- b * x + a
  
  residuums <- y     - fit
  
  return (list(
    x.values=x, 
    y.values=y, 
    fitted.values=fit, 
    residuals=residuums, 
    coefficients=c(a, b)))
}

proj.example <- function() {
  n <- 50
  a <- 2
  x <- c(1:n)
  y <- a*x + rnorm(n, sd=5)
  #
  proj <- proj.lm(x, y)
  #
  plot(y)
  abline(proj$coefficients[1], proj$coefficients[2], col = "red")
}

proj.example()
