gen_nd <- function(n,m,rate) {
  y<-1:n
  for(i in 1:m) {
    x     <- rexp(n, rate)
    x_n   <- mean(x)
    s     <- sd(x)
    y[i]  <- sqrt(n) * (x_n - 1/rate)/s
  }
  
  hist(y, freq=FALSE, breaks=50)
  curve(dnorm, col='red', add=TRUE)
}

n = 10000
gen_nd(n, n, 1)