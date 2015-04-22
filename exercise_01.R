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

#n = 10000
#gen_nd(n, n, 1)

gen_nd_boxmul <- function(n) {
  y <- 1:n
  for(i in 1:n) {
    U <- runif(1)
    V <- runif(1)
  
    X <- sqrt(-2*log(U)) * cos(2*pi*V)
    Y <- sqrt(-2*log(U)) * sin(2*pi*V)
    
    y[i + 0] <- X
    y[i + 1] <- Y
  }
  
  hist(y, freq=FALSE, breaks=50)
  curve(dnorm, col='red', add=TRUE)
}

n <- 10000
gen_nd_boxmul(n)