#!/usr/bin/Rscript

rss_42 <- function(base_sample1, base_sample2, m) {
  n <- length(base_sample1)
  l <- length(base_sample2)
  mega_sample <- c(base_sample1, base_sample2)
  
  T <- sqrt(1/n + 1/l) * ((mean(base_sample1) - mean(base_sample2))/sd(mega_sample))
  sum <- 0
  
  for(k in 1:m) {
    Z <- sample(mega_sample, replace = TRUE)
    X <- Z[1:n]
    Y <- Z[(n+1):(n+l)]
    
    snlk <- 1/(n+l)*((n-1)*var(X)+(l-1)*var(Y))
    Tstar <- sqrt(1/n + 1/l) * ((mean(X) - mean(Y))/snlk)
    
    if(Tstar > T) {
      sum <- sum + 1
    }
  }
  p <- 1/m*sum
}

n <- 100
m <- 100
Pi <- 1:n
for(i in 1:n) {
  base_sample1 <- rexp(n)
  base_sample2 <- rexp(n)
  Pi[i] <- rss_42(base_sample1, base_sample2, m)
}

plot(ecdf(Pi), do.points = FALSE, main="RSS 4.2")
lines(c(0,1),c(0,1), col="red")