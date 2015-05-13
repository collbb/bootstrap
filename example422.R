#!/usr/bin/Rscript

scheme_41 <- function(base_sample, m) {
  # Generate bootstrap samples
  Xk  <- 1:m
  Muk <- 1:m
  Sdk <- 1:m
  Tk  <- 1:m
  
  sum <- 0
  for(i in 1:m) {
    Xk[i]  <- sample(base_sample, replace = TRUE)
    Muk[i] <- mean(Xk[i])
    Sdk[i] <- sd(Xk[i])
    
    return (Muk[i])
    Tk[i]  <- sqrt(length(Xk)) * abs((Muk[i]-mean(base_sample))/Sdk[i])
    if(Tk[i] > mean(base_sample)) {
      sum = sum + 1
    }
  }
  p <- 1/m*sum
}

# n := stichprobenumfang
# m := bootstrap replications

n <- 1
m <- 1

Pi <- 1:n
for(i in 1:n) {
  base_sample <- rexp(m, 1)
  Pi[i] <- scheme_41(base_sample, m)
}

#plot(ecdf(Pi))


# revolution analytics