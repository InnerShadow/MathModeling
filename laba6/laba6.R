
T0 <- 0
Tend <- .4

N <- 10

lambda <- 10

x <- rep(0, N)
y <- matrix(data = rep(x, 10), ncol = 10, nrow = 10, byrow = F)

for(j in 1 : N){
  t <- T0
  i <- 1
  while(T){
    t <- t - (log(runif(1, 0, 1)) / lambda)
    if (t > Tend){
      break
    }
    y[i, j] <- t
    i <- i + 1
  }
}

intensivity <- ceiling(y)



