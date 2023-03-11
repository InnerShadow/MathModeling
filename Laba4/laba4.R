
task3_1 <- function(vec){
  plot(x = density(vec), xlim = c(0, 1), col = "blue")
  curve(dens, add = T, col = "red")
  grid(nx = 15, ny = 15, lty = 2, col = "gray", lwd = 1)
  legend(0.2, 0.2, c("red - teoretical", "blue - empirical"))
}

task3_2 <- function(vec){
  prob <- rep(100, 0)
  step <- 1 / 100
  sigment <- seq(step / 2, 1 - step / 2, length = 100)
  teoretical <- hist(vec, breaks = 100)$counts
  #task3_1(vec)
  
  for(i in 1 : 100){
    prob[i] <- integrate(dens, sigment[i] - step / 2, sigment[i] + step / 2)$value
  }
  chisqr <- c(0)
  for(i in 1 : 100){
    chisqr <- chisqr + (teoretical[i] - prob[i] * length(vec)) ^ 2 / (prob[i] * length(vec))
  }
  cat("counted chi^2:", chisqr, "table 95% chi^2:", qchisq(.95, df = 99), 
      "\nis counted < critical p-value?", (chisqr < qchisq(.95, df = 99)), "\n")
}

task3_3 <- function(vec){
  vec <- sort(vec)
  omegasqr <- 1 / (12 * length(vec))
  for(i in 1 : length(vec)){
    omegasqr <- omegasqr + (propobility(vec[i]) - (i - .5) / length(vec)) ^ 2
  }
  cat("counted omega^2:", omegasqr, "table 95% chi^2:", .46, 
      "\nis counted < critical p-value?", (omegasqr < .46), "\n")
}

{

  dens <- function(x) 1.5 * x ^ (1 / 2)
  
  propobility <- function(x) x ^ (3 / 2)
  
  l <- function(x) 1 / G + 0 * x
  
  G <- max(dens(seq(0, 1, length = 10000)))
  
  r <- G / (integrate((function(x) (G + x * 0)), 0, 1)$value) # x' = runif(1, 0, 1)
                                                              # y' = 3 / 2 * runif(1, 0, 1)
  {
    N <- 10000
    i <- 1
    
    x <- rep(0, 10000)
    
    while(i <= N){
      xdot <- runif(1, 0, 1)
      ydot <- G * runif(1, 0, 1)
      if(ydot <= dens(xdot)){
        x[i] <- xdot
        i <- i + 1
      }
    }
  }
  
  task3_2(x)
  
  task3_3(x)
  
  task3_1(x)

}
