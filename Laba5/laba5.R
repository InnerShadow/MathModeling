
chisqr <- function(vec){
  prob <- rep(10, 0)
  teoretical <- hist(vec, breaks = 10)$counts
  
  prob <- rep(0.1, 10)
  
  chisqr <- c(0)
  for(i in 1 : 10){
    chisqr <- chisqr + (teoretical[i] - prob[i] * length(vec)) ^ 2 / (prob[i] * length(vec))
  }
  cat("counted chi^2:", chisqr, "table 95% chi^2:", qchisq(.95, df = 9), 
      "\nis counted < critical p-value?", (chisqr < qchisq(.95, df = 9)), "\n")
}

{
  N <- c(1000)
  
  {
    x <- rep(0, N + 1)
    for(i in 1 : N){
      x[i] <- ceiling(runif(1, 0, 1) * 10)
    }
  }
  
  
  cat("empirical mean:", mean(x), "teoretical:", sum(seq(1, 10, by = 1) * rep(0.1, 10)), "\n")
  
  teoreticalMean <- c(sum(seq(1, 10, by = 1) * rep(0.1, 10)))
  
  cat("empirical variance:", var(x), 
      "teoretical:", sum((seq(1, 10, by = 1) - rep(teoreticalMean, 10)) ^ 2 * rep(0.1, 10)), "\n")
  
  chisqr(x)
  
  {
    colums <- hist(x, breaks = 10, xlim = c(0, 10))$density
    plot(x = seq(1, 10, by = 1), y = rep(0.1, 10))
    lines(x = seq(1, 10, by = 1), y = rep(0.1, 10), lwd = 3, col = "red")
    lines(x = seq(1, 10, by = 1), y = colums, lwd = 3, col = "blue")
    grid(nx = 15, ny = 15, lty = 2, col = "gray", lwd = 1)
    legend(4, .13, c("red - teoretical", "blue - empirical"))
  }
}

solveVar17()

