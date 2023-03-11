
chisqrVar17 <- function(vec, prob, teoretical){
  chisqr <- c(0)
  for(i in 1 : 11){
    chisqr <- chisqr + ((teoretical[i] - probobilytis[i]) ^ 2 / (probobilytis[i]))
  }
  cat("counted chi^2:", chisqr, "table 95% chi^2:", qchisq(.95, df = 10), 
      "\nis counted < critical p-value?", (chisqr < qchisq(.95, df = 10)))
}

solveVar17 <- function(){
  N <- c(1000)
  {
    
    probobilytis <- rep(0, 11)
    for(i in 1 : 11){
      probobilytis[i] <- choose(10, i - 1) * .3 ^ (i - 1) * .7 ^ (11 - i)
    }
    
    x <- rep(0, N)
    
    for(i in 1 : N){
      zi <- runif(1, 0, 1)
      for(j in 0 : 10){
        p <- choose(10, j) * .3 ^ j * .7 ^ (10 - j)
        zi <- zi - p
        if(zi < 0){
          x[i] <- j
          break
        }
      }
    }
  }
    
  cat("empirical mean:", mean(x), "teoretical:", sum(probobilytis * seq(0, 10, by = 1)), "\n")
  
  teoreticalMean <- sum(probobilytis * seq(0, 10, by = 1))
  
  cat("empirical variance:", var(x), 
      "teoretical:", sum((seq(0, 10, by = 1) - rep(teoreticalMean, 11)) ^ 2 * probobilytis), "\n")
  
  colums <- rep(0, 11)
  for(i in 1 : N){
    colums[x[i] + 1] <- colums[x[i] + 1] + 1 
  }
  colums <- colums / N
    
  chisqrVar17(x, probobilytis, colums)
  
  {
    plot(x = seq(0, 10, by = 1), y = probobilytis)
    lines(x = seq(0, 10, by = 1), y = probobilytis, lwd = 3, col = "red")
    lines(x = seq(0, 10, by = 1), y = colums, lwd = 3, col = "blue")
    grid(nx = 15, ny = 15, lty = 2, col = "gray", lwd = 1)
    legend(4, .25, c("red - teoretical", "blue - empirical"))
  }
}
