
dens <- function(x) cos(x)

distrib <- function(x) sin(x)

invert <- function(x) asin(x)

ybetta <- 1.96

sigment <- seq(0, 1, length = 10000)

z <- punif(sigment, min = 0, max = 1)

x <- invert(z)

{
  cat("empirical mean:", sum(x)/length(x), "\n")
  cat("mean using mean func:", mean(x), "\n")
  cat("mean from density func:", integrate((function(x) x * dens(x)), 0, pi / 2)$value, "\n")
  CounttedMean <- integrate((function(x) x * dens(x)), 0, pi / 2)$value
  
  cat("empirical varianse:", sum(x ^ 2) / (length(x) - 1) - sum(x) ^ 2 / (length(x) * (length(x) - 1)), "\n")
  cat("Varianse using var func:", var(x), "\n")
  cat("Variance from density function:", integrate((function(x) (x - CounttedMean) ^ 2 * dens(x)), 0, pi / 2)$value, "\n")
  CountedVar <- integrate((function(x) (x - CounttedMean) ^ 2 * dens(x)), 0, pi / 2)$value
  
  cat("confidence 95% interval: [", CounttedMean - ybetta * sqrt(CountedVar) / sqrt(length(x)), ",",
      CounttedMean + ybetta * sqrt(CountedVar) / sqrt(length(x)), "]", "real:", mean(x), "\n")
  
  {
    plot(x = density(x), xlab = "x", main = "Density", xlim = c(0, pi / 2), col = "blue")
    curve(dens, add = T, col = "red")
    grid(nx = 15, ny = 15, lty = 2, col = "gray", lwd = 1)     
    legend(0.7, 1, c("red - teoretical", "blue - empirical"))
  }

}
