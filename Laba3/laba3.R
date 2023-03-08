
dens <- function(x) cos(x)

distrib <- function(x) sin(x)

invert <- function(x) asin(x)

ybetta <- 1.96

sigment <- seq(0, 1, length = 10000)

z <- punif(sigment, min = 0, max = 1)

x <- invert(z)

cat("empirical mean:", sum(x)/length(x))
cat("mean using mean func:", mean(x))
cat("mean from density func:", integrate((function(x) x * dens(x)), 0, pi / 2)$value)
CounttedMean <- integrate((function(x) x * dens(x)), 0, pi / 2)$value

cat("empirical varianse:", sum(x ^ 2) / (length(x) - 1) - sum(x) ^ 2 / (length(x) * (length(x) - 1)))
cat("Varianse using var func:", var(x))
cat("Variance from density function:", integrate((function(x) (x - CounttedMean) ^ 2 * dens(x)), 0, pi / 2)$value)
CountedVar <- integrate((function(x) (x - CounttedMean) ^ 2 * dens(x)), 0, pi / 2)$value

cat("confidence 95% interval: [", CounttedMean - ybetta * sqrt(CountedVar) / sqrt(length(x)), ",",
    CounttedMean + ybetta * sqrt(CountedVar) / sqrt(length(x)), "]", "real:", mean(x))

plot(x = density(x), xlab = "x", main = "Density", xlim = c(0, pi / 2), col = "blue")
curve(dens, add = T, col = "red")
grid(nx = 15, ny = 15, lty = 2, col = "gray", lwd = 1)     
legend(0.7, 1, c("red - teoretical", "blue - empirical"))
