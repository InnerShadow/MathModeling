
dens <- function(x) cos(x)

distrib <- function(x) sin(x)

invert <- function(x) asin(x)

ybetta <- 1.96

z <- runif(10000, min = 0, max = 1)

x <- invert(z)

cat("empirical mean:", sum(x)/length(x))
cat("mean func mean:", mean(x))
cat("mean from density func:", integrate((function(x) x * dens(x)), 0, pi / 2)$value)
CounttedMean <- integrate((function(x) x * dens(x)), 0, pi / 2)$value

cat("Varianse mean:", sum(x ^ 2) / (length(x) - 1) - sum(x) ^ 2 / (length(x) * (length(x) - 1)))
cat("Varianse usin var func:", var(x))
cat("Variance from density function:", integrate((function(x) (x - CounttedMean) ^ 2 * dens(x)), 0, pi / 2)$value)
CountedVar <- integrate((function(x) (x - CounttedMean) ^ 2 * dens(x)), 0, pi / 2)$value

cat("confidence 95% interval: [", CounttedMean - ybetta * sqrt(CountedVar) / sqrt(length(x)), ",",
    CounttedMean + ybetta * sqrt(CountedVar) / sqrt(length(x)), "]", "real:", mean(x))

plot(x = density(x), xlab = "x", main = "Density", xlim = c(0, pi / 2), col = "blue")
curve(dens, add = T, col = "red")
legend(0.7, 1, c("red - teoretical", "blue - empirical"))
