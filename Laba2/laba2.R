
library('ggplot2')


task4_4 <- function(L){
  x <- rep(0, L)
  for(i in 1 : L) {
    x[i] <- mean(y[1 : i])
  }
  
  df <- data.frame(
    x <- x,
    y <- log10(1 : L)
  )
  
  ggplot() +
    geom_line(data = df, aes(x = x, y = log10(1 : L)), col = '#FF0000')
  #ggplot(data = data.frame(x = x, y = max(y[1:L]/2)), aes(x = x, y = max(y[1:L]/2)), col = '#FFFFFF') + 
  #geom_line() + geom_point()
}

task4_6_uniform <- function(vec){
  hist(vec, breaks = 10, probability = T)
  lines(x, dunif(x, min = 0, max = 1), col = "red", lwd = 5)
}

task4_8_uniform <- function(vec){
  uniformhist <- hist(vec, breaks = 10, plot = T, prob = TRUE)
  uniformhist$counts <- cumsum(uniformhist$counts) / length(vec)
  
  plot(uniformhist)
  lines(x, dunif(x, min = 0, max = 1), col = "red", lwd = 5)
}

task4_6_pois <- function(vec){
  hist(vec, breaks = 10, prob = TRUE)
  lines(density(vec, bw = 0.5), col = 4, lwd = 2)
}

x <- seq(0, 1, length = 7500)

uniform <- punif(x, min = 0, max = 1) 

poisDistr <- rpois(7500, 1.5) 

cat(y[3740 : 3756])
cat(poisDistr[1:16])

cat("min uniform:", min(uniform), " max unifirom:", max(uniform))
cat("min poisson:", min(poisDistr), " max poisson:", max(poisDistr))

cat("empirical uniform mean:", mean(uniform), "teoretical:", 1/2)
cat("empirical poisson mean:", mean(poisDistr), "teoretical:", 1.5)

task4_4(32)
task4_4(316)
task4_4(1000)
task4_4(3162)
task4_4(7500)

cat("empirical uniform sd:", sd(uniform), "teoretical:", 1/sqrt(12))
cat("empirical poisson sd:", sd(poisDistr), "teoretical:", sqrt(1.5))

task4_6_uniform(uniform)

task4_6_pois(poisDistr)

task4_8_uniform(uniform)
