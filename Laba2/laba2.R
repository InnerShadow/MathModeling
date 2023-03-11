
library('ggplot2')

distribution <- function(x) x

task4_4 <- function(vec){
  y <- c(mean(vec[1:32]), mean(vec[1:316]), mean(vec[1:1000]), mean(vec[1:3162]), mean(vec[1:length(x)]))
  x <- c(log10(32), log10(316), log10(1000), log10(3162), log10(length(vec)))
  
  df <- data.frame(
    x = x,
    y = y
  )
  
  ggplot() +
    geom_line(data = df, aes(x = x, y = y), col = '#FF0000') +
    labs(x = "log10(L)", y = "mean") 
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
  curve(distribution, add = T, col = "red")
  #lines(x, dunif(x, min = 0, max = 1), col = "red", lwd = 5)
}

task4_6_pois <- function(vec){
  hist(vec, breaks = 10, prob = TRUE)
  lines(density(vec, bw = 0.5), col = 4, lwd = 2)
}

{
  x <- seq(0, 1, length = 7500)
  
  uniform <- punif(x, min = 0, max = 1) 
  
  poisDistr <- rpois(7500, 1.5) 
  
  cat(uniform[3740 : 3756], "\n")
  cat(poisDistr[1:16], "\n")
  
  cat("min uniform:", min(uniform), " max unifirom:", max(uniform), "\n")
  cat("min poisson:", min(poisDistr), " max poisson:", max(poisDistr), "\n")
  
  cat("empirical uniform mean:", mean(uniform), "teoretical:", 1/2, "\n")
  cat("empirical poisson mean:", mean(poisDistr), "teoretical:", 1.5, "\n")
  
  task4_4(uniform)
  
  cat("empirical uniform sd:", sd(uniform), "teoretical:", 1/sqrt(12), "\n")
  cat("empirical poisson sd:", sd(poisDistr), "teoretical:", sqrt(1.5), "\n")
}
  
task4_6_uniform(uniform)

task4_6_pois(poisDistr)

task4_8_uniform(uniform)

