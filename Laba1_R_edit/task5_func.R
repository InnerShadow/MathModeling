
library('ggplot2')

task5 <- function(step){
  x <- seq(-5, 5, by = step)
  ggplot(data = data.frame(x = x, y = task4(x)), aes(x = x, y = task4(x))) +
    geom_point() + geom_line() + 
    labs(title = "function graph", x = "x", y = "f(x)") + 
    geom_text(aes(x = 0, y = 0), label = ("f(x) = -0.5x^3 - 10x^2 - 10"))
}
