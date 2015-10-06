library(fGarch)
library(magrittr)
source("0-data.R")

res <- garchFit(~garch(1, 1), data = returns$continuous)
plot(res) #look at some plots

qplot(returns %>% as.vector, geom = "histogram", xlim = c(-0.25,0.25), binwidth = 0.005)


delta_normal <- function(returns) {
  mu <- mean(returns)
  sigma <- sd(returns)
  qnorm(99.9E-2, mu, sigma)
}

index(price) %>% sapply(function(date)price[paste0("/",date)]) %>% head
