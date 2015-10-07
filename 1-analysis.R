library(fGarch)
library(magrittr)
library(ggplot2)
source("0-data.R")


res <- garchFit(~garch(1, 1), data = returns$continuous)
#plot(res) #look at some plots

qplot(returns %>% as.vector, geom = "histogram", xlim = c(-0.25,0.25), binwidth = 0.005)


## ---- Delta-Normal

delta_normal <- function(q=99.9E-2) {
  function(returns) {
    mu <- mean(returns)
    sigma <- sd(returns)
    qnorm(q, mu, sigma)
  }
}


## ---- Empirical

empirical <- function(q=99.9E-2) {
  function(returns) {
    -returns %>% quantile(q)
  } 
}


## ---- Model evaluation

evaluate_model <- function(model, lookback="1 month") {
  vars <- index(price) %>%
    .[-1] %>%
    lapply(function(date)price[paste0("/",date)]) %>%
    lapply(function(prices) {
      date <- prices %>% index %>% .[length(prices)] %>% as.Date
      returns <- prices %>% xts::last(lookback) %>% ROC %>% na.omit
      val <- model(returns)
      xts(val, date)
    }) %>%
    Reduce(rbind,.)
  vars
}

## ---- Work with models

delta_normal_var <- evaluate_model(delta_normal(),"1 year")

empirical_var <- evaluate_model(empirical())

delta_normal_var['1999/'] %>% chartSeries
empirical_var['1999/'] %>% chartSeries
