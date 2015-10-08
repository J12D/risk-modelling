library(fGarch)
library(magrittr)
library(ggplot2)
source("0-data.R")


res <- garchFit(~garch(1, 1), data = returns$continuous)
#plot(res) #look at some plots

qplot(returns %>% as.vector, geom = "histogram", xlim = c(-0.25,0.25), binwidth = 0.005)


## ---- Delta-Normal

delta_normal <- function(q=99.9E-2) {
  function(prices) {
    returns <- prices %>% ROC(type = "discrete") %>% na.omit
    mu <- returns %>% mean
    sigma <- returns %>% sd
    -(prices[length(prices)]) * qnorm(1 - q, mu, sigma)
  }
}


## ---- Empirical

empirical <- function(q=99.9E-2) {
  function(prices) {
    returns <- prices %>% ROC(type = "discrete") %>% na.omit
    price_today <- prices[length(prices)] %>% as.numeric
    val_changes <- price_today * returns
    -(stats::quantile(val_changes, 1 - q))
  }
}


## ---- Extreme Value Theory

evt <- function(q=99.9E-2) {
  function(prices) {
    returns <- prices %>% ROC(type = "discrete") %>% na.omit
    n <- round(length(prices) * (1 - (q - 1E-2)), 0)
    t <- findthresh(returns, n)
    GPD <- gpd(returns, threshold = t, method = c("ml"), information = c("observed"))
    (prices[length(prices)]) * riskmeasures(GPD, q)[,"quantile"]
  }
}

## ---- Model evaluation

evaluate_model <- function(model, lookback="4 quarters", skip=200) {
  vars <- index(price)[-(1:skip)] %>%
    lapply(function(date)price[paste0("/",date)]) %>%
    lapply(function(prices) {
      date <- index(prices)[length(prices)] %>% as.Date
      prices <- prices %>% xts::last(lookback)
      val <- model(prices)
      xts(val, date)
    }) %>%
    Reduce(rbind,.)
  vars
}

## ---- Work with models

delta_normal_var <- evaluate_model(delta_normal(99E-2))

delta_normal_var %>% plotXTS
(delta_normal_var/price) %>% plotXTS


empirical_var <- evaluate_model(empirical(99E-2))

empirical_var %>% plotXTS
(empirical_var/price) %>% plotXTS


evt_var <- evaluate_model(evt(99E-2),"5 years",800)

evt_var %>% plotXTS
(evt_var/price) %>% plotXTS



merge.xts((evt_var/price), (empirical_var/price), (delta_normal_var/price)) %>% plotXTS
