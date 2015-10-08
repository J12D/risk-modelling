library(fGarch)
library(magrittr)
library(ggplot2)
source("0-data.R")


## ---- GARCH

garch <- function(q=99.9E-2) {
  function(prices) {
    returns <- prices %>% ROC(type = "discrete") %>% na.omit
    res <- capture.output(garchFit(~garch(1,1), data = returns), file='/dev/null')
    forecast <- predict(res,1)
    mu <- forecast$meanForecast %>% as.numeric
    sigma <- forecast$standardDeviation %>% as.numeric
    print(mu)
    print(sigma)
    print(qnorm(1 - q, mu, sigma))
    -(prices[length(prices)]) * qnorm(1 - q, mu, sigma)
  }
}

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
    price_today <- prices[length(prices)]
    (-price_today * returns) %>% quantile(q)
  }
}


## ---- Model evaluation

evaluate_model <- function(model, lookback="1 year", skip = 200) {
  vars <- index(price) %>%
    .[-(1:skip)] %>%
    lapply(function(date)price[paste0("/",date)]) %>%
    lapply(function(prices) {
      date <- prices %>% index %>% .[length(prices)] %>% as.Date
      prices <- prices %>% xts::last(lookback)
      val <- model(prices)
      xts(val, date)
    }) %>%
    Reduce(rbind,.)
  vars
}

## ---- Work with models

delta_normal_var <- evaluate_model(delta_normal(99E-2),"3 years")

delta_normal_var %>% chartSeries
(delta_normal_var/price) %>% chartSeries


empirical_var <- evaluate_model(empirical(99E-2))

empirical_var %>% plotXTS
(empirical_var/price) %>% plotXTS


evt_var <- evaluate_model(evt(99E-2), "4 years", 800)

evt_var %>% plotXTS
(evt_var/price) %>% plotXTS

garch_var <- evaluate_model(garch(99E-2), "4 years", 800)


garch_var %>% plotXTS
(garch_var/price) %>% plotXTS


vars <- merge.xts((evt_var/price), (empirical_var/price), (delta_normal_var/price))
colnames(vars) <- c("EVT", "Historical", "DeltaNormal")
vars %>% plotXTS(size = 1)
vars %>% plotTable("plot-data/vars")