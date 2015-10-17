library(fGarch)
library(magrittr)
library(ggplot2)
library(evir)
library(ismev)
library(fExtremes)

source("0-data.R")

## ---- Get volatilities from GARCH model ----
garch_vol_yearly <- function(q=99.9E-2) {
  function(prices) {
    returns <- prices %>% ROC(type = "discrete") %>% na.omit
    res <- garchFit(~garch(1,1), data = returns, trace = F)
    forecast <- predict(res, 252)
    sigma <- forecast$standardDeviation ^ 2 %>% sum %>% sqrt
    # forecast <- predice(res, 1)
    # sigma <- forecast$standardDeviation
    sigma
  }
}

garch_vol_daily <- function(q=99.9E-2) {
  function(prices) {
    returns <- prices %>% ROC(type = "discrete") %>% na.omit
    res <- garchFit(~garch(1,1), data = returns, trace = F)
    forecast <- predict(res, 1)
    sigma <- forecast$standardDeviation
    sigma
  }
}



## ---- GARCH ---------------

garch <- function(q=99.9E-2) {
  function(prices) {
    returns <- prices %>% ROC(type = "discrete") %>% na.omit
    res <- garchFit(~garch(1,1), data = returns, trace = F)
    forecast <- predict(res,1)
    mu <- forecast$meanForecast %>% as.numeric
    sigma <- forecast$standardDeviation %>% as.numeric
    f <- -(prices[length(prices)]) * qnorm(1 - q, mu, sigma)
    colnames(f) <- c("garch")
    f
  }
}

## ---- Delta-Normal ----------------

delta_normal <- function(q=99.9E-2) {
  function(prices) {
    returns <- prices %>% ROC(type = "discrete") %>% na.omit
    mu <- returns %>% mean
    sigma <- returns %>% sd
    f <- -(prices[length(prices)]) * qnorm(1 - q, mu, sigma)
    colnames(f) <- c("delta-normal")
    f
  }
}


## ---- Empirical ---------------

empirical <- function(q=99.9E-2) {
  function(prices) {
    returns <- prices %>% ROC(type = "discrete") %>% na.omit
    price_today <- prices[length(prices)] %>% as.numeric
    f <- quantile(-price_today * returns, q)
    f <- xts(f, index(prices) %>% last)
    colnames(f) <- c("empricial")
    f
  }
}


## ---- Extreme Value Theory

evt <- function(q=99.9E-2) {
  function(prices) {
    losses <- prices %>% ROC(type = "discrete") %>% na.omit %>% -.
    n <- round(length(prices) * (1 - (q - 1E-2)), 0)
    t <- findthresh(losses, n)
    GPD <- gpd(losses, threshold = t, method = c("ml"), information = c("observed"))
    f <- (prices[length(prices)]) * riskmeasures(GPD, q)[,"quantile"]
    colnames(f) <- c("evt")
    f
  }
}

## ---- EWMA --------------------

ewma <- function(q=99.9E-2, lambda=0.94) {
  function(prices) {
    returns <- prices %>% ROC %>% na.omit
    price_today <- prices[length(prices)]
    weights <- (1 - lambda) * lambda ^ ((length(returns) - 1):0)
    mu <- returns %>% mean
    sigma <- sum(weights * (returns ^ 2))
    f <- -price_today * qnorm(1 - q, mu, sigma)
    colnames(f) <- c("ewma")
    f
  }
}

## ---- Model evaluation ---------------

evaluate_model <- function(price, model, lookback = "1 year", skip = 200) {
  vars <- index(price) %>%
    .[-(1:skip)] %>%
    lapply(function(date) price[paste0("/",date)]) %>%
    lapply(function(prices) {
      date <- prices %>% index %>% .[length(prices)] %>% as.Date
      prices <- prices %>% xts::last(lookback)
      val <- model(prices)
      xts(val, date)
    }) %>%
    Reduce(rbind,.)
  vars
}


## ---- Work with models ---------------

eval_price <- function(price, funs = list(delta_normal, empirical, evt, garch, ewma)) {
  results <- funs %>% lapply(function(model)evaluate_model(price, model(99E-2), "4 years", 800)/price)
  results %<>% Reduce(merge.xts, .)
  results
}

write_vars <- function(vars, filename) {
  colnames(vars) <- c("DeltaNormal", "Historical", "EVT", "GARCH", "EWMA")
  vars %>% plotXTS(size = 1)
  vars %>% plotTable(filename)
}