library(readxl)
library(dplyr)
library(magrittr)
library(quantmod)
library(lubridate)
source("0-helper.R")
Sys.setenv(tz = "UTC")


price_ <- read_excel("SwissAirAktie.xls", skip = 1, na = "#N/A N/A",col_types = c("date","numeric","numeric","numeric")) %>% as.data.frame
price_ <- xts(price_[,-1], as.Date(price_[,1]))
colnames(price_) <- c("Last", "Last_Actual", "Mid")
price <- price_$Last

returns <- xts(NULL)
returns$continuous <- ROC(price)
returns$discrete <- ROC(price, type = "discrete")
returns %<>% na.omit

price0 <- price['1996-08-02/1999-12-31']
price1 <- price['1996-08-02/2001-08-31']
   
price0 %>% chartSeries
price1 %>% chartSeries

returns0 <- returns['1996-08-02/1999-12-31']$discrete
returns1 <- returns['1996-08-02/2001-08-31']$discrete

-returns0 %>% chartSeries
-returns1 %>% plotXTS(ylim = c(-0.5,1))

price$Last %>% chartSeries
returns$discrete %>% chartSeries

read_kunal_data <- function(file, format, skip=0) {
  d <- read_excel(file, skip = skip)
  d <- xts(d[,3], d[,2][[1]] %>% parse_date_time(format))
}

# ---- Airline Index ----------------------------------------
#xal <- getSymbols("^XAL", auto.assign = F, from = "1990-01-01")# read.csv("data/XAL Data.csv", sep = ";", na.strings = "--")
#xal <- xal[,"XAL.Adjusted"] #saveRDS(xal, "data/xal")
xal <- readRDS("data/xal")

price0_xal <- xal['1996-08-02/1999-12-31']
price1_xal <- xal['1996-08-02/2001-08-31']

returns_xal <- xts(NULL)
returns_xal$continuous <- ROC(xal)
returns_xal$discrete <- ROC(xal, type = "discrete")
returns_xal %<>% na.omit

## ---- Lehman ---------------------------------------
lehman <- read_excel("data/Lehman-Updated.xlsx") %>% as.data.frame
lehman <- xts(lehman[,6], lehman[,2] %>% parse_date_time("%Y/%m/%d"))

## ---- United ------------------------------------------
united <- read_excel("data/United Airlines-Updated.xlsx") %>% as.data.frame
united <- xts(united[,6], united[,2] %>% parse_date_time("%Y/%m/%d"))

price0_united <- united['1996-08-02/1999-12-31']
price1_united <- united['1996-08-02/2001-08-31']

message("Imported data --------------------------")