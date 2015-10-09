library(readxl)
library(dplyr)
library(magrittr)
library(quantmod)
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
price1 <- price['1996-08-02/2001-10-29']
   
price0 %>% chartSeries
price1 %>% chartSeries

returns0 <- returns['1996-08-02/1999-12-31']$discrete
returns1 <- returns['1996-08-02/2001-10-29']$discrete

-returns0 %>% chartSeries
-returns1 %>% plotXTS(ylim = c(-0.5,1))

price$Last %>% chartSeries
returns$discrete %>% chartSeries

message("Imported data --------------------------")