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
           
price$Last %>% chartSeries
returns$discrete %>% chartSeries

message("Imported data --------------------------")