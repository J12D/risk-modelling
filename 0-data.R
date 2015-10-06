library(readxl)
library(dplyr)
library(magrittr)
library(quantmod)
source("0-helper.R")
Sys.setenv(tz = "UTC")


price <- read_excel("SwissAirAktie.xls", skip = 1, na = "#N/A N/A",col_types = c("date","numeric","numeric","numeric")) %>% as.data.frame
price <- xts(price[,-1], as.Date(price[,1]))
colnames(price) <- c("Last", "Last_Actual", "Mid")

returns <- price %>%
           xts2df %>%
           transmute(time = .[,"time"],
                     continuous = ROC(.[,"Last"]),
                     discrete = ROC(.[,"Last"], type = "discrete")) %>%
           na.omit %>%
           df2xts
           
price$Last %>% chartSeries
returns$discrete %>% chartSeries

message("Imported data --------------------------")