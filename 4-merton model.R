##--------Swissair Equity----------------------------------------
SA_BS <- read_excel("data/Equity-Swissair.xlsx") %>% as.data.frame
SAequity <- xts(SA_BS[,4], SAequity[,1] %>% parse_date_time("%Y/%m/%d"))
SAdebt <- xts(SA_BS[,5], SAequity[,1] %>% parse_date_time("%Y/%m/%d"))
SAasset<- xts(SA_BS[,6], SAequity[,1] %>% parse_date_time("%Y/%m/%d"))

##--------Set risk_free rate and default period------------------
t <- 1
r <- 0.05

##--------Import volatilities from GARCH model-------------------
vol_garch <- 


##--------get the probability------------------------------------
d2 <- log(SAasset/SAequity) + r*t - (vol_garch)^2*t
PD <- 1- qnorm(d2)


