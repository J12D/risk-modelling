library(evir)
library(ismev)
library(fExtremes)

source("1-analysis.R")

blockMaxima <- function(price, filename = "") {
  #Get arithmetic returns
  returns <- price %>% ROC(type = "discrete") %>% na.omit
  
  
  #Transform data in POSIXct data format
  SwissAirReturns <- returns
  SwissAirReturns <- coredata(returns[,1])
  attr(SwissAirReturns,"times") = as.POSIXct(index(returns))

  #Calculation of losses
  SwissAirLoss <- -1.0 * SwissAirReturns
  
  #Plot block maxima
  SwissAirGEV <- gev(SwissAirLoss,block = 21)
  png(
    paste0(filename,"max_monthly.png"),
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200
  )
  plot(
    SwissAirGEV$data,type = "h",col = "blue",xlab = "Months",ylab = "Block Maxima",main =
      "Maximum monthly losses"
  )
  dev.off()
  
  #Plot diagnostic plots for fitted GEV model
  SwissAirGEV2 <- gev.fit(SwissAirGEV$data)
  gev.diag(SwissAirGEV2)
  
  png(
    paste0(filename,"gev_4.png"),
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200
  )
  gev.prof(
    SwissAirGEV2,m = 20,xlow = 5,xup = 16,conf = 0.95
  )
  dev.off()
  
  gev.profxi(SwissAirGEV2,xlow = 0.0,xup = 0.7,conf = 0.95)
  mLoss <- max(SwissAirGEV$data)
  mYears <-
    1 / (
      1 - pgev(
        mLoss,mu = SwissAirGEV2$mle[1],sigma = SwissAirGEV2$mle[2],xi = SwissAirGEV2$mle[3]
      )
    ) / 2
  
  #Profile log-Likelihood plots for fitted GEV model
  SwissAirGEV3 <- gevFit(SwissAirGEV$data,type = "pwm")
  
  Years <- format(attr(SwissAirLoss,"time"),"%Y")
  attr(SwissAirLoss,"years") <- Years
  Yearu <- unique(Years)
  idx <- 1:length(Yearu)
  r <- 2
  SwissAirOrder <- t(sapply(idx,function(x)
    head(
      sort(SwissAirLoss[attr(SwissAirLoss,"years") ==
                          Yearu[x]],decreasing = TRUE),r
    )))
  rownames(SwissAirOrder) <- Yearu
  colnames(SwissAirOrder) <- paste("r",1:r,sep = "")
  
  #Plot of order data
  png(
    paste0(filename,"_order.png"),
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200
  )
  plot(
    Yearu,SwissAirOrder[,1],col = "black",ylim = range(SwissAirOrder),ylab =
      "LossesSwissair(percentages)",xlab = "",
    pch = 21,bg = "black"
  )
  
  points(Yearu,SwissAirOrder[,2],col = "grey",pch = 23,bg = "grey")
  dev.off()
  
  #Fit and diagnostics
  SwissAirOrderFit <- rlarg.fit(SwissAirOrder)
  rlarg.diag(SwissAirOrderFit)
}
# Swissair
print("1996-08-02/1999-12-31")
blockMaxima(price0,"graphs/0")
print("1996-08-02/2001-08-31")
blockMaxima(price1,"graphs/1")

# Airline Index
print("1996-08-02/1999-12-31")
blockMaxima(price0_xal, "graphs/xal0")
print("1996-08-02/2001-08-31")
blockMaxima(price1_xal,"graphs/xal1")

# United Airlines
print("1996-08-02/1999-12-31")
blockMaxima(price0_united, "graphs/ual0")
print("1996-08-02/2001-08-31")
blockMaxima(price1_united, "graphs/ual1")


## GARCH + EVT

library(evir)
library(fExtremes)

## ---- input:vol_garch ----
vol_garch_daily <- evaluate_model(price,garch_vol_daily(99E-2), "4 quarters", 200)

## ---- innovations ------------
aggr <- merge.xts(vol_garch_daily, returns) %>% na.omit
Z <- aggr$continuous/aggr$vol_garch_daily

## ---- Fit innovations into GPD----
q <-  0.99

GPD_Z <- gpd(Z, threshold = findthresh(Z, round(length(Z) * (1 - (q - 1E-2)), 0)), 
             method = c("ml"), information = c("observed"))

quantile <- riskmeasures(GPD_Z, q)[,"quantile"] %>% as.numeric