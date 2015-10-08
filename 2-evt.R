library(evir)
library(ismev)
library(fExtremes)

blockMaxima <- function(price) {
  #Get arithmetic returns
  returns <- price %>% ROC(type = "discrete") %>% na.omit
  
  #Transform data in POSIXct data format
  SwissAirReturns <- returns
  SwissAirReturns <- coredata(returns[,1])
  attr(SwissAirReturns,"times") = as.POSIXct(index(returns))
  
  #Calculation of losses
  SwissAirLoss <- -1.0 * SwissAirReturns * 100.0
  
  #Plot block maxima
  SwissAirGEV <- gev(SwissAirLoss,block = "month")
  plot(
    SwissAirGEV$data,type = "h",col = "blue",xlab = "",ylab = "BlockMaxima",main =
      "Maximum monthly losses"
  )
  
  #Plot diagnostic plots for fitted GEV model
  SwissAirGEV2 <- gev.fit(SwissAirGEV$data)
  gev.diag(SwissAirGEV2)
  par(mfrow = c(2,1))
  gev.prof(
    SwissAirGEV2,m = 20,xlow = 5,xup = 16,conf = 0.95
  )
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
  plot(
    Yearu,SwissAirOrder[,1],col = "black",ylim = range(SwissAirOrder),ylab =
      "LossesSwissair(percentages)",xlab = "",
    pch = 21,bg = "black"
  )
  points(Yearu,SwissAirOrder[,2],col = "grey",pch = 23,bg = "grey")
  
  #Fit and diagnostics
  SwissAirOrderFit <- rlarg.fit(SwissAirOrder)
  rlarg.diag(SwissAirOrderFit)
}
