library(evir)
library(ismev)

source("0-data.R")

SwissAirReturns <- returns
SwissAirReturns <- coredata(returns[, 1])
attr(SwissAirReturns, "times") = as.POSIXct(index(returns))

##Losses
SwissAirLoss <- -100.0 * SwissAirReturns

##packageevir:
SwissAirGEV <- gev(SwissAirLoss, block = "month")
SwissAirGEV
plot(
 SwissAirGEV$data,type = "h",col = "blue",xlab = "",
  ylab = "BlockMaxima",
  main = "MaximumMonthlyLossesofSwissAir"
)


d <- data.frame(data = SwissAirGEV$data, i = 1:139)

ggplot(data = d, aes(x = i, y = data)) +
  geom_bar(stat = "identity", width = 0.3)

##packageismev:
library(ismev)
SwissAirGEV2 <- gev.fit(SwissAirGEV$data)
SwissAirGEV2
gev.diag(SwissAirGEV2)
par(mfrow = c(2,1))
gev.prof(
 SwissAirGEV2,m = 20,xlow = 5,xup = 16,conf = 0.95
)
gev.profxi(SwissAirGEV2,xlow = 0.0,xup = 0.7,conf = 0.95)
mLoss <- max(SwissAirGEV$data)
mYears <-  1 / (1 - pgev(mLoss, mu =SwissAirGEV2$mle[1], sigma =SwissAirGEV2$mle[2], xi = SwissAirGEV2$mle[3])) / 2
##packagefExtremes:
library(fExtremes)
SwissAirGEV3 <- gevFit(SwissAirGEV$data,type = "pwm")
SwissAirGEV3

##################################

SwissAirLoss <- -1.0 * SwissAirReturns * 100                                                                                
Years <- format(attr(SwissAirLoss, "time"), "%Y")                                                   
attr(SwissAirLoss, "years") <- Years                                                                             
Yearu <- unique(Years)                                                                                              
idx <- 1:length(Yearu)                                                                                                
r <- 2                                                                                                                           
SwissAirOrder <- t(sapply(idx, function(x)                                                                   
  head(sort(SwissAirLoss[attr(SwissAirLoss, "years") ==                        
                           Yearu[x]], decreasing = TRUE), r)))                
rownames(SwissAirOrder) <- Yearu                                                                               
colnames(SwissAirOrder) <- paste("r", 1:r, sep = "")                                                
## Plot of order data                                                                                                     
plot(Yearu, SwissAirOrder[, 1], col = "black", ylim = range(SwissAirOrder),                   
     ylab = "Losses Swissair (percentages)", xlab = "",                                          
     pch = 21, bg = "black")                                                                                    
points(Yearu, SwissAirOrder[, 2], col = "grey", pch = 23, bg = "grey")                      
## Fit and diagnostics                                                                                                 
SwissAirOrderFit <- rlarg.fit(SwissAirOrder)                                                                        
rlarg.diag(SwissAirOrderFit)     
