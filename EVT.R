library(fExtremes)
library(evir)

### Threshold Exceedance
### plot sample mean excess   ## find proper threshold
meplot(returns, labels=TRUE) 
findthresh(returns, 57)


GPD <- gpd(returns, threshold = findthresh(returns, 57), method = c("ml"), information=c("observed"))

#calculate VaR and ES
tp <- tailplot(GPD)
gpd.q(tp, 0.999)
gpd.sfall(tp, 0.999)


#another way to calculate VaR and ES
riskmeasures(GPD, 0.999)

##plot  #Select 2 to get threshold, xi, scale, location
plot(GPD)  




### BlockMaxima
GEV <- gev(returns, block= 20)
    
## Return level  
rlevel.gev(GEV, k.blocks=20)
rlevel.gev(GEV, k.blocks=60)
rlevel.gev(GEV, k.blocks=250)
plot(GEV)
