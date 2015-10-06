library(fGarch)
source("0-data.R")

res <- garchFit(~garch(1, 1), data = returns$continuous)
plot(res) #look at some plots