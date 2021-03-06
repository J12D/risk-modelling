source("1-analysis.R")

##--------Swissair Equity----------------------------------------
SA_BS <- read_excel("data/Equity-Swissair.xlsx", na = "#N/A N/A")
SA_BS <- xts(SA_BS[,2:6], SA_BS[,1][[1]] %>% as.Date)
colnames(SA_BS) <- c("price", "shares", "equity", "debt", "assets")
##--------Set risk_free rate and default period------------------
t <- 1
r <- 0.05

vol_garch <- evaluate_model(SA_BS$price, garch_vol(99E-2), "4 quarters", 200)
vol_garch %>% plotXTS

aggr <- merge.xts(SA_BS[,c("assets", "debt")], vol_garch) %>% na.omit

## ---- get the probability ----------------
d2 <- (log(aggr$assets / aggr$debt) + r * t - (aggr$vol_garch) ^ 2 * t * 0.5)/(aggr$vol_garch * sqrt(t))
PD <- 1 - pnorm(d2)

PD %>% plotTable("plot-data/pd")