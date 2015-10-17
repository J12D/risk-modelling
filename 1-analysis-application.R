source("1-analysis.R")

## ---- General Analysis ---------------
par(ask = F)
price %>% chartSeries
returns['/2001-10-29']$discrete %>% plotXTS

qplot(returns %>% as.vector, geom = "histogram", xlim = c(-0.25,0.25), binwidth = 0.005)

## ---- Use functions ---------

price %>% eval_price %>% write_vars("plot-data/vars")
price %>% plotTable("plot-data/price")

lehman %>% eval_price %>% plotXTS #write_vars("plot-data/leh_vars")
lehman %>% plotTable("plot-data/lehman")

united %>% eval_price %>% write_vars("plot-data/united_vars")
united %>% plotTable("plot-data/united")