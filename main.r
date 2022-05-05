source("gbm.r")
source("stock.r")

paths<-GBMf(s0, 250, rets, sigmas, cdcorr)

plot(as.numeric(paths[1,]))