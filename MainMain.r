source("gbm.r")
source("impstocks.r")

TKs <- c("^GSPC", "EEM", "VGK", "SPMO", "IJR", "AIA")
start <- "2021-05-03"

df <- GetStocks(TKs, start)

source("stock.r")
source("AdjustedCVaRProcedure.r")
source("AdjCVaRF.r")
source("randomPortfolios.r")


library(ggplot2)

alfa <- 0.05
nsim_CVaR <- 10

rw <- RWgenerators(10, length(TKs))

ESWeights <- c()
retsweights <- c()
Sharpe <- c()


for(i in c(1:nrow(rw))){
  
  
  ES <- Final_AdjCVaR_Function(rets, sigmas, cdcorr, s0, alfa, rw[i,], nsim_CVaR)
  ESWeights <- c(ESWeights, ES)
  
  rs <- (sum(rets * rw[i,]))*100
  retsweights <- c(retsweights, rs)
  
  sh <- rs-ES
  Sharpe <- c(Sharpe, sh)
  
  
  print(length(ESWeights))
  
}


m <- min(ESWeights)
pos_min <- which(ESWeights == m)

MIN_CVaR_Portfolio <- rw[pos_min, ]


ma <- max(Sharpe)
pos_max <- which(Sharpe == ma)

MAX_SH_Portfolio <- rw[pos_max, ]

plot(ESWeights, retsweights)
print(MIN_CVaR_Portfolio)
print(MAX_SH_Portfolio)

