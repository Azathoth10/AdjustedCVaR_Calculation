source("gbm.r")
source("stock.r")
source("AdjustedCVaRProcedure.r")

Adj_CVaR <- c()
alfa <- 0.05
weights <- c(0,0,1,0,0)

nsim <- 10


for(i in c(1:nsim)){
  
  Adjusted_CVaR <- CVaR_Function(rets, sigmas, cdcorr, s0, alfa, weights)
  Adj_CVaR <- c(Adj_CVaR, Adjusted_CVaR)
  
}

Final_AdjCVaR <- -mean(Adj_CVaR)*100

######Final Results

Final_AdjCVaR
