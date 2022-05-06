source("gbm.r")
source("stock.r")
source("AdjustedCVaRProcedure.r")
source("AdjCVaRF.r")
source("randomPortfolios.r")

alfa <- 0.05
nsim_CVaR <- 10

rw <- RWgenerators(10, 5)

ESWeights <- c()
retsweights <- c()

for(i in c(1:nrow(rw))){
  
  ES <- Final_AdjCVaR_Function(rets, sigmas, cdcorr, s0, alfa, rw[i,], nsim_CVaR)
  ESWeights <- c(ESWeights, ES)
  
  rs <- (sum(rets * rw[i,]))*100
  retsweights <- c(retsweights, rs)
  
}

m <- min(ESWeights)
pos_min <- which(ESWeights == m)

MIN_CVaR_Portfolio <- rw[pos_min, ]

plot(ESWeights, retsweights)
print(MIN_CVaR_Portfolio)

