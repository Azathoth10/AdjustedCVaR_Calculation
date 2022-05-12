Final_AdjCVaR_Function <- function(rets, sigmas, cdcorr, s0, alfa, weights, nsim_CVaR, nperiods){
  Adj_CVaR <- c()
  
  for(i in c(1:nsim_CVaR)){
    
    
    Adjusted_CVaR <- CVaR_Function(rets, sigmas, cdcorr, s0, alfa, weights, nperiods)
    Adj_CVaR <- c(Adj_CVaR, Adjusted_CVaR)
    
  }
  
  Final_AdjCVaR <- -mean(Adj_CVaR)*100
  
  #######Final Results
  
  Final_AdjCVaR
  
  
}