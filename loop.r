source("gbm.r")
source("stock.r")

Adj_CVaR <- c()

nsim <- 1000

for(i in c(1:nsim)){
  
  paths<-data.frame(t(GBMf(s0, 250, rets, sigmas, cdcorr)))
  
  
  data1 <- data.frame(c(2:nrow(paths)-1))
  for(i in colnames(paths)[1:length(colnames(paths))]){
    v = as.vector(unlist(paths[i]))
    vr = diff(v)/(v[-length(v)])
    data1[i] <- vr
  }
  
  data1 <- data1[-1]
  
  weightseq <- matrix(1/(length(data1)), length(data1)) 
  
  finalrets <- c()
  
  for( i  in seq(from =1, to = nrow(data1), by = 1)){
    
    pr <- as.matrix(data1[i,])%*%weightseq
    finalrets <- c(finalrets, pr)
    
  }
  
  sorted_returns <- sort(finalrets, decreasing = FALSE)
  n_quant <- round(length(sorted_returns)*0.05,0)
  CVaR_Interval <- sorted_returns[1:n_quant]
  CVaR_weights <- c(1:length(CVaR_Interval))
  Adjusted_CVaR <- weighted.mean(CVaR_Interval, CVaR_weights)
  
  Adj_CVaR <- c(Adj_CVaR, Adjusted_CVaR)
  
}

Final_AdjCVaR <- -mean(Adj_CVaR)*100

Final_AdjCVaR