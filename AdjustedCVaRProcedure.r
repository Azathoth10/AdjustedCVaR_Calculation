####Look INput for gbm function and alfa for confidence
####Output########
####Adjusted CVaR for one set of paths



CVaR_Function <- function(rets, sigmas, cdcorr, s0,alfa, weights, nperiods){
  
  ###Running GBM for paths
  
  paths<-data.frame(t(GBMf(s0, nperiods, rets, sigmas, cdcorr)))
  
  ###Returns from prices
  
  data1 <- data.frame(c(2:nrow(paths)-1))
  for(i in colnames(paths)[1:length(colnames(paths))]){
    v = as.vector(unlist(paths[i]))
    vr = diff(v)/(v[-length(v)])
    data1[i] <- vr
  }
  
  data1 <- data1[-1]
  
  ###Equally weighted portfolio ########################################To be changed in future to add the option to choose
  
  #weightseq <- matrix(1/(length(data1)), length(data1)) 
  
  finalrets <- c()
  
  for( i  in seq(from =1, to = nrow(data1), by = 1)){
    
    pr <- as.matrix(data1[i,])%*%weights
    finalrets <- c(finalrets, pr)
    
  }
  
  ###Returns sorted from smallest to largest and the first 5% are chosen
  ###weighted average of the first values
  ###the VaR value has the largest weight until the smallest return with the lowest weight
  
  sorted_returns <- sort(finalrets, decreasing = FALSE)
  n_quant <- round(length(sorted_returns)*alfa,0)
  CVaR_Interval <- sorted_returns[1:n_quant]
  CVaR_weights <- c(1:length(CVaR_Interval))
  Adjusted_CVaR <- weighted.mean(CVaR_Interval, CVaR_weights)
  
  Adjusted_CVaR
}
