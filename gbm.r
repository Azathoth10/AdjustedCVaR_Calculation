#######GBM function 
#Out###n number of paths for number of assets
#In####
#s0####Initial stock prices
#t#####Time steps number
#m#####mean
#sig###standard deviations
#ch####Lower triangular cholesky times correlation matrix
#dt####time intervals

#tickers to choose as inputs for future changes ################################


GBMf <- function(s0, t, m, sig, ch, dt = 1./365){
  
  normvars <- matrix(0, nrow(s0), t)
  for(i in seq(from = 1, to = t, by = 1)){
    r <- rnorm(nrow(s0))
    normvars[,i] <- r
  }
  
  path <- cbind(s0, matrix(0, nrow(s0), t))
  
  
  
  for(i in seq(from = 1, to = t, by = 1)){
    cw <- ch %*% normvars[,i]
    
    rand <- sig*sqrt(dt)*cw
    drift <- (m - (sig^2)/2)*dt
    
    st <- as.numeric(path[,i])*exp(drift + rand) 
    path[,i+1] <- st
    
  }
  
  path
    
  }
  
