#######GBM function 
#Out###n number of paths for number of assets
#In####
#s0####Initial stock prices
#t#####Time steps number
#m#####mean
#sig###standard deviations
#ch####Lower triangular cholesky times correlation matrix
#dt####time intervals

GBMf <- function(s0, t, m, sig, ch, dt = 1./365){
  
  path <- cbind(s0, matrix(0, nrow(ch), t))
  
  normvars <- matrix(rnorm(nrow(ch), nrow(ch), t))
  
  
  for(i in seq(from = 1, to = t, by = 1)){
    cw <- ch %*% normvars[,i]
    
    rand <- sig*sqrt(dt)*cw
    drift <- (m - (sig^2)/2)*dt
    
    st <- path[i]*exp(drift + rand) 
    path[,i+1] <- st
    
  }
  
  path
    
  }
  
