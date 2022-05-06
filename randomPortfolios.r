library(openssl)

#######Function Generating n random numbers

RWgenerators <- function(nsim, nweights){
  
  
  rnd01 <- c()
  
  for( i in c(1:nsim)){
    
    r <- as.numeric(rand_bytes(nweights)) 
    r <- r/sum(r)
    
    
    rnd01 <- rbind(rnd01, r)
    
  }
  
  rnd01 <- as.matrix(rnd01)
  
}





