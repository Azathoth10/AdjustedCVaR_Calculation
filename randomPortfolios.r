rnd01 <- c()

for( i in c(1:100)){
  
  r <- as.numeric(rand_bytes(5)) 
  r <- r/sum(r)
  
  
  rnd01 <- rbind(rnd01, r)
  
}

rnd01 <- as.matrix(rnd01)
  
sum(rnd01[50])
rnd01[]

