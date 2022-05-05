source("gbm.r")
source("stock.r")

paths<-data.frame(t(GBMf(s0, 250, rets, sigmas, cdcorr)))

data1 <- data.frame(c(2:nrow(paths)-1))
for(i in colnames(paths)[1:length(colnames(paths))]){
  v = as.vector(unlist(paths[i]))
  vr = diff(v)/(v[-length(v)])
  data1[i] <- vr
}

data1 <- data1[-1]

for( i  in seq(from =1, to = nrow(data1), by = 1)){
  
  pr <- data1[i,]/length(data[i,])
  data1[i,] <- pr
  
}


