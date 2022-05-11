library(quantmod) 

TKs <- c("AAPL", "MSFT", "TSLA", "GOOG")

myStocks <-as.data.frame(lapply(TKs[1], function(x) {getSymbols(x, 
                                             from = "2021-05-03", 
                                             to = "2022-05-02",
                                             periodicity = "daily",
                                             auto.assign=FALSE)} ))

df <- data.frame(myStocks[4])

for(i in TKs[2:length(TKs)]){
  
  myStocks2 <-as.data.frame(lapply(i, function(x) {getSymbols(x, 
                                                 from = "2021-05-03", 
                                                 to = "2022-05-02",
                                                 periodicity = "daily",
                                                 auto.assign=FALSE)} ))
  
  df[i] <- myStocks2[4]
  
  
}

