library(dplyr)

###############Importing dataset####################################################

df <- read.csv("Stocks1.csv")
head(df)

###############Transforming Prices to returns#######################################

data <- data.frame(df["Date"])
data <- slice(data, 1:(n()-1))
for(i in colnames(df)[2:length(colnames(df))]){
  v = as.vector(unlist(df[i]))
  vr = diff(v)/(v[-length(v)])
  data[i] <- vr
}

rownames(data) <- data$Date
data$Date <- NULL
head(data)

###############Calculating Returns/stds and Covariance Matrix#############################

corr <- cor(data)

rets <- c()
sigmas <- c()
for(i in colnames(data)[1:length(colnames(data))]){
  v <- as.vector(unlist(data[i]))
  rets <- c(rets, mean(v))
  sigmas <- c(sigmas, sd(v))
}
rets <- matrix(rets)
rownames(rets) <- colnames(data)
colnames(rets) <- ("Returns")

sigmas <- matrix(sigmas)
rownames(sigmas) <- colnames(data)
colnames(sigmas) <- ("Standard Deviations")

################Current stock returns################################################

df$Date <- NULL
s0 <- matrix(df[nrow(df),])
rownames(s0) <- colnames(data)
colnames(s0) <- ("Current Prices")

################Cholesky Decomposition###############################################

cdcorr_t <- chol(corr)
cdcorr <- t(cdcorr_t)
cdcorr %*% cdcorr_t

################Standard Normal Variates#############################################
#normvars <- rnorm(nrow(corr))
#corrWiener <- cdcorr%*% normvars

print(cdcorr)
print(s0)
print(rets)
print(sigmas)


