library(dplyr)


###############Importing dataset####################################################

head(df)

###############Transforming Prices to returns#######################################

##data <- data.frame(df["Date"])
##data <- slice(data, 1:(n()-1))
##for(i in colnames(df)[2:length(colnames(df))]){
##  v = as.vector(unlist(df[i]))
##  vr = diff(v)/(v[-length(v)])
##  data[i] <- vr
#}

data <- data.frame(c(2:nrow(df)-1))
colnames(data) <- "To_remove"
for(i in colnames(df)[1:length(colnames(df))]){
  v = as.vector(unlist(df[i]))
  vr = diff(v)/(v[-length(v)])
  data[i] <- vr
}

data <- slice(data, 1:(n() - 1)) 


#rownames(data) <- data$Date
data$To_remove <- NULL

colnames(data) <- TKs
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


