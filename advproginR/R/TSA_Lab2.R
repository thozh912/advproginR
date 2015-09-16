rm(list=ls())
library(TSA)

#AR2sim takes scalar integer, vector, vector

AR2sim <- function(T,phi1,phi2){
  AR2 <- matrix(nrow = T, ncol=length(phi1))
  for(j in 1:length(phi1)){
  newAR2 <- c(0,0)
  
  for(i in 1:T){
    newAR2 <- c(newAR2, phi1[j] * newAR2[i+1] + phi2[j] * newAR2[i] + rnorm(1) )
  }
  newAR2 <- newAR2[-1:-2]
  plot(newAR2,type="l", main = c("Simulated AR(2) time series with phi1=",phi1[j],"and phi2=",phi2[j]),ylab="AR(2)", xlab="time")
  acf(ts(newAR2),main =c("Autocorrelation function for AR(2) process with phi1=",phi1[j],"and phi2=",phi2[j]),lag.max=20)
  AR2[,j] <- newAR2
  }
  return(AR2)
}
# stationary values for AR(2) process are following
phi1 =c(-1.4,-0.5,0.6,1.6,0.1)
phi2 =c(-0.8,0.2,-0.2,-0.9,0.7)
AR2sim(30,phi1,phi2)
AR2sim(100,phi1,phi2)
