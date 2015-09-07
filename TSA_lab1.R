rm(list=ls())
library(TSA)
rhine = read.csv2("CopyOfRhine.csv")
silver = read.csv2("CopyOfsilver.csv")
head(rhine)

rhine_ts = ts(rhine$TotN_conc,start=c(rhine$Year[1],rhine$Month[1]),freq=12)
plot(rhine_ts,main="Concentration of total Nitrogen in Rhine monthly",ylab="conc",xlab="year",col=4)

points(rhine_ts,pch=as.vector(season(rhine_ts)))

delay_scatter<-function(ts_name,delay,title){
  x<-c()
  y<-c()
  for(point in (delay+1):length(rhine_ts)){
    x<-c(x,rhine_ts[point-delay])
    y<-c(y,rhine_ts[point])
  }
  
  plot(x,y,main=c(title,paste("t_delayed = t -",delay)),xlab=expression("X"[t_delayed]),ylab=expression("X"[t]))
  return(plot)
}

delay_scatter(rhine_ts,1,"Scatterplot of N conc at time t against N conc at time t_delayed where")
delay_scatter(rhine_ts,6,"Scatterplot of N conc at time t against N conc at time t_delayed where")
delay_scatter(rhine_ts,12,"Scatterplot of N conc at time t against N conc at time t_delayed where")

acf_rhine_ts<-acf(rhine_ts,main="Autocorrelation function of Concentration of total Nitrogen in Rhine",xlab="years lag")

m1 = lm(rhine_ts ~ time(rhine_ts))
plot(rhine_ts,main="Concentration of total Nitrogen in Rhine monthly",ylab="conc",col=4)
lines(ts(fitted(m1),start=c(rhine$Year[1],rhine$Month[1]),freq=12),col=2)
legend("topright",c("Red line is a fitted linear model"))

rhine_residuals<- rstudent(m1)
acf_rhine_resudials <- acf(rhine_residuals,main="Autocorrelation function of residuals of linear model",xlab="years lag")



month. = season(rhine_ts)
m2 = lm(rhine_ts ~ month. + time(rhine_ts))
plot(rhine_ts,main="Concentration of total Nitrogen in Rhine monthly",ylab="conc",xlab="year",col=4)
lines(ts(fitted(m2),start=c(rhine$Year[1],rhine$Month[1]),freq=12),col=3)
legend("topright",c("Green line is a fitted seasonal model"))

resi <- rstudent(m2)
#summary(m2)
plot(resi,type="l",main="Residuals of the seasonal model of Conc of total Nitrogen in Rhine monthly",ylab="residual",xlab="Month after Dec 1988")
points(resi,pch=as.vector(season(rhine_ts)))
acf_seasonal_residuals<-acf(resi,main="Autocorrelation function of the residuals of the seasonal model",xlab="months lag",ylab="ACF")
qqnorm(resi,main="Quantiles of the residuals of the seasonal model against Theoretical gaussian quantiles", xlab="theoretical Gaussian quantiles", ylab="seasonal model residual quantiles")
qqline(resi)

#The first model M1 is an AR(1) model with root x = 1/0.8 = 5/4. It is stationary.
#The second model M2 is not stationary since phi2 - phi1 = 1
#The third model is stationary with complex conjugate roots

# calculates roots for M2 and M3
# since one of the roots of M2 has absolute value 1, M2 is not stationary
phi1 <- as.complex(c(-.5,0))
phi2 <- as.complex(c(0.5,-.64))

AR2charroots<-function(phi1,phi2){
  root1 <- (phi1 + sqrt(phi1^2 + 4 * phi2)) / (-2 * phi2)
  root2 <- (phi1 - sqrt(phi1^2 + 4 * phi2)) / (-2 * phi2)
  result <- cbind(root1,root2)
  return(result)
}

# Autocorrelation function for M1 is simply 0.8^k for k = 1,2,3,4
#Variance for M1 is 1 / (1-0.64) = 1/ 0.36 = 25 / 9
#Variance for M3 is given by (4.3.20) and is (1.64/0.36) * 1 / (1.64^2) 
AR2autocorr<-function(phi1,phi2){
  rho1 <- phi1 / (1 - phi2)
  rho2 <- phi1 * rho1 + phi2 
  rho3 <- phi1 * rho2 + phi2 * rho1
  rho4 <- phi1 * rho3 + phi2 * rho2
  result <- cbind(rho1,rho2,rho3,rho4)
  return(result)
}

AR2autocorr(as.double(phi1),as.double(phi2))
AR2charroots(phi1,phi2)

AR2sim <- function(T,phi1,phi2){
  AR2 <- c(0,0)
  
  for(i in 1:T){
    AR2 <- c(AR2, phi1 * AR2[i+1] + phi2 * AR2[i] + rnorm(1) )
  }
  AR2 <-AR2[-1:-2]
  plot(AR2,type="l", main = c("Simulated AR(2) time series with phi1=",phi1,"and phi2=",phi2))
  acf(ts(AR2),main =c("Autocorrelation function for AR(2) process with phi1=",phi1,"and phi2=",phi2))
  return(AR2)
}

AR2sim(100,.8,0)
AR2sim(100,-.5,.5)
AR2sim(100,0,-.64)



