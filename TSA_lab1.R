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
  for(point in (delay+1):length(ts_name)){
    x<-c(x,ts_name[point-delay])
    y<-c(y,ts_name[point])
  }
  
  plot(x,y,main=c(title,paste("t_delayed = t -",delay)),xlab=expression("X"[t_delayed]),ylab=expression("X"[t]))
  return(plot)
}

delay_scatter(rhine_ts,1,"Scatterplot of N conc at time t against N conc at time t_delayed where")
delay_scatter(rhine_ts,6,"Scatterplot of N conc at time t against N conc at time t_delayed where")
delay_scatter(rhine_ts,12,"Scatterplot of N conc at time t against N conc at time t_delayed where")



m1 = lm(rhine_ts ~ time(rhine_ts))
plot(rhine_ts,main="Concentration of total Nitrogen in Rhine monthly",ylab="conc",col=4)
lines(ts(fitted(m1),start=c(rhine$Year[1],rhine$Month[1]),freq=12),col=2)
legend("topright",c("Red line is a fitted linear model"))

rhine_residuals<- rstudent(m1)
acf_rhine_residuals <- acf(rhine_residuals,main="Autocorrelation function of residuals of linear model",xlab="months lag")



month. = season(rhine_ts)
m2 = lm(rhine_ts ~ month. + time(rhine_ts))
plot(rhine_ts,main="Concentration of total Nitrogen in Rhine monthly",ylab="conc",xlab="year",col=4)
lines(ts(fitted(m2),start=c(rhine$Year[1],rhine$Month[1]),freq=12),col=3)
legend("topright",c("Green line is a fitted seasonal model"))

resi <- rstudent(m2)

plot(resi,type="l",main="Residuals of the seasonal model of Conc of total Nitrogen in Rhine monthly",ylab="residual",xlab="Month after Dec 1988")
points(resi,pch=as.vector(season(rhine_ts)))
acf_seasonal_residuals<-acf(resi,main="Autocorrelation function of the residuals of the seasonal model",xlab="months lag",ylab="ACF")
qqnorm(resi,main=c("Quantiles of the residuals of the seasonal model","against Theoretical gaussian quantiles"), xlab="theoretical Gaussian quantiles", ylab="seasonal model residual quantiles")
qqline(resi)

data(milk)

plot(milk,main="Average monthly milk production per cow in the US",ylab="lbs milk",xlab="year",col=4)
points(milk,pch=as.vector(season(milk)))

delay_scatter(milk,1,"Monthly milk/cow in month t against monthly milk/cow in month t_delayed where")
delay_scatter(milk,6,"Monthly milk/cow in month t against monthly milk/cow in month t_delayed where")
delay_scatter(milk,12,"Monthly milk/cow in month t against monthly milk/cow in month t_delayed where")

v1 = lm(milk ~ time(milk))
plot(milk,main="Average monthly milk production per cow in the US",ylab="lbs milk", xlab ="year",col=4)
lines(ts(fitted(v1),start=c(1994,1),freq=12),col=2)
legend("topright",c("Red line is a fitted linear model"))

milk_residuals<- rstudent(v1)
acf_milk_residuals <- acf(milk_residuals,main="Autocorrelation function of residuals of linear model",xlab="months lag")

month. = season(milk)
v2 = lm(milk ~ month. + time(milk))
plot(milk,main="Average monthly milk production per cow in the US",ylab="lbs milk",xlab="year",col=4)
lines(ts(fitted(v2),start=c(1994,1),freq=12),col=3)
legend("topleft",c("Green line is a fitted seasonal model"))

resid <- rstudent(v2)

plot(resid,type="l",main="Residuals of the seasonal model of Average monthly milk production per cow",ylab="residual",xlab="Month after Dec 1993")
points(resid,pch=as.vector(season(milk)))
acf_seasonal_residuals<-acf(resid,main="Autocorrelation function of the residuals of the seasonal model",xlab="months lag",ylab="ACF")
qqnorm(resid,main=c("Quantiles of the residuals of the seasonal model","against Theoretical gaussian quantiles"), xlab="theoretical Gaussian quantiles", ylab="seasonal model residual quantiles")
qqline(resid)



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

silver_ts <- ts(silver$EURO,start=c(silver$Date[1]),freq=52)
plot(silver_ts,main="Price of one ounce silver in Euro",ylab="Euro",xlab="Years after 2004",col=10)
# mean and variance is not constant
logsilv <- log(silver_ts)
difflogsilv <- diff(logsilv)

plot(difflogsilv,main="Difference of logarithms of price of silver",xlab="Years after 2004",ylab="diff(log(Euro))",col=6)
# mean is constant, not variance
acf(difflogsilv, lag.max = 250, main=c("Autocorrelation function for the difference of","logarithms of price of silver"), xlab="Years lag",col=3)
qqnorm(difflogsilv,main=c("Quantiles of the difference of logarithms of silver","against Theoretical gaussian quantiles"), xlab="theoretical Gaussian quantiles", ylab="diff(log(silver price)) quantiles")
qqline(difflogsilv)

boxcoxvaluessilver <- BoxCox.ar(silver_ts)
#box suggests -,15 = lambda is best parameter value in that transform

plot(diff(diff(logsilv)),main="Difference of difference of logarithms of price of silver",xlab="Years after 2004",ylab="diff(diff(log(Euro)))",col=9)
acf(diff(difflogsilv),lag.max=250,main=c("Autocorrelation function for the difference of the difference of","logarithms of price of silver"),xlab="Years lag",ylab="diff(diff(log(Euro)))",col=2)
qqnorm(diff(difflogsilv),main=c("Quantiles of the difference of difference of logarithms of silver","against Theoretical gaussian quantiles"), xlab="theoretical Gaussian quantiles", ylab="diff(diff(log(silver price))) quantiles")
qqline(diff(difflogsilv))

data(winnebago)
plot(winnebago,main="Monthly sales of winnebagos in Forrest City,Iowa",xlab="year",ylab="units")
logwinne <- log(winnebago)
difflogwinne <- diff(logwinne)
plot(difflogwinne,main="Difference of logarithms of winnebago sales monthly",xlab="Year",ylab="diff(log(units))",col=6)
boxcoxvalueswinne <- BoxCox.ar(winnebago)
#box best suggestion a log transform
acf(difflogwinne,lag.max=60,main=c("Autocorrelation function for the difference of","logarithms of winnebago sales monthly"), xlab="Years lag",col=3)
qqnorm(difflogwinne,main=c("Quantiles of the difference of logarithms of winnebago sales monthly","against Theoretical gaussian quantiles"), xlab="theoretical Gaussian quantiles", ylab="diff(log(units)) quantiles")
qqline(difflogwinne)
