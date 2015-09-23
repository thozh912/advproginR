rm(list=ls())
#library(forecast)
library(TSA)
library("gridExtra")



AR2sim_matr <- function(T,phi1,phi2){
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

AR2sim <- function(T,phi1,phi2,sigma){
  AR2 <- c(0,0)
  
  for(i in 1:T){
    AR2 <- c(AR2, phi1 * AR2[i+1] + phi2 * AR2[i] + rnorm(1,0,sigma) )
  }
  AR2 <-AR2[-1:-2]
  plot(AR2,type="l", main = c(paste("Simulated AR(2) time series with phi1=",phi1),paste("and phi2=",phi2,"and sigma=",sigma)),ylab="AR(2)", xlab="time")
  acf(ts(AR2),main =c(paste("Autocorrelation function for AR(2) process with phi1=",phi1),paste("and phi2=",phi2,"and sigma=",sigma)),lag.max=20)
  return(AR2)
}



AR2LS <- function(Y){
  Yfront <- Y[-1:-2]
  phi1_est <- Y[-c(1,length(Y))]
  phi2_est <- Y[-(length(Y)-1):-length(Y)]
  Ydata <- data.frame(Yfront,phi1_est,phi2_est)
  
  m1 <- lm(Yfront ~ phi1_est + phi2_est,Ydata)
  
  errorterms <- Yfront - m1$coefficients[2] * phi1_est - m1$coefficients[3] * phi2_est
  cond_sumofsquares <- sum(errorterms^2)
  wnvarest <- cond_sumofsquares / (length(Yfront) - 2)
  wnsdest <- sqrt(wnvarest)
  names(wnsdest) <- c("Estimate of the std. dev. of the white noise term in AR(2) process")
  m1$coefficients <- c(m1$coefficients,wnsdest)
  return(m1$coefficients)
}

AR2simaggregator <- function(T,phi1,phi2,sigma){
  phi1estimates <-c()
  phi2estimates <-c()
  wnsdest <-c()
  for(i in 1:100){
    Y <- AR2sim(T,phi1,phi2,sigma)
    phi1estimates <- c(phi1estimates,AR2LS(Y)[2])
    phi2estimates <- c(phi2estimates,AR2LS(Y)[3])
    wnsdest <- c(wnsdest,AR2LS(Y)[4])
  }
  plot1 <- hist(phi1estimates)
  plot2 <- hist(phi2estimates)
  plot3 <- hist(wnsdest)
  sdvec <- c(sd(phi1estimates-phi1),sd(phi2estimates-phi2),mean(wnsdest))
  return(sdvec)
}

Y <- AR2sim(30,1.6,-0.9,5)

AR2LS(Y)

arima(Y,order=c(2,0,0),include.mean=FALSE,method="ML")
AR2simaggregator(30,1.6,-0.9,5)

# stationary values for AR(2) process are following
#phi1 =c(-1.4,-0.5,0.6,1.6,0.1)
#phi2 =c(-0.8,0.2,-0.2,-0.9,0.7)

ele_con <- read.csv2("electricity_consumption.csv")

ele_ts <- ts(ele_con,start=c(1),freq=12)
#plot(ele_ts,main="total Swedish electricity consumption in GWh 1990-2014",xlab="Years after 1 Jan 1989",ylab="GWh")
ele_model <- window(ele_ts,start=1,end=12)

ele_model <- ele_model[,2]
#plot(ele_model,main="total Swedish electricity consumption in GWh 1990-2014",xlab="Years after 1 Jan 1989",ylab="GWh")
ele_test <- window(ele_ts,start=12,end=26)

# DETERMINISTIC SEASONAL FIT FOLLOWED BY ARIMA(1,0,3) MODEL

month. = season(ele_model)

m2 <- lm(ele_model ~ month. + time(ele_model))
lines(ts(fitted(m2),start=c(1),freq=12),col=3)
legend("topleft",c("Green line is a fitted seasonal model"))
resi <- rstandard(m2)
plot(resi,type="l",main="Residuals of the seasonal model",ylab="residual",xlab="Months after 1 Jan 1990")
points(resi,pch=as.vector(season(ele_model)))
acf_seasonal_residuals<-acf(resi,main="Autocorrelation function of the residuals of the seasonal model",xlab="months lag",ylab="ACF",lag.max=36)
pacf_seasonal_residuals<-pacf(resi,main="Partial Autocorrelation function of the residuals of the seasonal model",xlab="months lag",ylab="PACF",lag.max=36)
eacf(resi)
qqnorm(resi,main=c("Quantiles of the residuals of the seasonal model","against Theoretical gaussian quantiles"), xlab="theoretical Gaussian quantiles", ylab="seasonal model residual quantiles")
qqline(resi)

ma4 <- arima(resi,order=c(1,0,3), seasonal = list(order = c(0,0,0)))
fit_ar1_ma3_res <- rstandard(ma4)

plot(fit_ar1_ma3_res,type="l",main="residuals of ARIMA(1,0,3) fit to the residuals of fitted seasonal model",ylab="residual",xlab="Months after 1 Jan 1990")
acf(fit_ar1_ma3_res,main=c("Autocorrelation function of the residuals of the","residuals of ARIMA(1,0,3) fit to the residuals of fitted seasonal model"),xlab="months lag",ylab="ACF",lag.max=36)
pacf(fit_ar1_ma3_res,main=c("Partial Autocorrelation function of the","residuals of ARIMA(1,0,3) fit to the residuals of fitted seasonal model"),,xlab="months lag",ylab="PACF",lag.max=36)
Box.test(fit_ar1_ma3_res,type="Ljung",lag=50)
qqnorm(fit_ar1_ma3_res,main=c("Quantiles of the residuals of ARIMA(1,0,3) fit to the residuals of fitted seasonal model",
                              "against Theoretical gaussian quantiles"), xlab="theoretical Gaussian quantiles", ylab="ARIMA(1,0,3) fit to residuals of seasonal model residual quantiles")
qqline(fit_ar1_ma3_res)
tsdiag(ma4,gof.lag=36)
eacf(fit_ar1_ma3_res,ar.max=25,ma.max=36)



# #DIFFLOG & SEASONAL MODEL
# difflogele <-diff(log(ele_model))
# plot(difflogele)
# month. = season(ele_model)
# month. <- month.[-length(ele_model)]
# m3 <- lm(difflogele ~ month. + time(difflogele))
# lines(ts(fitted(m3),start=c(1),freq=12),col=3)
# 
# 
# seasonaldiff<- diff(difflogele,12)
# plot(seasonaldiff,type="l")
# acf(seasonaldiff,lag.max=36)
# pacf(seasonaldiff,lag.max=36)
# eacf(seasonaldiff)
# ma2 <- arima(seasonaldiff,order=c(2,0,3), seasonal = list(order = c(1,0,0)))
# ma2residuals <- rstandard(m2)
# 
# plot(ma2residuals,type="l")
# acf(ma2residuals)
# pacf(ma2residuals)
# Box.test(ma2residuals,type="Ljung",lag=50)
# qqnorm(ma2residuals)
# qqline(ma2residuals)
# 
# eacf(ma2residuals)

#KISS MODEL FOUND BY ANDREA

ar1 <- arima(ele_model, order = c(1,0,0), seasonal = list(order = c(1,1,0)))
res_ar1 <- rstandard(ar1)

plot(res_ar1,main="Residuals of seasonal ARIMA(1,0,0)X(1,1,0) model",ylab="residual",xlab="Years after 1 Jan 1989")
acf(res_ar1,main="Autocorrelation function of the residuals of seasonal ARIMA(1,0,0)X(1,1,0)",xlab="years lag",ylab="ACF",lag.max=36)
pacf(res_ar1,main="Partial Autocorrelation function of the residuals of seasonal ARIMA(1,0,0)X(1,1,0)",xlab="years lag",ylab="PACF",lag.max=36))
hist(res_ar1)
qqnorm(res_ar1,main=c("Quantiles of the residuals of the seasonal ARIMA(1,0,0)X(1,1,0)","against Theoretical gaussian quantiles"), xlab="theoretical Gaussian quantiles", ylab="seasonal ARIMA(1,0,0)X(1,1,0) residual quantiles")
qqline(res_ar1)
eacf(res_ar1,ar.max=25,ma.max=36)
tsdiag(ar1,gof.lag=36)
Box.test(res_ar1,type="Ljung",lag=50)

# FORECASTING (under construction)

pre <- predict(ar1,n.ahead = 12)
plot(pre$pred,type="l")

plot(ar1, n1 = c(1), n.ahead = 24)

plot(forecast(ar1))

