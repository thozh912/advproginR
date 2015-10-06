rm(list=ls())
library(TSA)
library(zoo)
library(lubridate)


fMRI <- read.csv2("fMRI.csv")
fMRI_ts <- ts(fMRI[,2])

Xt <- rep(0,9)
Xt <-c(Xt,rep(c(rep(1,10),rep(0,10)),8))
Xt <- Xt[-(length(Xt)-8):-length(Xt)]


plot(fMRI_ts,ylim=c(500,540),main=expression("BOLD signal over time together with a time series X"[t]),xlab="2 Second interval #",ylab="Oxygen level")
lines(510 +15 * Xt ,col="red")
legend( x="topleft", 
        legend=c("BOLD signal",expression("X"[t])),
        col=c("black","red"), lwd=1, lty=c(1,1) 
)

# discarding first two observations of fMRI and Xt

fMRI_ts <- fMRI_ts[-1:-2]
Xt <- Xt[-1:-2]

ccf(Xt, fMRI_ts, lag.max = 24 ,main = " Sample Cross-Correlation between Xt and the Bold signal", ylab = "CCF")

acf(fMRI_ts)
pacf(fMRI_ts)
eacf(fMRI_ts)

armafMRI <- arima(fMRI_ts, order = c(2, 0, 1))
res <- residuals(armafMRI)
plot(res)
acf(res)
pacf(res)

prewhiten(x = fMRI_ts, y = Xt, x.model = armafMRI, ylab="CCF")



#Problem 2

apple <- read.csv2("Apple.csv")
apple_ts <- ts(apple[,2])
apple_model <-apple_ts[1:(length(apple_ts)-500)]
apple_test <- apple_ts[(length(apple_ts)-500):length(apple_ts)]

plot(apple_ts,main="Price of Apple share from 07/07/05 to 07/07/15",ylab="$/share",xlab="Trading days after July 6th,2005")
plot(apple_model,type='l')
acf(apple_model,lag.max = 4000)
pacf(apple_model)
# It is not stationary


r_app <- ts(diff(log(apple_model)) *100)
plot(r_app, main= "Log-difference rt of the Apple series")
abline(h = 0)
acf(r_app, main= "Sample ACF of the log-difference rt of the Apple series")
pacf(r_app, main= "Sample PACF of the log-difference rt of the Apple series")

r_app2 <- r_app^2
plot(r_app2)
acf(r_app2)
pacf(r_app2)

Box.test(r_app2, type='Ljung')

eacf(r_app2)
eacf(r_app)
#from the eacf for r_app2 maybe GARCH(3, 1)?
#from the eacf for r_app maybe GARCH(1, 1)?
garch_r_app <- garch(r_app, order = c(3, 1))
summary(garch_r_app)

garch_r_app2 <- garch(r_app, order = c(1, 1))
summary(garch_r_app2)

garch_r_app3 <- garch(r_app, order = c(3, 2))
summary(garch_r_app3)

plot(residuals(garch_r_app), type = "h", ylab = "Standardize residuals from the fitted GARCH(3,1) model")
plot(residuals(garch_r_app2), type = "h", ylab = "Standardize residuals from the fitted GARCH(3,0) model")
plot(residuals(garch_r_app3), type = "h", ylab = "Standardize residuals from the fitted GARCH(3,2) model")


