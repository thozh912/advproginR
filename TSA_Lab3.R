rm(list=ls())
library(TSA)




fMRI <- read.csv2("fMRI.csv")
fMRI_ts <- ts(fMRI[,2])

Xt <- rep(0,9)
Xt <-ts(c(Xt,rep(c(rep(1,10),rep(0,10)),8)))
Xt <- Xt[-(length(Xt)-8):-length(Xt)]


plot(fMRI_ts,ylim=c(500,540),type="l",main=expression("BOLD signal over time together with a time series X"[t]),xlab="2 Second interval #",ylab="Oxygen level")
lines(510 +15 * Xt ,col="red")
legend( x="topleft", 
        legend=c("BOLD signal",expression("X"[t])),
        col=c("black","red"), lwd=1, lty=c(1,1) 
)

# discarding first two observations of fMRI and Xt

fMRI_ts <- fMRI_ts[-1:-2]
Xt <- Xt[-1:-2]
Xt_futured <- ts(c(Xt[-1], 0))
Xt_lagged3 <- Xt[1:(length(Xt)-3)]
fMRI_ts_lagged3 <- fMRI_ts[4:length(fMRI_ts)]

# The convention in ccf is that first.arg[t+k] get correlated with second.arg[t] at lag k.

ccf(Xt, fMRI_ts, lag.max = 24 ,main = " Sample Cross-Correlation between Xt and the BOLD signal", ylab = "CCF")
ccf(fMRI_ts, Xt, lag.max = 24 ,main = " Sample Cross-Correlation between the BOLD signal and Xt", ylab = "CCF")




# acf(fMRI_ts,main="sample auto-correlation of BOLD signal",xlab="# Measurements lag")
# pacf(fMRI_ts,main="sample partial auto-correlation of BOLD signal",xlab="# Measurements lag")
# eacf(fMRI_ts)
# 
# #Differencing does not appear to help make it easy to ARMA model fMRI.
# 
# # dfMRI_ts <- diff(fMRI_ts)
# # 
# # acf(dfMRI_ts,lag.max=36, main="sample auto-correlation of first difference of BOLD signal",xlab="# Measurements lag")
# # pacf(dfMRI_ts,lag.max=36,main="sample partial auto-correlation of first difference of BOLD signal",xlab="# Measurements lag")
# # eacf(dfMRI_ts)
# 
# #We try to find an ARIMA model for fMRI_ts. (So we can prewhiten the CCF). The eacf suggests an arima(2,0,1)
# 
# armafMRI <- arima(fMRI_ts, order = c(2, 0, 1))
# armafMRI
# res <- residuals(armafMRI)
# plot(res,main="Residuals of an ARIMA(2,0,1) model estimated for the BOLD signal",xlab="2 Second interval #",ylab="Oxygen level residual")
# acf(res,lag.max=36, main ="Auto-correlation function of residuals to ARIMA(2,0,1) model for BOLD signal",xlab="# Measurements lag")
# pacf(res,lag.max=36, main ="partial Auto-correlation function of residuals to ARIMA(2,0,1) model for BOLD signal",xlab="# Measurements lag")


# "s is decided upon how many significant
# spikes there are after the first significant
# spike and before the spikes start to die
# down."
# 
# "The order of the denominator polynomial,
# i.e. r is decided upon how the spikes are
# decaying.
# exponential : choose r = 1
# damped sine wave: choose r = 2
# cuts of after lag b + s : choose r = 0"
# "Since the first significant spike occurs at k = –3 it is natural to set b = 3"

# Our first significant spike occurs at k = +1 <- b = -1.
#Our most significant spike occurs at k = -3.
#There are four significant spikes between the first one and the largest one. -> s = 4
# sine wave damped falling off, we think r = 2

# We assume that the next value in Xt will be a zero.

# It is not necessary to include parameters in the transfer function for lags greater
# than 20 because the covariate X_t has periodicity 20.
# For instance, X_25 returns the same as X_5 and and any parameter multiplied with X_25 
# could just as well be multiplied with X_5. We feel that any parameter beyond
# lag 20 would just contribute to instability in the transfer function model.

# m1 <- arimax(fMRI_ts, order= c(1,0,0),xtransf=Xt,
#              transfer=list(c(0,10)))
# plot(fitted(m1),type="l",xlim=c(1,158),ylim=c(500,530),col="red")
# lines(fMRI_ts,col="black")
# resi_std1 <- rstandard(m1)
# acf(resi_std1,na.action = na.pass,main=c("sample auto-correlation of standardized residuals",
#     "of M1 model of BOLD signal"),xlab="# Measurements lag")
# pacf(resi_std1,na.action = na.pass,main=c("sample partial auto-correlation of standardized residuals",
#                     "of M1 model of BOLD signal"),xlab="# Measurements lag")
# eacf(resi_std1[-(1:10)])





# m1a <- arimax(fMRI_ts, order= c(1,0,0),xtransf=Xt_futured,
#              transfer=list(c(0,10)))
# plot(fitted(m1a),type="l",xlim=c(1,158),ylim=c(500,530),col="red")
# lines(fMRI_ts,col="black")
# resi_std1a <- rstandard(m1a)
# acf(resi_std1a,na.action = na.pass,main=c("sample auto-correlation of standardized residuals",
#                                          "of M2 model of BOLD signal"),xlab="# Measurements lag")
# pacf(resi_std1a,na.action = na.pass,main=c("sample partial auto-correlation of standardized residuals",
#                                           "of M2 model of BOLD signal"),xlab="# Measurements lag")
# eacf(resi_std1a[-(1:10)])




m2 <- arimax(fMRI_ts, order= c(1,0,0),xtransf=Xt,
             transfer=list(c(2,4)))
plot(fitted(m2),type="l",xlim=c(1,158),ylim=c(490,530),col="red",
     main=c("BOLD signal over time together with","fitted Transfer function model M2"),xlab="2 Second interval #",ylab="Oxygen level")
lines(fMRI_ts,col="black")
legend( x="bottomright", 
        legend=c("BOLD signal","Transfer function model"),
        col=c("black","red"), lwd=1, lty=c(1,1) 
)
resi_std2 <- rstandard(m2)
plot(resi_std2,main=c("std. residuals of the BOLD signal fitted by Transfer function model M2"),
     xlab="2 Second interval #",ylab="residual Oxygen level")
acf(resi_std2,na.action = na.pass,main=c("sample auto-correlation of standardized residuals",
                     "of M2 model of BOLD signal"),xlab="# Measurements lag")
pacf(resi_std2,na.action = na.pass,main=c("sample partial auto-correlation of standardized residuals",
                      "of M2 model of BOLD signal"),xlab="# Measurements lag")
eacf(resi_std2[-(1:4)])
qqnorm(resi_std2,main=c("Normal Q-Q plot of the residuals of the BOLD signal",
                        "fitted by Transfer function model M2"))
qqline(resi_std2)


# m2a <- arimax(fMRI_ts_lagged3, order= c(1,0,0),xtransf=Xt_lagged3,
#               transfer=list(c(2,0)))
# plot(fitted(m2a),type="l",xlim=c(1,158),ylim=c(500,530),col="red")
# lines(fMRI_ts_lagged3,col="black")
# resi_std2a <- rstandard(m2a)
# acf(resi_std2a,na.action = na.pass,main=c("sample auto-correlation of standardized residuals",
#                                           "of M2a model of BOLD signal"),xlab="# Measurements lag")
# pacf(resi_std2a,na.action = na.pass,main=c("sample partial auto-correlation of standardized residuals",
#                                            "of M2a model of BOLD signal"),xlab="# Measurements lag")
# eacf(resi_std2a)
# 
# m3 <- arimax(fMRI_ts, order= c(1,0,0),xtransf=Xt_futured,
#              transfer = list(c(2,7)))
# plot(fitted(m3),type="l",xlim=c(1,158),ylim=c(500,530),col="red")
# lines(fMRI_ts,col="black")
# resi_std3 <- rstandard(m3)
# acf(resi_std3,na.action = na.pass,main=c("sample auto-correlation of standardized residuals",
#                      "of M3 model of BOLD signal"),xlab="# Measurements lag")
# pacf(resi_std3,na.action = na.pass,main=c("sample partial auto-correlation of standardized residuals",
#                       "of M3 model of BOLD signal"),xlab="# Measurements lag")
# eacf(resi_std3[-(1:7)])


#1.c.
# We are inferring from help(arimax) that the omegas (besides omega_0) have
# the opposite sign to TSA convention.
# Addenum: after examining how coef works we are unsure about how coef works here. HOW IS ???
m2

# We are going to assume that Y_0 and Y_-1 are set to zero.

Ymatr <- matrix(0,length(fMRI_ts)-2,length(fMRI_ts)-2)
diag(Ymatr) <- -m2$coef[4]
Ymatr <- rbind(0,Ymatr)
Ymatr <- cbind(Ymatr,0)
diag(Ymatr) <- -m2$coef[3]
Ymatr <- rbind(0,Ymatr)
Ymatr <- cbind(Ymatr,0)
diag(Ymatr) <- 1


Xrhs <-rep(0,length(fMRI_ts))
#Xrhs <- rep((1-m2$coef[3]-m2$coef[4])*m2$coef[2],length(fMRI_ts))
Xrhs <- Xrhs + c(m2$coef[6],m2$coef[7],m2$coef[8],m2$coef[9],rep(0,length(fMRI_ts)-4))

Xrhs <- unname(Xrhs)

sol <- solve(Ymatr,Xrhs)
plot(sol[1:15],type ="l",main="Pulse response of our M2 model, base level set to zero",,xlab="2 Second interval #",ylab="Oxygen level above base level")



#Problem 2

apple <- read.csv2("Apple.csv")
apple_ts <- ts(apple[,2])
apple_model <-ts(apple_ts[1:(length(apple_ts)-500)])
apple_test <- ts(apple_ts[(length(apple_ts)-500):length(apple_ts)])

plot(apple_ts,main="Price of Apple share from 07/07/05 to 07/07/15",ylab="$/share",xlab="Trading days after July 6th,2005")

plot(apple_model,main="Price of the Apple shares, training data",ylab="$/share",xlab="Trading days after July 6th,2005")
acf(apple_model,main= "Sample ACF of the log-difference of price of the Apple shares",lag.max=2000)
pacf(apple_model,main= "Sample PACF of the log-difference of price of the Apple shares",lag.max=2000)

# It is not stationary
#There is volatility clustering

r_app <- ts(diff(log(apple_model)))
plot(r_app, main= "Log-difference of price of the Apple shares",ylab="diff(log($/share)",xlab="Trading days after July 6th,2005")
abline(h = 0)
acf(r_app, main= "Sample ACF of the log-difference of price of the Apple shares",lag.max=2000)
pacf(r_app, main= "Sample PACF of the log-difference of price of the Apple shares",lag.max=2000)

r_app2 <- r_app^2
plot(r_app2, main= "Square of the log-difference of price of the Apple shares",ylab="(diff(log($/share))^2",xlab="Trading days after July 6th,2005")
acf(r_app2,main= "Sample ACF of the square of the log-difference of price of the Apple shares",lag.max=2000)
pacf(r_app2, main= "Sample PACF of the square of the log-difference of price of the Apple shares",lag.max=2000)

Box.test(r_app2, type='Ljung')

eacf(r_app2)
eacf(abs(r_app))
#from the eacf for r_app2 maybe GARCH(3, 1)? <-FALSE CONVERGENCE
#from the eacf for r_app2 maybe GARCH(3, 3)? NA
#from the eacf for abs(r_app) maybe GARCH(1, 1)? NA

We are going to go with GARCH(3,2), it shows relative function convergence and it
has non-significant box ljung test of squared residuals. b1 maybe is and b2 is probably zero
(they are not very significant)

# garch_r_app <- garch(r_app, order = c(2,2 ))
# summary(garch_r_app)
# 
# garch_r_app2 <- garch(r_app, order = c(1, 1))
# summary(garch_r_app2)

garch_r_app3 <- garch(r_app, order = c(3, 2))
summary(garch_r_app3)

# garch_r_app4 <- garch(r_app, order = c(3, 3))
# summary(garch_r_app4)

# All models show non-gaussian residuals..


# plot(residuals(garch_r_app), type = "h", main = "Residuals from the fitted GARCH(3,1) model")
# qqnorm(residuals(garch_r_app))
# qqline(residuals(garch_r_app))
# 
# plot(residuals(garch_r_app2), type = "h", main = "Residuals from the fitted GARCH(3,0) model")

plot(residuals(garch_r_app3), type = "h", main = "Std. Residuals from the fitted GARCH(3,2) model")
qqnorm(residuals(garch_r_app3),main=c("Q-Q plot of the std. residuals from the fitted GARCH(3,2) model",
"vs gaussian quantiles"))
qqline(residuals(garch_r_app3))

#plot(residuals(garch_r_app4), type = "h", main = "Residuals from the fitted GARCH(3,3) model")



# garch_r_app_1<- garch(r_app, order = c(6, 2))
# summary(garch_r_app_1)
# 
# garch_r_app_2<- garch(r_app, order = c(5, 5))
# summary(garch_r_app_2)
# 
# qqnorm(residuals(garch_r_app_2))
# qqline(residuals(garch_r_app_2))


res <- residuals(garch_r_app3)
acf(res[1:length(res)]^2, na.action = na.omit)

# I am dropping b2 since it is not significant. b1 has to stay in calculations because of its magnitude.

# tsvar is a little larger than statvar
tsvar <- var(r_app)
statvar <- garch_r_app3$coef[1] / (1- (garch_r_app3$coef[2] + garch_r_app3$coef[3] + garch_r_app3$coef[4] + garch_r_app3$coef[6]))

#Its ok, we fulfill the stationarity condition.

garch32.sim <- garch.sim(alpha = c(1.93e-05,7.8e-02,7.6e-02),beta=c(0,0,6.2e-01),n=2000)
fitt_garch <- fitted(garch_r_app)^2
plot(fitt_garch[,1])
plot(garch32.sim,type="l")
summary(garch32.sim)
blmean <- garch_r_app$coef[1]
blse <- sqrt(garch_r_app$vcov[1,1])
plow <- blmean - 1.96*blse
pup <- blmean + 1.96*blse
