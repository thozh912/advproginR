rm(list=ls())
library(TSA)
library(zoo)
library(lubridate)

fMRI <- read.csv2("fMRI.csv")
fMRI_ts <- ts(fMRI[,2])
apple <- read.csv2("Apple.csv")

apple_ts <- ts(apple[,2])
apple_model <-apple_ts[1:(length(apple_ts)-500)]
apple_test <- apple_ts[(length(apple_ts)-500):length(apple_ts)]

Xt <- rep(0,9)
Xt <-c(Xt,rep(c(rep(1,10),rep(0,10)),8))
Xt <- Xt[-(length(Xt)-8):-length(Xt)]

plot(apple_ts,main="Price of Apple share from 07/07/05 to 07/07/15",ylab="$/share",xlab="Trading days after July 6th,2005")
plot(fMRI_ts,ylim=c(500,540),main=expression("BOLD signal over time together with a time series X"[t]),xlab="2 Second interval #",ylab="Oxygen level")
lines(510 +15 * Xt ,col="red")
legend( x="topleft", 
        legend=c("BOLD signal",expression("X"[t])),
        col=c("black","red"), lwd=1, lty=c(1,1) 
)

# discarding first two observations of fMRI and Xt

fMRI_ts <-fMRI_ts[-1:-2]
Xt <- Xt[-1:-2]

