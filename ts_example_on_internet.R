library(TSA)
library(forecast)

data <- rep(cos(1:52*(3.1416/26)),5)*100+1000
a.ts <- ts(data,start=c(2009,1),frequency=52)

## Predict from previous '3' years then one year out & generate the plot
a.win  <- window(a.ts,start=c(end(a.ts)[1]-3,end(a.ts)[2]),frequency=52)
a.fit  <- auto.arima(a.win)  
a.pred <- forecast(a.fit, h=52)
plot(a.pred, type="l", xlab="weeks", ylab="counts",
     main="Overlay forecasts & actuals",
     sub="green=FIT(1-105,by 16) wks back & PREDICT(26) wks, blue=52 wks")

for (j in seq(1, 90, by=8)) {   ## Loop to overlay early forecasts 
  result1 <- tryCatch({
    b.end   <- c(end(a.ts)[1],end(a.ts)[2]-j) ## Window the time series  
    b.start <- c(b.end[1]-3,b.end[2])
    b.window <- window(a.ts, start=b.start, end=b.end, frequency=52)
    
    b.fit  <-auto.arima(b.window) 
    b.pred <- forecast(b.fit, h=26)
    lines(b.pred$mean, col="green", lty="dashed" )
  }, error = function(e) {return(e$message)} ) ## Skip Errors
}