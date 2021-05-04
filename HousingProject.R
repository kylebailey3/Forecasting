# IMPORTING DATA AND CREATING TIME SERIES

library(zoo)
library(forecast)
library(urca)

setwd("C:/Users/Kyle/Desktop/School/20-21WQ/ECON 4120-01")
WAHPI <- read_excel("C:/Users/Kyle/Desktop/School/20-21WQ/ECON 4120-01/WAHPI.xls")

plot(WAHPI,main="Washington State Home Price Index 1975-2020") # initial data plot

WAHPI <- ts(WAHPI$WASTHPI,frequency=4,start=c(1975)) # convert data to time series
logWA <- diff(log(WAHPI))*100 # take first log difference to eliminate upwards trend
plot(logWA,main="First Log Difference WAHPI",ylab="Log Difference") # plot of log time series

acf(logWA,lagmax=20,main="ACF of Log Difference of WA Home Price Index")
pacf(logWA,lag.max=20,main="PACF of Log Difference of WA Home Price Index")

# TESTING STATIONARITY WITH Augmented Dickey-Fuller Test

summary(ur.df(logWA, type ="drift", 6)) # passes test as |t-stat| < |tau2| at 1% significance

# POTENTIAL MODELS

# AR(1)
AR1 <- arima(logWA, order=c(1,0,0))
fAR1 <- forecast(AR1,H=6)

# AR(3)
AR3 <- arima(logWA, order=c(3,0,0))
fAR3 <- forecast(AR3,H=6)

# MA(1)
MA1 <- arima(logWA, order=c(0,0,1))
fMA1 <- forecast(MA1,H=6)

# ARMA(3,0,1)
ARMA301 <-arima(logWA, order=c(3,0,1))
fARMA301 <- forecast(ARMA301,H=6)

# ARMA(3,1,1)
ARMA311 <-arima(logWA, order=c(3,1,1))
fARMA311 <- forecast(ARMA311,H=6)

# FORECAST PLOTS

plot(fAR1, include=24)
plot(fAR3, include=24)
plot(fMA1, include=24)
plot(fARMA301, include=24)
plot(fARMA311, include=24)

# Akaike information criterion (AIC) AND  Bayesian information criterion (BIC) TESTS

AIC(AR1)
BIC(AR1)

AIC(AR3)
BIC(AR3)

AIC(MA1)
BIC(MA1)

AIC(ARMA301)
BIC(ARMA301)

AIC(ARMA311)
BIC(ARMA311)

# FINAL MODEL

logWAr <- ts(logWA[1:160],frequency=4,start=c(1975)) # restrict time series to start of 2015 for in-sample test
model <- arima(logWAr, order=c(3,1,1)) # generate final model from restricted time series for in-sample test
plot(model)

flogWAr <- forecast(model,h=12) 
plot(flogWAr)
summary(flogWAr) # forecast summary

acf(residuals(model), main="ACF of Residuals for ARMA(3,1,1) Model")
pacf(residuals(model), main="PACF of Residuals for ARMA(3,1,1) Model")

plot(flogWAr, include=24)
lines(logWA, col=6)
lines(logWAr, col=0)
