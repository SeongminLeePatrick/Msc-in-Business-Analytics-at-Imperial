install.packages("forecast")
install.packages("tseries")
library(forecast)
library(tseries)
library(ggplot2)

setwd("~/00. Spring/Logistics & Supply Chain Analytics/Topic1 demand fcst")
data <- read.csv(file = "shampoo.csv", header = TRUE)
head(data)

shampoo <- ts(data[, 2], frequency = 12, start = c(2001, 1)) #convert
#frequency = 12: 12 months(seasonality: 1 month date = 12), 
#start = c(2001, 1) : 2001-january


adf.test(shampoo)  # Null: time series is NOT stationary
pp.test(shampoo)   # Null: time series is NOT stationary
kpss.test(shampoo)


plot.ts(shampoo)
autoplot(shampoo)
ggtsdisplay(shampoo)

### stationarize time series
# take first order difference
shampoo.diff1 <- diff(shampoo, differences = 1)
autoplot(shampoo.diff1)
ggtsdisplay(shampoo.diff1)

# stationary test (only look at the p-value) 
adf.test(shampoo.diff1)  # Null: time series is NOT stationary
pp.test(shampoo.diff1)   # Null: time series is NOT stationary
kpss.test(shampoo.diff1) # Null: time series is stationary ***
ndiffs(shampoo) # how many differece to reach the stationary

#check the data if seasonality -> stl function (gray bar is large then no seaonality factor)
shampoo %>% stl(s.window = "period") %>% autoplot
# seasonal stationary
nsdiffs(shampoo, m = 12, test = "ocsb", max.D = 1) 
# what is the number of seasonal difference to take to get the stationary
#(timeseries data, m =frequency, test = "ocsb", max.D = max difference(ususally 1))
#m = 12 means 13-jan with 12-jan,,,, daily data then 7,,,, 


### determine optimal p and q
# acf plot
acf(shampoo.diff1, lag.max = 20) # q <= 2 (lag from 0)
ggAcf(shampoo.diff1) #ggAcf from the forecast pkgv same as acf (lag from 1)

# pacf plot
pacf(shampoo.diff1, lag.max = 20) # p <= 1
ggPacf(shampoo.diff1)


# Based on the above analysis, we can set component of auto.arima function
# if there is no seasonal difference, we can set D = 0
# choose optimal p and q based on information criteria
auto.arima(shampoo, trace = TRUE, ic = 'bic') 
# Best model: ARIMA(1,1,1)(0,0,1)[12] with drift (BIC=407.3)
# Second best: ARIMA(1,1,1) with drift (BIC=408.7)
# [12] : seasonality
# with drift: ÀýÆí 

# two candidate models (estimation)
shampoo.m1 <- Arima(shampoo, order = c(1, 1, 1), 
                       seasonal = list(order = c(0, 0, 1), period = 12), include.drift = TRUE)
shampoo.m2 <- Arima(shampoo, order = c(1, 1, 1), include.drift = TRUE)

# in-sample one-step forecasts
accuracy(shampoo.m1) # look at RMSE

# residual analysis (white noise = 0 mean)
autoplot(shampoo.m1$residuals)
ggAcf(shampoo.m1$residuals)
Box.test(shampoo.m1$residuals, lag = 20, type = "Ljung-Box") 
# null : all the obv are independent
# look at p-value: can't reject the null
# if rejecting the null, we have to go back to auto.arima and build a new model

checkresiduals(shampoo.m1) # cool
# 1. residual plot, 
# 2. acf plot, 
# 3. histgram of residual - avg should be 0 and shape should be normal dist

# forecast
shampoo.f <- forecast(shampoo.m1, h = 6)
autoplot(shampoo.f)


### model evaluation
# Fit model with first 2 and half years of data
auto.arima(window(shampoo, end = c(2003, 6)), d = 1, trace = TRUE, ic = 'bic')
# best model : ARIMA(1,1,1) with drift : 332.667
# sec  model : ARIMA(0,1,1) with drift : 332.9748

# two candidate models: ARIMA(1,1,1) / ARIMA(0,1,1) with drift
m1 <- Arima(window(shampoo, end = c(2003, 6)), order = c(1, 1, 1), include.drift =  TRUE)
m2 <- Arima(window(shampoo, end = c(2003, 6)), order = c(0, 1, 1), include.drift =  TRUE)

# Apply fitted model to later data
m1.f <- Arima(window(shampoo, start = c(2003, 7)), model = m1)
m2.f <- Arima(window(shampoo, start = c(2003, 7)), model = m2)

# out-of-sample one-step forecasts
accuracy(m1.f)
accuracy(m2.f)
# in terms of RMSE, model1 is better than m2


# out-of-sample multi-step forecasts (h = 6: FCST next half year)
# accracy(fcst , true val)
accuracy(forecast(m1, h = 6), window(shampoo, start = c(2003, 7)))
accuracy(forecast(m2, h = 6), window(shampoo, start = c(2003, 7)))
# model1 is better than model2 in terms of RMSE.