library(readr)
library(dplyr)
library(forecast)
library(xts)
library(tseries)
library(lmtest)
library(FitAR)



crime <- read_csv("C:/Users/Claire Li/Desktop/Forecast/crime.csv")

summary(crime)

commonCrime <- crime %>%
  group_by(OFFENSE_CODE_GROUP)%>%
  summarise(count = n())%>%
  arrange(desc(count))
commonCrime

crime$DateTime <- as.POSIXct(crime$OCCURRED_ON_DATE,format="%m/%d/%Y %H:%M")

crime$Hour <- format(as.POSIXct(strptime(crime$OCCURRED_ON_DATE,"%m/%d/%Y %H:%M",tz="")) ,
                format = "%H:%M")

crime$Date <- format(as.POSIXct(strptime(crime$OCCURRED_ON_DATE,"%m/%d/%Y %H:%M",tz="")) ,
               format = "%Y-%m-%d")

crimeMonth <- crime %>% 
  group_by(MONTH) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

crimeMonth

district <- crime %>% 
  group_by(DISTRICT) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
district

myts <- xts(crimeMonth[,1], order.by = date(crimeMonth[,-1], "%Y/%m"))
myts <- ts(as.vector(crimeMonth[,-2]), start=c(2015, 6), end=c(2018, 9), frequency=12)

# Aggregate the data by date and count the crimes that happened on that day
weekForecast <- crime %>%
  group_by(Date)%>%
  summarise(count = n())

# Check for missing data
# There is not missing data
missing <- sum(is.na(weekForecast$count))

# Remove the last two rows
# Outliers that have very low value and NA
weekForecast <- weekForecast[1:1176,]

# Turn the dataframe into xts format for convenience of coercing the date
weekForecast <- as.xts(
  weekForecast$count, order.by = as.Date(weekForecast$Date))

# Coerce daily data into weekly data
weekly <- apply.weekly(weekForecast, sum)

# Weekly time series
myts <- ts(weekly, 
           start=c(2015, 6, 15), 
           end=c(2018, 9, 2), 
           frequency = 52)

# Series plot
par(mar=c(1,1,1,1))
autoplot(myts)

# Decompose the series
mytscompo <- decompose(myts)
plot(mytscompo)

#Remove seasonality
#mytsAdj <- myts - mytscompo$seasonal
#autoplot(mytsAdj)


# Ljung-Box test for white noise 
# Reject the null that the series is white noise
# Conclude that the series is not white noise
Box.test(myts, type="Ljung-Box")


#	Augemented Dickey-Fuller test
# Reject the null hypothesis that it is non-stationary
# Conclude that the time series is stationary
acf(myts)# slow decay
nsdiffs(myts) #number of differencing needed = 1
myts1 <- diff(myts, differences = 1)


adf.test(myts1) #p < 0.01 stationary
acf(myts1) #fast decay, stationary
pacf(myts1)
autoplot(myts1) #looks stationary



# ARIMA(1,0,)
# fit an ARIMA model of order P, D, Q
auto <- auto.arima(myts1)
qqnorm(auto$residuals)
qqline(auto$residuals) 

auto
accuracy(auto)
coeftest(auto)


# p-value = 0.6732
# Conclude that the residuals are white noise
boxresult <- Box.test(auto$residuals,type="Ljung-Box")
boxresult


plot.ts(auto$residuals)     
plotForecastErrors(auto$residuals)
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

   
fitARIMA1 <- arima(myts1, order=c(0,0,2),
                  seasonal = list(order = c(1,0,0), period = 52),
                  method="ML")

fitARIMA2 <- arima(myts1, order=c(0,0,3),
                  seasonal = list(order = c(1,0,0), period = 52),
                  method="ML")

# An SAR signature usually occurs 
# when the autocorrelation at the seasonal period is positive

# Test for the significance of the coefficients
coeftest(fitARIMA1)     
coeftest(fitARIMA2)

# Predictive accuracy
accuracy(fitARIMA1)
accuracy(fitARIMA2)

# predict next 3 observations
autoArimaFit <- auto.arima(myts)
forecast(autoArimaFit,3)
plot(forecast(autoArimaFit, 3))
