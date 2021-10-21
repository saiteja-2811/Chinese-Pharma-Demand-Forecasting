library(tidyverse)
library(fpp2)

setwd("C:/Users/saite/PycharmProjects/py38/Interviews/Fedex")

sales_data = read.csv("sales.csv")

# 1. Airline data - Monthly from Jan - 1990 till April - 2004
sales_data_ts <- ts(data = sales_data$Sales, start = c(2009,7),end=c(2012,6),frequency = 12)

sales_data_ts_pre <- window(sales_data_ts,start=c(2009,7),end=c(2012,3))

# Estimate parameters
fc <- ses(sales_data_ts_pre, h=3,alpha=0.4,optimise=FALSE)

# Accuracy of one-step-ahead training errors
round(accuracy(fc),2)

# True Values
plot(sales_data_ts_pre,
     xlab="Month",
     ylab="AMonthly Sales",
     main="Actual & Forecasts from Simple Exp Model",
     xaxs="i",
     lty=1)

# Fitted Values
lines(fc$fitted, lwd = 2,col="blue",lty=2)

autoplot(fc) +
  autolayer(fitted(fc), series=c("Out")) +
  ylab("Monthly Sales") + xlab("Year") 

legend(2009, 9000, legend=c("Actual","Fitted","Forecast"),
       col=c("black","red", "blue"), lty=c(1,2,1), cex=0.8)
