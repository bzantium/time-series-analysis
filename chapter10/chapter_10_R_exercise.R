library(ggplot2)
library(scales)
library(lubridate)
library(astsa)
library(forecast)
library(lmtest)
library(portes)
library(aTSA)

#exercise 10.2
data <- read.csv('../timedata/tourist.txt', sep='', header=FALSE)
pass <- na.omit(c(t(data)))
lenz <- length(pass)
date <- ymd("19801201") + months(1:lenz-1)
lpass <- log(pass)

arimaModel1 <- Arima(lpass, order=c(0,1,1), seasonal=list(order=c(0,1,1), period=12), method="ML")
arimaModel2 <- Arima(lpass, order=c(0,1,1), seasonal=list(order=c(1,1,0), period=12), method="ML")
coeftest(arimaModel1)
coeftest(arimaModel2)

df <- data.frame(date, residual=as.numeric(residuals(arimaModel1)))
ggplot(aes(x=date), data=df) + geom_line(aes(y=residual)) + 
  scale_x_date(breaks="1 year", labels = date_format("%Y"))

df <- data.frame(date, residual=as.numeric(residuals(arimaModel2)))
ggplot(aes(x=date), data=df) + geom_line(aes(y=residual)) + 
  scale_x_date(breaks="1 year", labels = date_format("%Y"))

arimaModel1$aic
arimaModel2$aic
arimaModel1$bic
arimaModel2$bic

#exercise 10.8
data <- read.csv('../timedata/mindex.txt', sep='', header=FALSE)
mindex <- na.omit(c(t(data)))
lenz <- length(mindex)
date <- ymd("860101") + months(1:lenz-1)

df <- data.frame(mindex, date)
ggplot(aes(x=date), data=df) + geom_line(aes(y=mindex)) + 
  scale_x_date(breaks="1 year", labels = date_format("%Y"))

mindex1 <- c(rep(NA,1), diff(mindex))
mindex12 <- c(rep(NA,12), diff(mindex, 12))
mindex121 <- c(rep(NA,1), diff(mindex12, 1))
df <- data.frame(mindex, mindex1, mindex12, mindex121, date)

options(warn=-1)
ggplot(aes(x=date), data=df) + geom_line(aes(y=mindex1)) + 
  scale_x_date(breaks="1 year", labels = date_format("%Y"))

ggplot(aes(x=date), data=df) + geom_line(aes(y=mindex12)) + 
  scale_x_date(breaks="1 year", labels = date_format("%Y"))

ggplot(aes(x=date), data=df) + geom_line(aes(y=mindex121)) + 
  scale_x_date(breaks="1 year", labels = date_format("%Y"))
options(warn=0)

acf2(mindex1, max.lag=30)
arimaModel <- Arima(mindex, order=c(3,1,2))
coeftest(arimaModel)
residual <- as.numeric(residuals(arimaModel))
df <- data.frame(date, residual)
ggplot(aes(x=date), data=df) + geom_line(aes(y=residual)) + 
  scale_x_date(breaks="1 year", labels = date_format("%Y"))
acf2(residual, max.lag=30)
arimaModel$aic
arimaModel$bic

arimaModel <- Arima(mindex, order=c(3,1,2), seasonal=list(order=c(1,0,0), period=12))
coeftest(arimaModel)
residual <- as.numeric(residuals(arimaModel))
df <- data.frame(date, residual)
ggplot(aes(x=date), data=df) + geom_line(aes(y=residual)) + 
  scale_x_date(breaks="1 year", labels = date_format("%Y"))
acf2(residual, max.lag=30)
arimaModel$aic
arimaModel$bic

arimaModel <- Arima(mindex, order=c(3,1,2), seasonal=list(order=c(1,0,1), period=12))
coeftest(arimaModel)
residual <- as.numeric(residuals(arimaModel))
df <- data.frame(date, residual)
ggplot(aes(x=date), data=df) + geom_line(aes(y=residual)) + 
  scale_x_date(breaks="1 year", labels = date_format("%Y"))
acf2(residual, max.lag=30)
arimaModel$aic
arimaModel$bic

arimaModel <- Arima(mindex, order=c(3,1,2), seasonal=list(order=c(2,0,0), period=12))
coeftest(arimaModel)
residual <- as.numeric(residuals(arimaModel))
df <- data.frame(date, residual)
ggplot(aes(x=date), data=df) + geom_line(aes(y=residual)) + 
  scale_x_date(breaks="1 year", labels = date_format("%Y"))
acf2(residual, max.lag=30)
arimaModel$aic
arimaModel$bic
