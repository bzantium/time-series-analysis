library(ggplot2)
library(scales)
library(lubridate)
library(astsa)
library(forecast)
library(lmtest)
library(portes)
library(aTSA)

#example 8.6
data <- read.csv('../timedata/gas.txt', sep='', header=FALSE)
data <- data[,seq(1,ncol(data),by=2)]
gas <- na.omit(c(t(data)))
time <- 1:length(gas)
df <- data.frame(gas, time)
ggplot(aes(x=time, y=gas), data=df) + geom_line()
# LjungBox(gas, lags=seq(6,24,6))

acf2(gas)
arimaModel <- Arima(gas, order=c(3,0,0), method="CSS")
summary(arimaModel)
coeftest(arimaModel)

arimaModel <- Arima(gas, order=c(3,0,0), include.mean=F , method="ML")
summary(arimaModel)
coeftest(arimaModel)

residual <- arimaModel$residuals
acf2(residual)

qqnorm(residual)
qqline(residual, col=2, lty=2)

LjungBox(arimaModel, lags=seq(6,24,6))

df <- data.frame(residual=as.numeric(residual), time)
ggplot(aes(x=time, y=residual), data=df) + geom_line()

#example 8.7
data <- read.csv('../timedata/ex8_7.txt', sep='', header=FALSE)
z <- na.omit(c(t(data)))
time <- 1:length(z)
df <- data.frame(z, time)
ggplot(aes(x=time, y=z), data=df) + geom_line()
LjungBox(z, lags=seq(6,24,6))

acf2(z)
arimaModel <- Arima(z, order=c(1,0,0)) 
summary(arimaModel)
coeftest(arimaModel)

residual <- arimaModel$residuals
acf2(residual)

qqnorm(residual)
qqline(residual, col=2, lty=2)

LjungBox(arimaModel, lags=seq(6,24,6))

df <- data.frame(residual=as.numeric(residual), time)
ggplot(aes(x=time, y=residual), data=df) + geom_line()

LjungBox(residual, lags=seq(6,24,6))

arimaModel <- Arima(z, order=c(2,0,0))
summary(arimaModel)
coeftest(arimaModel)
arimaModel <- Arima(z, order=c(1,0,1))
summary(arimaModel)
coeftest(arimaModel)

#example 8.8
data <- read.csv('../timedata/elecstock.txt', sep='', header=FALSE)
stock <- as.numeric(na.omit(c(t(data))))
week <- 1:length(stock)
df <- data.frame(stock, week)
ggplot(aes(x=week, y=stock), data=df) + geom_line()
LjungBox(stock, lags=seq(6,24,6))
adf.test(stock, nlag=3)
acf2(stock)


dstock <- diff(stock, 1)
df <- data.frame(dstock, week=week[1:(length(week)-1)])
ggplot(aes(x=week, y=dstock), data=df) + 
  geom_line() + 
  geom_hline(yintercept=0, color="grey20", linetype="dashed")
acf2(dstock)
LjungBox(dstock, lags=seq(6,24,6))

#example 8.9
data <- read.csv('../timedata/female.txt', sep='', header=FALSE)
female <- as.numeric(na.omit(c(t(data))))
date <- ymd("821201") + months(1:length(female)-1)
df <- data.frame(female, date)
ggplot(aes(x=date, y=female), data=df) + geom_line()
LjungBox(female, lags=seq(6,24,6))
adf.test(female, nlag=3)
acf2(female)

dfemale <- diff(female, 1)
df <- data.frame(dfemale, date=date[1:(length(date)-1)])
ggplot(aes(x=date, y=dfemale), data=df) + 
  geom_line() + 
  geom_hline(yintercept=0, color="grey20", linetype="dashed")
acf2(dfemale)
LjungBox(dfemale, lags=seq(6,24,6))
