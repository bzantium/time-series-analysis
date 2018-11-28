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

residual <- as.numeric(arimaModel$residuals)
acf2(residual)

ggplot(aes(sample=residual), data=df) +
  stat_qq() + 
  stat_qq_line()
LjungBox(arimaModel, lags=seq(6,24,6))

df <- data.frame(residual, time)
ggplot(aes(x=time, y=residual), data=df) + geom_line()

#example 8.7
data <- read.csv('../timedata/ex8_7.txt', sep='', header=FALSE)
z <- na.omit(c(t(data)))
time <- 1:length(z)
df <- data.frame(z, time)
ggplot(aes(x=time, y=z), data=df) + geom_line()
LjungBox(z, lags=seq(6,24,6))

acf2(z)
arimaModel <- Arima(z, order=c(1,0,0), method="CSS") 
summary(arimaModel)
coeftest(arimaModel)

residual <- as.numeric(arimaModel$residuals)
acf2(residual)
LjungBox(arimaModel, lags=seq(6,24,6))

df <- data.frame(residual=residual, time)
ggplot(aes(sample=residual), data=df) +
  stat_qq() + 
  stat_qq_line()
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
adf.test(stock, nlag=5)
acf2(stock)

dstock <- diff(stock, 1)
df <- data.frame(dstock, week=week[1:(length(week)-1)])
ggplot(aes(x=week, y=dstock), data=df) + 
  geom_line() + 
  geom_hline(yintercept=0, color="grey20", linetype="dashed")
acf2(dstock)
df <- data.frame(residual=dstock)
ggplot(aes(sample=residual), data=df) +
  stat_qq() + 
  stat_qq_line()
LjungBox(dstock, lags=seq(6,24,6))

x_bar <- mean(dstock)
std <- sd(dstock)
nu <- length(dstock)-1
tstat <- x_bar/(std/sqrt(nu))
p_value <- 2*(1-pt(abs(tstat), nu))

#example 8.9
data <- read.csv('../timedata/female.txt', sep='', header=FALSE)
female <- as.numeric(na.omit(c(t(data))))
date <- ymd("821201") + months(1:length(female)-1)
df <- data.frame(female, date)
ggplot(aes(x=date, y=female), data=df) + geom_line()
LjungBox(female, lags=seq(6,24,6))
adf.test(female, nlag=5)
acf2(female)

lmModel <- lm(female~date, data=df)
lmModel$coefficients
residual <- lmModel$residuals
df <- data.frame(date, residual)
ggplot(aes(x=date, y=residual), data=df) + geom_line()

residual <- lmModel$residuals
acf2(residual)
arimaModel <- Arima(residual, order=c(1,0,0), include.mean=F)
coeftest(arimaModel)
residual <- as.numeric(arimaModel$residuals)
acf2(residual)
df <- data.frame(date, residual)
ggplot(aes(x=date, y=residual), data=df) + 
  geom_line() +
  geom_hline(yintercept=0, color="grey20", linetype="dashed")

dfemale <- diff(female, 1)
df <- data.frame(dfemale, date=date[1:(length(date)-1)])
ggplot(aes(x=date, y=dfemale), data=df) + 
  geom_line() + 
  geom_hline(yintercept=0, color="grey20", linetype="dashed")
acf2(dfemale)
LjungBox(dfemale, lags=seq(6,24,6))
