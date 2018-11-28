library(ggplot2)
library(scales)
library(lubridate)
library(astsa)
library(forecast)
library(lmtest)
library(portes)
library(aTSA)

#example 10.5
data <- read.csv('../timedata/tourist.txt', sep='', header=FALSE)
pass <- na.omit(c(t(data)))
lenz <- length(pass)
date <- ymd("19801201") + months(1:lenz-1)
lpass <- log(pass)
lpass1 <- c(rep(NA,1), diff(lpass))
lpass12 <- c(rep(NA,12), diff(lpass, 12))
lpass121 <- c(rep(NA,1), diff(lpass12, 1))
df <- data.frame(pass, lpass, lpass1, lpass12, lpass121, date)

options(warn=-1)
ggplot(aes(x=date), data=df) + geom_line(aes(y=pass)) + 
  scale_x_date(breaks="1 year", labels = date_format("%Y"))

ggplot(aes(x=date), data=df) + geom_line(aes(y=lpass)) + 
  scale_x_date(breaks="1 year", labels = date_format("%Y"))

ggplot(aes(x=date), data=df) + geom_line(aes(y=lpass1)) + 
  scale_x_date(breaks="1 year", labels = date_format("%Y"))

ggplot(aes(x=date), data=df) + geom_line(aes(y=lpass12)) + 
  scale_x_date(breaks="1 year", labels = date_format("%Y"))

ggplot(aes(x=date), data=df) + geom_line(aes(y=lpass121)) + 
  scale_x_date(breaks="1 year", labels = date_format("%Y"))
options(warn=0)

acf2(lpass121)
arimaModel <- Arima(lpass, order=c(0,1,1), seasonal=list(order=c(0,1,1), period=12), method="CSS")
coeftest(arimaModel)
df <- data.frame(date, residual=as.numeric(residuals(arimaModel)))
ggplot(aes(x=date), data=df) + geom_line(aes(y=residual)) + 
  scale_x_date(breaks="1 year", labels = date_format("%Y"))