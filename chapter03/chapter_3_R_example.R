library(lubridate)
library(ggplot2)
library(forecast)

# example 3.1: Simple Exponential Smoothing
data <- read.csv('../timedata/mindex.txt', sep='', header=FALSE)
mindex <- na.omit(c(t(data)))
date <- ymd("860101") + months(1:length(mindex)-1)

w <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,
       0.81,0.82,0.83,0.84,0.85,0.86,0.87,
       0.88,0.89,0.90,0.91,0.92,0.93,0.94,
       0.95,0.96,0.97,0.98,0.99)

sse <- c(1460.371,1012.6626,779.00814,648.75022,
         570.4980,521.570,491.440,475.1534,
         474.1885,473.3390,472.6044,471.9842,
         471.478,471.0859,470.8079,470.6749,
         470.5947,470.661,470.842,471.1406,
         471.5567,472.0919,472.7475,473.525,
         474.4263,475.4532,476.6078)

df <- data.frame(w, sse)
ggplot(data=df, aes(x=w)) + 
  geom_line(aes(y=sse, colour='sse'))

mindexTS <- ts(mindex, start=1986, frequency=12)
hwt <- HoltWinters(mindexTS, alpha=0.89, beta=F, gamma=F)
plot(hwt)
autoplot(forecast(hwt, h=6))
df <- data.frame(date, as.numeric(hwt$x-c(hwt$fitted[1],hwt$fitted[,1])))
colnames(df) <- c('date', 'residual')
ggplot(data=df, aes(x=date)) + 
  geom_line(aes(y=residual)) +
  geom_hline(yintercept=0)

hwt <- HoltWinters(mindexTS, alpha=0.2, beta=F, gamma=F)
plot(hwt)
autoplot(forecast(hwt, h=6))
df <- data.frame(date, as.numeric(hwt$x-c(hwt$fitted[1],hwt$fitted[,1])))
colnames(df) <- c('date', 'residual')
ggplot(data=df, aes(x=date)) + 
  geom_line(aes(y=residual)) +
  geom_hline(yintercept=0)

# example 3.2: Double Exponential Smoothing
data <- read.csv('../timedata/stock.txt', sep='', header=FALSE)
stock <- na.omit(c(t(data)))
date <- ymd("840101") + months(1:length(stock)-1)

stockTS <- ts(stock, start=1986, frequency=12)
hwt <- HoltWinters(stockTS, alpha=0.6, beta=0.6, gamma=F)
plot(hwt)
autoplot(forecast(hwt, h=6))
df <- data.frame(date, as.numeric(hwt$x-c(hwt$fitted[1:2],hwt$fitted[,1])))
colnames(df) <- c('date', 'residual')
ggplot(data=df, aes(x=date)) + 
  geom_line(aes(y=residual)) +
  geom_hline(yintercept=0)

# example 3.3: Winters Seasonal Exponential Smoothing
data <- read.csv('../timedata/koreapass.txt', sep='', header=FALSE)
koreapass <- na.omit(c(t(data)))
date <- ymd("810101") + months(1:length(koreapass)-1)

koreapassTS <- ts(koreapass, start=1986, frequency=12)
hwt <- HoltWinters(koreapassTS, alpha=0.4, beta=0.1, gamma=0.7)
plot(hwt)
autoplot(forecast(hwt, h=12))
df <- data.frame(date, as.numeric(hwt$x-c(hwt$fitted[1:12],hwt$fitted[,1])))
colnames(df) <- c('date', 'residual')
ggplot(data=df, aes(x=date)) + 
  geom_line(aes(y=residual)) +
  geom_hline(yintercept=0)

koreapassTS <- ts(koreapass, start=1986, frequency=12)
hwt <- HoltWinters(koreapassTS, alpha=0.5, beta=0.1, gamma=0.4, seasonal='mult')
plot(hwt)
autoplot(forecast(hwt, h=12))
df <- data.frame(date, as.numeric(hwt$x-c(hwt$fitted[1:12],hwt$fitted[,1])))
colnames(df) <- c('date', 'residual')
ggplot(data=df, aes(x=date)) + 
  geom_line(aes(y=residual)) +
  geom_hline(yintercept=0)