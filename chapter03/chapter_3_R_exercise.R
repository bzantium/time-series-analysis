library(lubridate)
library(ggplot2)
library(forecast)

# exercise 3.5
# female
data <- read.csv('../timedata/female.txt', sep='', header=FALSE)
female <- na.omit(c(t(data)))
date <- ymd("860101") + months(1:length(female)-1)

femaleTS <- ts(female, start=1986, frequency=12)
hwt <- HoltWinters(femaleTS, alpha=0.89, beta=0.89, gamma=F)
plot(hwt)
autoplot(forecast(hwt, h=6))
df <- data.frame(date, as.numeric(hwt$x-c(hwt$fitted[1:2],hwt$fitted[,1])))
colnames(df) <- c('date', 'residual')
ggplot(data=df, aes(x=date)) + 
  geom_line(aes(y=residual)) +
  geom_hline(yintercept=0)

# build
data <- read.csv('../timedata/build.txt', sep='', header=FALSE)
build <- na.omit(c(t(data)))
date <- ymd("860101") + months(1:length(build)-1)

buildTS <- ts(build, start=1986, frequency=12)
hwt <- HoltWinters(buildTS, alpha=0.89, beta=0.89, gamma=F)
plot(hwt)
autoplot(forecast(hwt, h=6))
df <- data.frame(date, as.numeric(hwt$x-c(hwt$fitted[1:2],hwt$fitted[,1])))
colnames(df) <- c('date', 'residual')
ggplot(data=df, aes(x=date)) + 
  geom_line(aes(y=residual)) +
  geom_hline(yintercept=0)

# export
data <- read.csv('../timedata/export.txt', sep='', header=FALSE)
export <- na.omit(c(t(data)))
date <- ymd("860101") + months(1:length(export)-1)

exportTS <- ts(export, start=1986, frequency=12)
hwt <- HoltWinters(exportTS, alpha=0.89, beta=0.89, gamma=F)
plot(hwt)
autoplot(forecast(hwt, h=6))
df <- data.frame(date, as.numeric(hwt$x-c(hwt$fitted[1:2],hwt$fitted[,1])))
colnames(df) <- c('date', 'residual')
ggplot(data=df, aes(x=date)) + 
  geom_line(aes(y=residual)) +
  geom_hline(yintercept=0)

# usapass
data <- read.csv('../timedata/usapass.txt', sep='', header=FALSE)
usapass <- na.omit(c(t(data)))
date <- ymd("860101") + months(1:length(usapass)-1)

usapassTS <- ts(usapass, start=1986, frequency=12)
hwt <- HoltWinters(usapassTS, seasonal='mult')
plot(hwt)
autoplot(forecast(hwt, h=6))
df <- data.frame(date, as.numeric(hwt$x-c(hwt$fitted[1:12],hwt$fitted[,1])))
colnames(df) <- c('date', 'residual')
ggplot(data=df, aes(x=date)) + 
  geom_line(aes(y=residual)) +
  geom_hline(yintercept=0)

# depart
data <- read.csv('../timedata/depart.txt', sep='', header=FALSE)
depart <- na.omit(c(t(data)))
date <- ymd("860101") + months(1:length(depart)-1)

departTS <- ts(depart, start=1986, frequency=12)
hwt <- HoltWinters(departTS, seasonal='mult')
plot(hwt)
autoplot(forecast(hwt, h=6))
df <- data.frame(date, as.numeric(hwt$x-c(hwt$fitted[1:12],hwt$fitted[,1])))
colnames(df) <- c('date', 'residual')
ggplot(data=df, aes(x=date)) + 
  geom_line(aes(y=residual)) +
  geom_hline(yintercept=0)

# stationery
data <- read.csv('../timedata/stationery.txt', sep='', header=FALSE)
stationery <- na.omit(c(t(data)))
date <- ymd("860101") + months(1:length(stationery)-1)

stationeryTS <- ts(stationery, start=1986, frequency=12)
hwt <- HoltWinters(stationeryTS, alpha=0.89, beta=F, gamma=F)
plot(hwt)
autoplot(forecast(hwt, h=6))
df <- data.frame(date, as.numeric(hwt$x-c(hwt$fitted[1],hwt$fitted[,1])))
colnames(df) <- c('date', 'residual')
ggplot(data=df, aes(x=date)) + 
  geom_line(aes(y=residual)) +
  geom_hline(yintercept=0)

# book
data <- read.csv('../timedata/book.txt', sep='', header=FALSE)
book <- na.omit(c(t(data)))
date <- ymd("860101") + months(1:length(book)-1)

bookTS <- ts(book, start=1986, frequency=12)
hwt <- HoltWinters(bookTS, alpha=0.89, beta=0.89, gamma=F)
plot(hwt)
autoplot(forecast(hwt, h=6))
df <- data.frame(date, as.numeric(hwt$x-c(hwt$fitted[1:2],hwt$fitted[,1])))
colnames(df) <- c('date', 'residual')
ggplot(data=df, aes(x=date)) + 
  geom_line(aes(y=residual)) +
  geom_hline(yintercept=0)
