library(lubridate)
library(ggplot2)
library(car)
library(forecast)
library(seasonal)

# example 4.1
data <- read.csv('../timedata/food.txt', sep='', header=FALSE)
food <- na.omit(c(t(data)))
date <- ymd("800101") + months(1:length(food)-1)
i1 <- as.numeric(month(date) == 1)
i2 <- as.numeric(month(date) == 2)
i3 <- as.numeric(month(date) == 3)
i4 <- as.numeric(month(date) == 4)
i5 <- as.numeric(month(date) == 5)
i6 <- as.numeric(month(date) == 6)
i7 <- as.numeric(month(date) == 7)
i8 <- as.numeric(month(date) == 8)
i9 <- as.numeric(month(date) == 9)
i10 <- as.numeric(month(date) == 10)
i11 <- as.numeric(month(date) == 11)
i12 <- as.numeric(month(date) == 12)
t <- 1:length(food)

df <- data.frame(date, food, t)
reg1 <- lm(food~t, data=df)
trend <- reg1$fitted.values
adjtrend <- food/trend
durbinWatsonTest(reg1)
reg2 <- Arima(adjtrend, xreg=i1+i2+i3+i4+i5+i6+i7+i8+i9+i10+i11, order=c(13,0,0))
seasonal <- as.numeric(reg2$fitted)
irregula <- adjtrend/seasonal
fitted <- trend*seasonal

ggplot(data=df, aes(x=date, y=seasonal)) + geom_line()
ggplot(data=df, aes(x=date, y=irregula)) + geom_line()
ggplot(data=df, aes(x=date)) + 
  geom_line(aes(y=food, colour='food')) + 
  geom_line(aes(y=fitted, colour='fitted')) +
  theme(legend.position = c(0.05,0.9), legend.background=element_rect(fill="transparent"))

# example 4.2
data <- read.csv('../timedata/mindex.txt', sep='', header=FALSE)
mindex <- na.omit(c(t(data)))
date <- ymd("800101") + months(1:length(mindex)-1)
m3 <- ma(mindex, 3)
m7 <- ma(mindex, 7)
df <- data.frame(date, mindex, m3, m7)

ggplot(data=df, aes(x=date)) + 
  geom_line(aes(y=mindex, colour='mindex')) + 
  geom_line(aes(y=m3, colour='m3')) +
  theme(legend.position = c(0.05,0.9), legend.background=element_rect(fill="transparent"))

  
ggplot(data=df, aes(x=date)) + 
  geom_line(aes(y=mindex, colour='mindex')) + 
  geom_line(aes(y=m7, colour='m7')) + 
  theme(legend.position = c(0.05,0.9), legend.background=element_rect(fill="transparent"))
  
# example 4.3
data <- read.csv('../timedata/food.txt', sep='', header=FALSE)
food <- na.omit(c(t(data)))
date <- ymd("800101") + months(1:length(food)-1)
tsfood <- ts(food, frequency=12, start=c(1980,1))
decom <- decompose(tsfood, type="additive") # moving average, season average, 
trend <- decom$trend
seasonal <- decom$seasonal
irregula <- decom$random
adjseason <- decom$x - decom$seasonal
df <- data.frame(date, food, trend, seasonal, irregula, adjseason)

ggplot(data=df, aes(x=date)) + 
  geom_line(aes(y=food, colour='food')) + 
  geom_line(aes(y=trend, colour='trend')) + 
  theme(legend.position = c(0.07,0.91), legend.background=element_rect(fill="transparent"), legend.title=element_blank())

ggplot(data=df, aes(x=date)) + 
  geom_line(aes(y=food, colour='food')) + 
  geom_line(aes(y=seasonal, colour='seasonal')) + 
  theme(legend.position = c(0.07,0.91), legend.background=element_rect(fill="transparent"), legend.title=element_blank())

ggplot(data=df, aes(x=date)) + 
  geom_line(aes(y=food, colour='food')) + 
  geom_line(aes(y=irregula, colour='irregula')) + 
  theme(legend.position = c(0.07,0.91), legend.background=element_rect(fill="transparent"), legend.title=element_blank())

ggplot(data=df, aes(x=date)) + 
  geom_line(aes(y=food, colour='food')) + 
  geom_line(aes(y=adjseason, colour='adjseason')) + 
  theme(legend.position = c(0.07,0.91), legend.background=element_rect(fill="transparent"), legend.title=element_blank())

# decompose equivalent:
# mafood <- ma(food, 12, centre=TRUE)
# adjtrend <- food - mafood
# season <- rep(0,12)
# for(i in 1:12) {
#   season[i] <- mean(na.omit(adjtrend[0:11*12+i])) - mean(na.omit(adjtrend))
# }
# season <- rep(season, 12)
# irr <- adjtrend - season

z <- c(10,12,8,12,7,5,8,7,9,10,3,6,8,4,9,12,8,12,13,9)
t <- 1:length(z)
ma3 <- ma(z, 3)
ma33 <- ma(ma3, 3)
ma35 <- ma(ma3, 5)
ma12 <- ma(z, 12, centre=F)
ma122 <- ma(ma12, 2, centre=F)
hendersn <- rep(0, length(z)-4)
for(i in 1:(length(z)-4)) {
  hendersn[i] <- sum(c(-0.073, 0.294, 0.558, 0.294, -0.073) * z[0:4+i])
}
hendersn <- c(rep(NA,2), hendersn, rep(NA,2))
df <- data.frame(t,z,ma3,ma33,ma35,ma12, ma122,hendersn)
ggplot(data=df, aes(x=t)) + 
  geom_line(aes(y=z, colour='z')) + 
  geom_line(aes(y=ma3, colour='ma3')) + 
  geom_line(aes(y=ma33, colour='ma33')) + 
  geom_line(aes(y=ma35, colour='ma35')) + 
  geom_line(aes(y=ma12, colour='ma12')) + 
  geom_line(aes(y=ma122, colour='ma122')) + 
  geom_line(aes(y=hendersn, colour='hendersn')) + 
  theme(legend.position = c(0.07,0.91), legend.background=element_rect(fill="transparent"), legend.title=element_blank())

# example 4.4
data <- read.csv('../timedata/food.txt', sep='', header=FALSE)
food <- na.omit(c(t(data)))
date <- ymd("800101") + months(1:length(food)-1)
tsfood <- ts(food, frequency=12, start=c(1980,1))
foodout <- seas(tsfood, x11="")
autoplot(foodout)
seasonal <- foodout$series$d10
adjseason <- foodout$series$d11
trend <- foodout$series$d12
irregula <- as.numeric(foodout$series$d13)
df <- data.frame(date, food, trend, seasonal, irregula, adjseason)
df

ggplot(data=df, aes(x=date)) + 
  geom_line(aes(y=food, colour='food')) + 
  geom_line(aes(y=trend, colour='trend')) + 
  theme(legend.position = c(0.06,0.92), legend.background=element_rect(fill="transparent"), legend.title=element_blank())

ggplot(data=df, aes(x=date)) +  
  geom_line(aes(y=irregula, colour='irregula')) + 
  geom_hline(yintercept=1) + 
  theme(legend.position = c(0.06,0.95), legend.background=element_rect(fill="transparent"), legend.title=element_blank())

ggplot(data=df, aes(x=date)) + 
  geom_line(aes(y=food, colour='food')) + 
  geom_line(aes(y=adjseason, colour='adjseason')) + 
  theme(legend.position = c(0.08,0.92), legend.background=element_rect(fill="transparent"), legend.title=element_blank())

