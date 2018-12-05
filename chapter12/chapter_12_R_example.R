library(ggplot2)
library(scales)
library(lubridate)
library(astsa)
library(forecast)
library(lmtest)
library(portes)
library(tsoutliers)

#example 12.1
data <- read.csv('../timedata/ozone.txt', sep='', header=FALSE)
ozone <- na.omit(c(t(data)))
lenz <- length(ozone)
date <- ymd("19550101") + months(1:lenz-1)

month <- month(date)
year <- year(date)

x1 <- as.numeric(year > 1960)
summer <- as.numeric((5<month) * (month<11) * (year>1965))
winter <- (year>1965) - summer

df <- data.frame(ozone, date)
ggplot(aes(x=date), data=df) + geom_line(aes(y=ozone)) + 
  scale_x_date(breaks="2 year", labels = date_format("Jan %y"))

acf2(ozone[year<1960])
LjungBox(ozone[year<1960], seq(6,24,6))

acf2(ozone)
LjungBox(ts(ozone), seq(6,24,6))

period <- 12
arimaModel <- Arima(ozone[year<1960], order=c(0,0,1), 
                    seasonal=list(order=c(0,0,1), period=period), 
                    include.mean=FALSE)
coeftest(arimaModel)

npred <- 12
pred <- as.numeric(predict(arimaModel, npred)$pred)
se <- as.numeric(predict(arimaModel, npred)$se)
ub <- pred + qnorm(0.975) * se
lb <- pred - qnorm(0.975) * se
date <- ymd("19600101") + months(1:npred-1)

df <- data.frame(pred, ub, lb, date)
ggplot(aes(x=date), data=df) + 
  geom_line(aes(y=pred), linetype="longdash", color="blue") +
  geom_ribbon(aes(ymin=lb,ymax=ub), alpha=0.2) +
  scale_x_date(breaks="1 month", labels = date_format("%m")) +
  ylab("forecast")

pred <- c(rep(NA, lenz), pred)
ub <- c(rep(NA, lenz), ub)
lb <- c(rep(NA, lenz), lb)
ozone <- c(ozone, rep(NA, npred))
date <- ymd("19550101") + months(1:length(ozone)-1)

df <- data.frame(ozone, pred, ub, lb, date)
options(warn=-1)
ggplot(aes(x=date), data=df) + 
  geom_line(aes(y=ozone, color="Z")) + 
  geom_line(aes(y=pred, color="forecast"), linetype="longdash") +
  geom_ribbon(aes(ymin=lb,ymax=ub), alpha=0.2) +
  scale_color_manual(values = c('Z' = 'black', 'forecast' = 'blue')) +
  scale_x_date(breaks="2 year", labels = date_format("%Y")) +
  theme(legend.position = c(0.06, 0.13), 
        legend.background=element_rect(fill="transparent"),
        legend.title=element_blank())
options(warn=0)

residual <- arimaModel$residuals
acf2(residual)
LjungBox(arimaModel, seq(6,24,6))

ozone <- head(ozone, -npred)
dozone <- diff(ozone, period)
dx1 <- diff(x1, period)
summer <- head(summer, -npred)
winter <- head(winter, -npred)
df2 <- data.frame(dx1, summer, winter)
arimaModel <- Arima(dozone, order=c(0,0,1), 
                    seasonal=list(order=c(0,0,1), period=period), 
                    xreg=df2, include.mean=FALSE)
coeftest(arimaModel)
residual <- arimaModel$residuals
acf2(residual)
LjungBox(arimaModel, seq(6,24,6))

#example 12.2
data <- read.csv('../timedata/const.txt', sep='', header=FALSE)
area <- na.omit(c(t(data)))
lenz <- length(area)
date <- ymd("19800101") + months(1:lenz-1)
phase <- as.numeric(date > ymd('19890201') & date < ymd('19910801'))

df <- data.frame(area, date)
ggplot(aes(x=date), data=df) + geom_line(aes(y=area)) + 
  scale_x_date(breaks="2 year", labels = date_format("Jan %y"))

larea <- log(area)
df <- data.frame(larea, date)
ggplot(aes(x=date), data=df) + geom_line(aes(y=larea)) + 
  scale_x_date(breaks="2 year", labels = date_format("Jan %y"))

period <- 12
dlarea <- diff(diff(larea, 1), period)
dphase <- diff(diff(phase, 1), period)

arimaModel <- Arima(dlarea, order=c(0,0,1), 
                    seasonal=list(order=c(0,0,1), period=period), 
                    xreg=dphase, include.mean=FALSE)
coeftest(arimaModel)
residual <- arimaModel$residuals
acf2(residual)

npred <- 12
pred <- as.numeric(predict(arimaModel, newxreg=rep(0,npred), npred)$pred)
se <- as.numeric(predict(arimaModel, newxreg=rep(0,npred), npred)$se)
ub <- pred + qnorm(0.975) * se
lb <- pred - qnorm(0.975) * se
date <- ymd("19950201") + months(1:npred-1)

df <- data.frame(pred, ub, lb, date)
ggplot(aes(x=date), data=df) + 
  geom_line(aes(y=pred), linetype="longdash", color="blue") +
  geom_ribbon(aes(ymin=lb,ymax=ub), alpha=0.2) +
  scale_x_date(breaks="1 month", labels = date_format("%m")) +
  ylab("forecast")

larea1 <- diff(larea, 1)

larea1.fitted <- arimaModel$fitted + head(larea1, -period)
larea.fitted <- larea1.fitted + head(tail(larea, length(larea1.fitted) + 1), length(larea1.fitted))
larea1.pred <- pred + head(tail(larea1, npred + period), npred)
larea.pred <- larea1.pred + head(tail(larea, npred + 1), npred)
larea1.ub <- ub + head(tail(larea1, npred + period), npred)
larea.ub <- larea1.ub + head(tail(larea, npred + 1), npred)
larea1.lb <- lb + head(tail(larea1, npred + period), npred)
larea.lb <- larea1.lb + head(tail(larea, npred), npred)

area.fitted <- exp(larea.fitted)
area.pred <- exp(larea.pred)
area.ub <- exp(larea.ub)
area.lb <- exp(larea.lb)

area.fitted <- c(rep(NA, 13), area.fitted, rep(NA, npred))
area.pred <- c(rep(NA, lenz), area.pred)
area.ub <- c(rep(NA, lenz), area.ub)
area.lb <- c(rep(NA, lenz), area.lb)
area <- c(area, rep(NA, npred))
date <- ymd("19800101") + months(1:length(area)-1)
df <- data.frame(area, area.fitted, pred=area.pred, ub=area.ub, lb=area.lb, date)
options(warn=-1)
ggplot(aes(x=date), data=df) + 
  geom_line(aes(y=area, color="Z")) + 
  geom_line(aes(y=area.fitted, color="fitted")) + 
  geom_line(aes(y=pred, color="forecast"), linetype="longdash") +
  geom_ribbon(aes(ymin=lb,ymax=ub), alpha=0.2) +
  scale_color_manual(values = c('Z' = 'black', 'fitted'='blue', 'forecast' = 'blue')) +
  scale_x_date(breaks="2 year", labels = date_format("%Y")) +
  theme(legend.position = c(0.06, 0.90), 
        legend.background=element_rect(fill="transparent"),
        legend.title=element_blank())
options(warn=0)

#example 12.4
data <- read.csv('../timedata/ozone.txt', sep='', header=FALSE)
ozone <- na.omit(c(t(data)))
lenz <- length(ozone)
date <- ymd("19550101") + months(1:lenz-1)
outlier.ozone <- tso(ts(ozone), 
                     types=c("AO", "LS"), 
                     tsmethod="arima", 
                     cval=3.1,
                     args.tsmethod=list(order=c(0,0,1), seasonal=list(order=c(0,1,1), period=12)))
outlier.ozone
plot(outlier.ozone)

