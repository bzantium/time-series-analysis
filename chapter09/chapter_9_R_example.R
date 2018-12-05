library(ggplot2)
library(scales)
library(lubridate)
library(astsa)
library(forecast)
library(lmtest)
library(portes)
library(aTSA)

#example 9.1
data <- read.csv('../timedata/ex8_7.txt', sep='', header=FALSE)
z <- na.omit(c(t(data)))
time <- 1:length(z)
df <- data.frame(z, time)
ggplot(aes(x=time, y=z), data=df) +
  geom_line()

acf2(z)
arimaModel <- Arima(z, order=c(1,0,0))
acf2(arimaModel$residuals)
npred <- 25
pred <- as.numeric(predict(arimaModel, npred)$pred)
se <- as.numeric(predict(arimaModel, npred)$se)
ub <- pred + qnorm(0.975) * se
lb <- pred - qnorm(0.975) * se

pred <- c(rep(NA, (length(z))), pred)
ub <- c(rep(NA, (length(z))), ub)
lb <- c(rep(NA, (length(z))), lb)
z <- c(z, rep(NA, npred))
time <- 1:length(z)
df <- data.frame(z, time, pred, ub, lb)

options(warn=-1)
ggplot(aes(x=time), data=df) + 
  geom_line(aes(y=z, color="Z")) + 
  geom_line(aes(y=pred, color="forecast"), linetype="longdash") +
  geom_ribbon(aes(ymin=lb,ymax=ub), alpha=0.2) +
  scale_x_continuous(breaks=seq(0,125,25)) +
  scale_color_manual(values = c('Z' = 'black', 'forecast' = 'blue')) +
  theme(legend.position = c(0.06, 0.92), 
        legend.background=element_rect(fill="transparent"),
        legend.title=element_blank())
options(warn=0)

#example 9.5
data <- read.csv('../timedata/ex9_5.txt', sep='', header=FALSE)
z <- na.omit(c(t(data)))
time <- 1:length(z)
df <- data.frame(z, time)
ggplot(aes(x=time, y=z), data=df) +
  geom_line()

acf2(z)
adf.test(z) # fail to reject H_0
dz <- diff(z)
acf2(dz) # MA(1)
arimaModel <- Arima(z, order=c(0,1,1), method="ML")
acf2(arimaModel$residuals)
npred <- 50
pred <- as.numeric(predict(arimaModel, npred)$pred)
se <- as.numeric(predict(arimaModel, npred)$se)
ub <- pred + qnorm(0.975) * se
lb <- pred - qnorm(0.975) * se

pred <- c(rep(NA, (length(z))), pred)
ub <- c(rep(NA, (length(z))), ub)
lb <- c(rep(NA, (length(z))), lb)
z <- c(z, rep(NA, npred))
time <- 1:length(z)
df <- data.frame(z, time, pred, ub, lb)

options(warn=-1)
ggplot(aes(x=time), data=df) + 
  geom_line(aes(y=z, color="Z")) + 
  geom_line(aes(y=pred, color="forecast"), linetype="longdash") +
  geom_ribbon(aes(ymin=lb,ymax=ub), alpha=0.2) +
  scale_x_continuous(breaks=seq(0,350,50)) +
  scale_color_manual(values = c('Z' = 'black', 'forecast' = 'blue')) +
  theme(legend.position = c(0.06, 0.92), 
        legend.background=element_rect(fill="transparent"),
        legend.title=element_blank())
options(warn=0)
