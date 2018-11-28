library(lubridate)
library(ggplot2)
library(astsa)
library(scales)
library(car)
library(forecast)

# example 2.1
data <- read.csv('../timedata/population.txt', sep='', header=FALSE)
pop <- na.omit(c(t(data)))
pop <- round(pop/10000)
lnpop <- log(pop)
t <- 1:length(pop)
t2 <- t**2
year <- 1959 + t
df1 <- data.frame(pop, lnpop, t, t2, year)
ggplot(data=df1, aes(year, pop)) + 
  geom_line() + 
  geom_point(shape=8, color='black')

regmodel <- lm(pop~t, data=df1)
anova(regmodel)
summary(regmodel)

df2 <- data.frame(year, regmodel$fitted.values, regmodel$residuals)
colnames(df2) <- c("year", "pred", "residual")
ggplot(data=df2, aes(year, residual)) + 
  geom_line() + 
  geom_point(shape=3, color='black') + 
  geom_hline(yintercept=0)

regmodel <- lm(pop~t+t2, data=df1)
anova(regmodel)
summary(regmodel)
durbinWatsonTest(regmodel)

df3 <- data.frame(year, pop, regmodel$fitted.values, regmodel$residuals)
colnames(df3) <- c("year", "pop", "pred", "residual")
ggplot(data=df3, aes(x=year)) + 
  geom_line(aes(y=pop, colour='pop')) + 
  geom_line(aes(y=pred, colour='pred')) +
  geom_point(data=df3, aes(y=pop), shape=8, size=1, color='#ff0000') + 
  geom_point(data=df3, aes(y=pred), shape=3, size=1, color='#33ccff') + 
  theme(legend.position = c(0.05,0.9), legend.background=element_rect(fill="transparent"))

ggplot(data=df3, aes(year, residual)) + 
  geom_line() + 
  geom_point(shape=3, color='black') + 
  geom_hline(yintercept=0)

regmodel <- lm(lnpop~t+t2, data=df1)
anova(regmodel)
summary(regmodel)
df4 <- data.frame(year, regmodel$residuals)
colnames(df4) <- c("year", "residual")
ggplot(data=df4, aes(year, residual)) + 
  geom_line() + 
  geom_point(shape=3, color='black') + 
  geom_hline(yintercept=0)

# figure 2.8
t <- 1:100
a1 <- -0.8
a2 <- 1.4
phi1 <- pi/8
phi2 <- 3*pi/4
first <- a1 * sin(pi*t/6 + phi1)
second <- a2 * sin(pi*t/3 + phi2)
z <- first + second
df <- data.frame(t, z, first, second)
ggplot(data=df, aes(t)) + 
  geom_line(aes(y=z, colour='z')) + 
  geom_line(aes(y=first, colour='first')) +
  geom_line(aes(y=second, colour='second')) +
  geom_point(data=df, aes(y=z), shape=20, size=1, color='#3366cc') + 
  geom_point(data=df, aes(y=first), shape=16, size=1, color='#ff0000') + 
  geom_point(data=df, aes(y=second), shape=8, size=1, color='#00cc33') +
  theme(legend.title=element_blank())

# example 2.2 & 2.4
data <- read.csv('../timedata/depart.txt', sep='', header=FALSE)
dept <- na.omit(c(t(data)))
t <- 1:length(dept)
lndept <- log(dept)
date <- ymd("840101") + months(1:length(dept)-1)
mon <- month(date)
i1 <- as.integer(mon==1)
i2 <- as.integer(mon==2)
i3 <- as.integer(mon==3)
i4 <- as.integer(mon==4)
i5 <- as.integer(mon==5)
i6 <- as.integer(mon==6)
i7 <- as.integer(mon==7)
i8 <- as.integer(mon==8)
i9 <- as.integer(mon==9)
i10 <- as.integer(mon==10)
i11 <- as.integer(mon==11)
i12 <- as.integer(mon==12)
df1 <- data.frame(date, dept, lndept, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12)

ggplot(data=df1, aes(date, dept)) +
  geom_line()

ggplot(data=df1, aes(date, lndept)) +
  geom_line()

regmodel <- lm(lndept~t+i1+i2+i3+i4+i5+i6+i7+i8+i9+i10+i11+i12+0, data=df1)
regmodel$coefficients

df2 <- data.frame(date, residual=as.numeric(regmodel$residuals))

ggplot(data=df2, aes(date, residual)) +
  geom_line()

acf2(residuals(regmodel))
ar3res <- arima(residuals(regmodel),order=c(3,0,0))

df3 <- data.frame(date, residual=as.numeric(ar3res$residuals))
ggplot(data=df3, aes(date, residual)) +
  geom_line() +
  geom_hline(yintercept=0) +
  scale_x_date(date_breaks = "1 year",
               labels = date_format("JAN %y"))

# figure 2.12
t <- 1:60

b0 <- 0.2
b1 <- -12
z1 <- exp(b0 + b1/t)

b0 <- 10
b1 <- 0.15
k <- 1
z2 <- k*exp(-b0*exp(-b1*t))

b0 <- 0.95
b1 <- 0.09
z3 <- (1-b0*exp((-b1)*t))**3

b0 <- 5
b1 <- -0.2
z4 <- k/(1+exp(b0+b1*t))

df <- data.frame(t, z1, z2, z3, z4)

ggplot(data=df, aes(x=t)) +
  geom_line(aes(y=z3, colour='von')) +
  geom_line(aes(y=z2, colour='gom')) +
  geom_line(aes(y=z4, colour='pearl')) +
  geom_line(aes(y=z1, colour='exp')) +
  geom_point(data=df, aes(y=z3), shape=8, size=1, color='purple') + 
  geom_point(data=df, aes(y=z2), shape=16, size=1, color='lightgreen') + 
  geom_point(data=df, aes(y=z4), shape=5, size=1, color='lightblue') +
  geom_point(data=df, aes(y=z1), shape=20, size=1, color='red') +
  theme(legend.position = c(0.05,0.87), legend.background=element_rect(fill="transparent"), legend.title=element_blank())

# example 2.3
data <- read.csv('../timedata/catv.txt', sep='', header=FALSE)
catv <- na.omit(c(t(data)))
t <- 1:length(catv)
year <- 1969 + t
k <- 70000000
lncatv <- log(k/catv-1)
df1 <- data.frame(year, catv, lncatv)

ggplot(data=df1, aes(year, catv)) +
  geom_point(shape=3)

ggplot(data=df1, aes(year, lncatv)) +
  geom_point(shape=3)

regmodel <- lm(lncatv~year, data=df1)
anova(regmodel)
summary(regmodel)

p1 <- k/(exp(regmodel$fitted.values)+1)
residual <- catv-p1
df2 <- data.frame(year, regmodel$fitted.values, p1, residual)
colnames(df2) <- c('year', 'pred', 'p1', 'residual')

ggplot(data=df2, aes(x=year)) + 
  geom_point(data=df2, aes(y=catv, colour='catv'), shape=3, size=1) +
  geom_point(data=df2, aes(y=p1, colour='forecast'), shape=8, size=1) +
  theme(legend.position = c(0.1, 0.96), 
        legend.background=element_rect(fill="transparent"), 
        legend.title=element_blank(),
        legend.direction = "horizontal")

ggplot(data=df2, aes(x=year, y=residual)) +
  geom_point(shape=3, size=1) +
  geom_hline(yintercept=0, color="#999999")

# trend model prediction
z <- c(23,25,27,34,38,47,49,39,57,59,63,64,69,78,73,89,83,84,86,92)
t <- 1:length(z)
regmodel <- lm(z~t)
new_t <- 1:32
new <- data.frame(t=new_t)
df <- data.frame(predict(lm(z~t), new, interval="prediction", level=0.95))
df['t'] <- new_t
df['z'] <- c(z, rep('NA', 12))

ggplot(data=df, aes(x=t)) + 
  geom_line(aes(y=fit, colour='prediction')) + 
  geom_line(aes(y=lwr, colour='lower')) + 
  geom_line(aes(y=upr, colour='upper')) + 
  geom_line(aes(x=t,y=as.numeric(z), colour='z')) +
  geom_point(aes(y=fit), shape='p') + 
  geom_point(aes(y=lwr), shape='L') +
  geom_point(aes(y=upr), shape='U') +
  geom_point(aes(y=as.numeric(z)), shape=20) +
  geom_vline(xintercept=21, color='#999999') +
  theme(legend.position = c(0.2, 0.96), 
        legend.background=element_rect(fill="transparent"), 
        legend.title=element_blank(),
        legend.direction = "horizontal")

