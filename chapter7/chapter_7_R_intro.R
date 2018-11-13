library(ggplot2)
library(scales)
library(lubridate)
library(astsa)

#example 7.1
data <- read.csv('../timedata/depart.txt', sep='', header=FALSE)
z <- na.omit(c(t(data)))
lenz <- length(z)
logz <- log(z)
dif1 <- c(rep(NA,1),logz[2:lenz]-logz[1:(lenz-1)])
dif1_12 <- c(rep(NA,13),dif1[13:(lenz-1)]-dif1[1:(lenz-13)])
date <- ymd("840101") + months(1:lenz-1)

df <- data.frame(z, logz, dif1, dif1_12, date)
ggplot(data=df, aes(x=date, y=z)) + 
  geom_line() + 
  ylab("depart") +
  scale_x_date(date_breaks = "1 year",
               labels = date_format("JAN%y"))

ggplot(data=df, aes(x=date, y=logz)) + 
  geom_line() + 
  ylab("lnZ") +
  scale_x_date(date_breaks = "1 year",
               labels = date_format("JAN%y"))

ggplot(data=df, aes(x=date, y=dif1)) + 
  geom_line() + 
  geom_hline(yintercept=0) +
  ylab("d_lnZ") +
  scale_x_date(date_breaks = "1 year",
               labels = date_format("JAN%y"))

ggplot(data=df, aes(x=date, y=dif1_12)) + 
  geom_line() + 
  ylab("dd12_lnz") +
  scale_x_date(date_breaks = "1 year",
               labels = date_format("JAN%y"),
               limits=c(date[13], date[lenz]))

#figure 7.3
data <- read.csv('../timedata/interest.txt', sep='', header=FALSE)
interest <- na.omit(c(t(data)))
date <- ymd("820401") + months(1:length(interest)-1)

df <- data.frame(interest, date)
ggplot(data=df, aes(date, interest)) +
  geom_line() +
  geom_vline(xintercept=ymd("911201"), color="grey55") +
  scale_x_date(date_breaks = "1 year",
               labels = date_format("JAN%y"))

#figure 7.4
t <- 1:300
a <- rnorm(300)
a[0] <- 0
lenz <- length(z)
z <- cumsum(a)
dif1 <- c(rep(NA,1), z[2:lenz]-z[1:(lenz-1)])

df <- data.frame(t, z, dif1)
ggplot(data=df, aes(x=t,y=z)) + 
  geom_line() +
  xlab("time")

ggplot(data=df, aes(x=t,y=dif1)) + 
  geom_line() +
  xlab("time") +
  ylab("d_Z")

#figure 7.7
t <- 1:300
z <- rep(0,302)
a1 <- rnorm(1)
for (i in 1:300) {
  a <- rnorm(1)
  z[i+2] <- 1.8*z[i+1] - 0.8*z[i] + a - 0.5*a1
  a1 <- a
}
z <- z[3:302]
dif1 <- c(rep(NA,1), z[2:lenz]-z[1:(lenz-1)])
df <- data.frame(t, z, dif1)

ggplot(data=df, aes(x=t,y=z)) + 
  geom_line() +
  xlab("time")
acf2(z)

ggplot(data=df, aes(x=t,y=dif1)) + 
  geom_line() +
  geom_hline(yintercept=0, color="grey20") + 
  xlab("time") +
  scale_x_continuous(breaks=seq(0,300, by=100))
acf2(dif1)
