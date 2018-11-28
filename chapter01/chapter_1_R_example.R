library(lubridate)
library(ggplot2)

# figure 1.1
t <- 1:100
z <- 5000 + rnorm(100, mean=0, sd=sqrt(20))
df <- data.frame(t, z)
ggplot(data=df, aes(t, z)) + geom_line(color="darkblue")

# figure 1.2
t <- 1:100
z <- 0.5 * t + rnorm(100, 0, sqrt(10))
df <- data.frame(t, z)
ggplot(data=df, aes(t, z)) + geom_line(color="darkblue")

# figure 1.3
t <- 1:120
z <- 10 + 3 * sin(2 * pi * t / 12) + rnorm(120, 0, 0.8)
df <- data.frame(t, z)
ggplot(data=df, aes(t, z)) + geom_line(color="darkblue")

# figure 1.4
data <- read.csv('../timedata/depart.txt', sep='', header=FALSE)
z <- na.omit(c(t(data)))
date <- ymd("940101") + months(1:length(z)-1)
x <- 2.701573 + 0.000409 * as.numeric(date)
df <- data.frame(date, z, x)
ggplot(data=df, aes(date)) + geom_line(aes(y=log(z), colour="z")) + 
  geom_line(aes(y=x, colour="x")) + 
  scale_x_date(date_breaks="1 year") + ylab("sales")

# figure 1.5
data <- read.csv('../timedata/koreapass.txt', sep='', header=FALSE)
z <- na.omit(c(t(data)))
date <- ymd("810101") + months(1:length(z)-1)
df <- data.frame(date, z)
ggplot(data=df, aes(date, z)) + geom_line(color="darkblue")

# figure 1.6
t <- c(0.5*1:60, 2*15:74)
z <- t + rnorm(120, 0, 1)
date <- ymd("850101") + months(1:length(z)-1)
df <- data.frame(date, z)
ggplot(data=df, aes(date, z)) + geom_line(color="darkblue")
