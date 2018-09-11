library(lubridate)
library(ggplot2)

# figure 1.1
t <- 1:100
z <- 5000 + rnorm(100, mean=0, sd=sqrt(20))
df <- data.frame(t, z)
ggplot(data = df, aes(t, z)) + geom_line(color="darkblue")

# figure 1.2
t <- 1:100
z <- 0.5 * t + rnorm(100, 0, sqrt(10))
df <- data.frame(t, z)
ggplot(data = df, aes(t, z)) + geom_line(color="darkblue")

# figure 1.3
t <- 1:120
z <- 10 + 3 * sin(2 * pi * t / 12) + rnorm(120, 0, 0.8)
df <- data.frame(t, z)
ggplot(data = df, aes(t, z)) + geom_line(color="darkblue")

# figure 1.4
data <- read.csv('../timedata/depart.txt', sep='', header=FALSE)
z <- na.omit(c(t(data)))
date <- ymd("940101") + months(1:length(z))
x <- 2.701573 + 0.000409 * as.numeric(date)
df <- data.frame(date, z, x)
ggplot(data = df, aes(date)) + geom_line(aes(y=log(z), colour="z")) + 
  geom_line(aes(y=x, colour="x")) + 
  scale_x_date(date_breaks="1 year") + ylab("sales")

# figure 1.5
data <- read.csv('../timedata/koreapass.txt', sep='', header=FALSE)
z <- na.omit(c(t(data)))
date <- ymd("810101") + months(1:length(z))
df <- data.frame(date, z)
ggplot(data=df, aes(date, z)) + geom_line(color="darkblue")

# figure 1.6
t <- c(0.5*1:60, 2*15:74)
z <- t + rnorm(120, 0, 1)
date <- ymd("850101") + months(1:length(z))
df <- data.frame(date, z)
ggplot(data=df, aes(date, z)) + geom_line(color="darkblue")

# ex1_5
t <- 1:100
z1 <- 100 + rnorm(100)
z2 <- 500 + rnorm(100)
z3 <- 100 + rnorm(100, 0, 10)
z4 <- 100 + t * rnorm(100)
df <- data.frame(t,z1,z2,z3,z4)
ggplot(data=df, aes(t)) + 
  geom_line(aes(y=z1, colour='z1')) + 
  geom_line(aes(y=z2, colour='z2')) + 
  geom_line(aes(y=z3, colour='z3')) + 
  geom_line(aes(y=z4, colour='z4')) +
  ylab('z')

# ex1_6_1
t <- 1:100
e <- rep(100, 100)
z <- 100 + rnorm(100)
df <- data.frame(t, e, z)
ggplot(data=df, aes(t)) + geom_line(aes(y=z, colour="Z")) + geom_line(aes(y=e, colour="E[Z]"))

# ex1_6_2
t <- 1:100
e <- 100 + t
z <- 100 + t + rnorm(100)
df <- data.frame(t, e, z)
ggplot(data=df, aes(t)) + geom_line(aes(y=z, colour="Z")) + geom_line(aes(y=e, colour="E[Z]"))

# ex1_6_3
t <- 1:100
e <- 100 + t + 2*t**2
z <- 100 + t + 2*t**2 + rnorm(100)
df <- data.frame(t, e, z)
ggplot(data=df, aes(t)) + geom_line(aes(y=z, colour="Z")) + geom_line(aes(y=e, colour="E[Z]"))

# ex1_6_4
t <- 1:100
e <- 100 + sin(2*pi*t/12) + cos(2*pi*t/12)
z <- 100 + sin(2*pi*t/12) + cos(2*pi*t/12) + rnorm(100, 0, 5)
df <- data.frame(t, e, z)
ggplot(data=df, aes(t)) + geom_line(aes(y=z, colour="Z")) + geom_line(aes(y=e, colour="E[Z]"))

# ex1_6_5
t <- 1:100
e <- 100 + sin(2*pi*t/4) + cos(2*pi*t/4)
z <- 100 + sin(2*pi*t/4) + cos(2*pi*t/4) + rnorm(100, 0, 5)
df <- data.frame(t, e, z)
ggplot(data=df, aes(t)) + geom_line(aes(y=z, colour="Z")) + geom_line(aes(y=e, colour="E[Z]"))

# ex1_6_6
t <- 1:100
e <- 100 + 0.3*t + sin(2*pi*t/12) + cos(2*pi*t/12)
z <- 100 + 0.3*t + sin(2*pi*t/12) + cos(2*pi*t/12) + rnorm(100, 0, 5)
df <- data.frame(t, e, z)
ggplot(data=df, aes(t)) + geom_line(aes(y=z, colour="Z")) + geom_line(aes(y=e, colour="E[Z]"))

# ex1_6_7
t <- 1:100
e <- 100 + sin(2*pi*t/12) + cos(2*pi*t/12) + 0.8*sin(2*pi*t/6) + 0.7*cos(2*pi*t/6)
z <- 100 + sin(2*pi*t/12) + cos(2*pi*t/12) + 0.8*sin(2*pi*t/6) + 0.7*cos(2*pi*t/6) + rnorm(100, 0, 5)
df <- data.frame(t, e, z)
ggplot(data=df, aes(t)) + geom_line(aes(y=z, colour="Z")) + geom_line(aes(y=e, colour="E[Z]"))

# ex1_6_8
t <- 1:100
e <- rep(250, 100)
z <- 250 + rnorm(1)
for(i in 2:100) {
  z[i] <- 100 + 0.6*z[i-1] + rnorm(1)
}
df <- data.frame(t, e, z)
ggplot(data=df, aes(t)) + geom_line(aes(y=z, colour="Z")) + geom_line(aes(y=e, colour="E[Z]"))

# ex1_6_9
t <- 1:100
e <- rep(100/1.2, 100)
z <- 50 + rnorm(1)
z[2] <- 100 + 0.5*z[1] + rnorm(1)
for(i in 3:100) {
  z[i] <- 100 + 0.5*z[i-1] - 0.7*z[i-2] + rnorm(1)
}
df <- data.frame(t, e, z)
ggplot(data=df, aes(t)) + geom_line(aes(y=z, colour="Z")) + geom_line(aes(y=e, colour="E[Z]"))

# ex1_6_10
t <- 1:100
eps <- rnorm(101)
e <- rep(100,100)
z <- 100 + eps[2:101] + 0.8*eps[1:100]
df <- data.frame(t, e, z)
ggplot(data=df, aes(t)) + geom_line(aes(y=z, colour="Z")) + geom_line(aes(y=e, colour="E[Z]"))

# ex1_6_11
t <- 1:100
eps <- rnorm(101)
e <- rep(100,100)
z <- 100 + eps[2:101] - 0.8*eps[1:100]
df <- data.frame(t, e, z)
ggplot(data=df, aes(t)) + geom_line(aes(y=z, colour="Z")) + geom_line(aes(y=e, colour="E[Z]"))

# ex1_7_1
data <- read.csv('../timedata/female.txt', sep='', header=FALSE)
z <- na.omit(c(t(data)))
t <- 1:length(z)
df <- data.frame(t, z)
ggplot(data = df, aes(t, z)) + 
  geom_line(color="darkblue") +
  xlab("months") +
  ylab("female workers(unit: 100thou)")

# ex1_7_2
data <- read.csv('../timedata/build.txt', sep='', header=FALSE)
z <- na.omit(c(t(data)))
t <- 1:length(z)
df <- data.frame(t, z)
ggplot(data = df, aes(t, z)) + 
  geom_line(color="darkblue") +
  xlab("months") +
  ylab("pemission")

# ex1_7_3
data <- read.csv('../timedata/export.txt', sep='', header=FALSE)
z <- na.omit(c(t(data)))
t <- 1:length(z)
df <- data.frame(t, z)
ggplot(data = df, aes(t, z)) + 
  geom_line(color="darkblue") +
  xlab("months") +
  ylab("sales(unit: $100m)")

# ex1_7_4
data <- read.csv('../timedata/usapass.txt', sep='', header=FALSE)
z <- na.omit(c(t(data)))
t <- 1:length(z)
df <- data.frame(t, z)
ggplot(data = df, aes(t, z)) + 
  geom_line(color="darkblue") +
  xlab("months") +
  ylab("flight customers(unit: 1thou)")
