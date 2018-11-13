library(lubridate)
library(ggplot2)
library(car)
library(forecast)

# exercise 2.2
z <- c(303,298,303,314,303,314,310,324,317,326,323,324,331,330,332)
t <- 1:15
df <- data.frame(t, z)
reg <- lm(z~t, data=df)
summary(reg)
res <- data.frame(t, reg$residuals)
colnames(res) <- c('t', 'residual')
ggplot(data=res, aes(x=t,y=residual)) + 
  geom_line() +
  geom_point()

new_t <- 1:20
new <- data.frame(t=new_t)
df <- data.frame(predict(reg, new, interval="prediction", level=0.95))
df['t'] <- new_t
df['z'] <- c(z, rep('NA', 5))
print(df)
ggplot(data=df, aes(x=t)) +
  geom_line(aes(y=fit, colour='fit')) +
  geom_line(aes(y=lwr, colour='lwr')) +
  geom_line(aes(y=upr, colour='upr')) +
  geom_line(aes(x=t,y=as.numeric(z), colour='z')) +
  geom_point(aes(y=fit, colour='fit'), shape='p') +
  geom_point(aes(y=lwr, colour='lwr'), shape='L') +
  geom_point(aes(y=upr, colour='upr'), shape='U') +
  geom_point(aes(x=t,y=as.numeric(z), colour='z'), shape=20) +
  theme(legend.position = c(0.15, 0.96), 
        legend.background=element_rect(fill="transparent"), 
        legend.title=element_blank(),
        legend.direction = "horizontal")

# exercise 2.9
data <- read.csv('../timedata/catv.txt', sep='', header=FALSE)
z <- na.omit(c(t(data)))
t <- 1:length(z)
df <- data.frame(t, z)
reg <- lm(z~t, data=df)
summary(reg)

new_t <- 1:(length(z)+12)
new <- data.frame(t=new_t)
df <- data.frame(predict(reg, new, interval="prediction", level=0.95))
df['t'] <- new_t
df['z'] <- c(z, rep('NA', 12))
print(df)
ggplot(data=df, aes(x=t)) +
  geom_line(aes(y=fit, colour='fit')) +
  geom_line(aes(y=lwr, colour='lwr')) +
  geom_line(aes(y=upr, colour='upr')) +
  geom_line(aes(x=t,y=as.numeric(z), colour='z')) +
  geom_point(aes(y=fit, colour='fit'), shape='p') +
  geom_point(aes(y=lwr, colour='lwr'), shape='L') +
  geom_point(aes(y=upr, colour='upr'), shape='U') +
  geom_point(aes(x=t,y=as.numeric(z), colour='z'), shape=20) +
  theme(legend.position = c(0.15, 0.96), 
        legend.background=element_rect(fill="transparent"), 
        legend.title=element_blank(),
        legend.direction = "horizontal")