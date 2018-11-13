library(ggplot2)

#example 5.1
t <- 1:100
z <- 15
for(i in 2:101){
  z[i] <- 17 - 0.7*z[i-1] + rnorm(1)
}
z1 <- z[1:100]
z <- z[2:101]
z2 <- c(NA, NA, z[1:98])

df <- data.frame(t, z, z1, z2)

ggplot(data=df, aes(x=t)) + 
  geom_line(aes(y=z)) + 
  geom_hline(yintercept=10, color="grey50") + 
  scale_x_continuous(breaks = seq(0, 100, by = 10))

ggplot(data=df, aes(x=z)) + 
  geom_point(aes(y=z1)) +
  geom_hline(yintercept=10, color="grey50") + 
  geom_vline(xintercept=10, color="grey50")
  
ggplot(data=df, aes(x=z)) + 
  geom_point(aes(y=z2)) +
  geom_hline(yintercept=10, color="grey50") + 
  geom_vline(xintercept=10, color="grey50")

#figure 5.2
t <- 1:300
z <- rnorm(300)
df <- data.frame(t, z)
ggplot(data=df, aes(x=t, y=z)) + geom_line()

#figure 5.3
t <- 1:300
z <- 0
for(i in 2:301){
  z[i] <- z[i-1] + rnorm(1)
}
z <- z[2:301]
df <- data.frame(t, z)
ggplot(data=df, aes(x=t, y=z)) + geom_line()

#figure 5.4
t <- 1:300
z <- 0
for(i in 2:301){
  z[i] <- 0.2 + z[i-1] + rnorm(1)
}
z <- z[2:301]
df <- data.frame(t, z)
ggplot(data=df, aes(x=t, y=z)) + geom_line()

#example 5.2
t <- 1:100
a1 <- 0
z <- 0
for(i in 1:100){
  a <- rnorm(1)
  z[i] <- a + 0.8 * a1
  a1 <- a
}
z1 <- c(NA, z[1:99])
z2 <- c(NA, NA, z[1:98])
z4 <- z - 0.4991*z1
z3 <- (z2 - 0.4991*z1)
df <- data.frame(t, z, z3, z4)

ggplot(data=df, aes(x=t, y=z)) + 
  geom_line() + 
  geom_hline(yintercept=0, color="grey50") + 
  scale_x_continuous(breaks = seq(0, 100, by = 20))

ggplot(data=df, aes(x=z3, y=z4)) + 
  geom_point() +
  geom_hline(yintercept=0, color="grey50") +
  geom_vline(xintercept=0, color="grey50")
