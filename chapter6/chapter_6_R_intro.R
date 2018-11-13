library(ggplot2)

#figure 6.1
t <- 1:100
a <- rnorm(100)
z <- rep(0, 100)
y <- rep(0, 100)
for(i in 1:99) {
  z[i+1] <- -0.5*z[i] + a[i];
  y[i+1] <- 0.5*y[i] + a[i];
}

df <- data.frame(t,z,y)

ggplot(aes(x=t), data=df) + 
  geom_line(aes(y=y)) + 
  scale_x_continuous(breaks = seq(0, 100, by = 20)) +
  geom_hline(yintercept=0)

ggplot(aes(x=t), data=df) + 
  geom_line(aes(y=z)) + 
  scale_x_continuous(breaks = seq(0, 100, by = 20)) +
  geom_hline(yintercept=0)


#figure 6.6
t <- 1:100
a1 <- 0
z <- rep(0, 100)
y <- rep(0, 100)
for(i in 1:100) {
  a <- rnorm(1)
  z[i] <- a-0.6*a1
  y[i] <- a+0.6*a1
  a1 <- a
}

df <- data.frame(t,z,y)

ggplot(aes(x=t), data=df) + 
  geom_line(aes(y=y)) + 
  scale_x_continuous(breaks = seq(0, 100, by = 20)) +
  geom_hline(yintercept=0, color="grey10")

ggplot(aes(x=t), data=df) + 
  geom_line(aes(y=z)) + 
  scale_x_continuous(breaks = seq(0, 100, by = 20)) +
  geom_hline(yintercept=0, color="grey10")

#figure 6.4
testplot <- function(df, mode='acf'){
  
  if(mode=='acf') {
    if(!is.null(df$acf)) {
      ylegend <- "ACF"
      gplt <- ggplot(data=df, aes(x=lag, y=acf))
    }
  }
  else if(mode=='pacf') {
    if(!is.null(df$pacf)) {
      ylegend <- "PACF"
      gplt <- ggplot(data=df, aes(x=lag, y=pacf))
    }
  }
  else {
    stop('Given data frame must have acf or pacf')
  }
 
  gplt + 
    geom_hline(yintercept=0) +
    geom_hline(yintercept=0.5, color="blue", linetype="dashed") + 
    geom_hline(yintercept=-0.5, color="blue", linetype="dashed") + 
    geom_segment(aes(xend=df$lag, yend=0), size=2, color="darkgreen") + 
    xlab("Lag") +
    ylab(ylegend) + 
    scale_x_continuous(breaks = seq(0, length(df$lag), by = 1)) +
    scale_y_continuous(limits=c(-1,1), breaks = seq(-1, 1, by=0.2))
}

fig6_4 <- function(phi, mode='acf') {
  k <- 0:10
  acf <- phi**k
  pacf <- rep(0, 11)
  pacf[2] <- phi
  df <- data.frame(acf, pacf, lag=k)
  testplot(df, mode)
}

fig6_4(0.6, 'acf')
fig6_4(0.6, 'pacf')
fig6_4(-0.6, 'acf')
fig6_4(-0.6, 'pacf')

#figure 6.5
fig6_5 <- function(phi1, phi2, mode='acf') {
  k <- 0:10
  acf <- rep(0, 11)
  pacf <- rep(0, 11)
  acf[1] <- 1
  acf[2] <- phi1/(1-phi2)
  acf[3] <- (phi1**2+phi2-phi2**2)/(1-phi2)
  pacf[2] <- acf[2]
  pacf[3] <- phi2
  for(i in 3:10) {
    acf[i+1] <- phi1*acf[i]+phi2*acf[i-1]
  }
  df <- data.frame(acf, pacf, lag=k)
  testplot(df, mode)
}

fig6_5(-0.8, -0.7, 'acf')
fig6_5(-0.8, -0.7, 'pacf')
fig6_5(-0.5, 0.2, 'acf')
fig6_5(-0.5, 0.2, 'pacf')
fig6_5(0.7, 0.2, 'acf')
fig6_5(0.7, 0.2, 'pacf')
fig6_5(1.3, -0.5, 'acf')
fig6_5(1.3, -0.5, 'pacf')

#figure 6.8
fig6_8 <- function(theta, mode='acf') {
  k <- 0:10
  kk <- 2*(k+1)
  acf <- rep(0, 11)
  pacf <- rep(0, 11)
  acf[1] <- 1
  acf[2] <- -theta/(1+theta**2)
  pacf <- -(theta**k)*(1-theta**2)/(1-theta**kk)
  
  df <- data.frame(acf, pacf, lag=k)
  testplot(df, mode)
}

fig6_8(0.9, 'acf')
fig6_8(0.9, 'pacf')
fig6_8(-0.6, 'acf')
fig6_8(-0.6, 'pacf')

#figure 6.9
fig6_9 <- function(theta1, theta2, mode) {
  k <- 0:10
  acf <- rep(0, 11)
  pacf <- rep(0, 11)
  acf[1] <- 1
  acf[2] <- -theta1*(1-theta2)/(1+theta1**2+theta2**2)
  acf[3] <- -theta2/(1+theta1**2+theta2**2)
  
  df <- data.frame(acf, lag=k)
  testplot(df, mode)
}

fig6_9(-1.6, -0.7, 'acf')
fig6_9(0.8, -0.7, 'acf')

#figure 6.10
fig6_10 <- function(phi, theta, mode) {
  k <- 0:10
  acf <- rep(0, 11)
  pacf <- rep(0, 11)
  acf[1] <- 1
  acf[2] <- (phi-theta)*(1-phi*theta)/(1+theta**2-2*phi*theta)
  for(i in 2:10) {
    acf[i+1] <- phi*acf[i]
  }
  df <- data.frame(acf, lag=k)
  testplot(df, mode)
}

fig6_10(-0.9, -0.5, 'acf')
fig6_10(0.8, -0.7, 'acf')
