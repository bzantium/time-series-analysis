library(forecast)
library(ggplot2)

#exercise 5.2
z <- c(7,6,5,8,9,4,5,5,4,6,7,8,5,6,5)
acfz_ <- acf(z, pl=F)
print(acfz_)
autoplot(acfz_)
pacfz_ <- pacf(z, pl=F)
print(pacfz_)
autoplot(pacfz_)

#custom function
ACF <- function(z, nlag=11){
  lenz <- length(z)
  zbar <- mean(z)
  gamma0 <- mean((z-zbar)**2)
  acf_ <- 0
  for(t in 1:nlag){
    gammak <- sum((z[1:(lenz-t)]-zbar)*(z[(1+t):lenz]-zbar))/lenz
    acf_[t] <- gammak/gamma0
  }
  lag <- 1:nlag
  return(data.frame(acf=acf_, lag=lag))
}

PACF <- function(z, nlag=11){
  lenz <- length(z)
  pacfMat <- matrix(rep(0,nlag**2), nrow=nlag, ncol=nlag)
  zbar <- mean(z)
  gamma0 <- mean((z-zbar)**2)
  gamma1 <- sum((z[1:(lenz-1)]-zbar)*(z[2:lenz]-zbar))/lenz
  acf_ <- gamma1/gamma0
  pacfMat[1,1] <- acf_[1]
  for(t in 2:nlag){
    gammak <- sum((z[1:(lenz-t)]-zbar)*(z[(1+t):lenz]-zbar))/lenz
    acf_[t] <- gammak/gamma0
    pacfMat[t, t] <- (acf_[t] - sum(pacfMat[t-1,1:(t-1)]*acf_[(t-1):1]))/(1-sum(pacfMat[t-1,1:(t-1)]*acf_[1:(t-1)]))
    for(k in 1:(t-1)){
      pacfMat[t,k] <- pacfMat[t-1,k] - pacfMat[t, t]*pacfMat[t-1,t-k]
    }
  }
  lag <- 1:nlag
  return(data.frame(pacf=diag(pacfMat), lag=lag))
}

testplot <- function(df){
  
  if(!is.null(df$acf)) {
    ylegend <- "ACF"
    gplt <- ggplot(data=df, aes(x=lag, y=acf))
  }
  else if(!is.null(df$pacf)) {
    ylegend <- "PACF"
    gplt <- ggplot(data=df, aes(x=lag, y=pacf))
  }
  else {
    stop('Given data frame must have acf or pacf')
  }
  
  gplt + 
    geom_hline(yintercept=0) +
    geom_hline(yintercept=0.5, color="blue", linetype="dashed") + 
    geom_hline(yintercept=-0.5, color="blue", linetype="dashed") + 
    geom_segment(aes(xend=lag, yend=0)) + 
    xlab("Lag") +
    ylab(ylegend) + 
    scale_x_continuous(breaks = seq(1, length(df$lag), by = 1))
}

acfz <- ACF(z)
print(acfz)
testplot(acfz)

pacfz <- PACF(z)
print(pacfz)
testplot(pacfz)

#exercise 5.3
data <- read.csv('../timedata/ex5_3.txt', sep='', header=FALSE)
z <- na.omit(c(t(data)))
lenz <- length(z)
t <- 1:lenz
df <- data.frame(t, z)
ggplot(data=df, aes(x=t)) + geom_line(aes(y=z))
z1 <- z[1:(lenz-1)]
z2 <- z[1:(lenz-2)]

df2 <- data.frame(z=z[2:lenz], z1=z1)
ggplot(data=df2, aes(x=z)) + geom_point(aes(y=z1))
regz1 <- lm(z~z1, data=df2)
regz1$coefficients["z1"]
acfz <- ACF(z)
acfz$acf[1]
testplot(acfz)

df3 <- data.frame(z=z[3:lenz], z2=z2)
ggplot(data=df3, aes(x=z)) + geom_point(aes(y=z2))
regz2 <- lm(z~z2, data=df3)
regz2$coefficients["z2"]
acfz$acf[2]
pacfz <- PACF(z)
testplot(pacfz)