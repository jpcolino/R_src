library("gdata")
library("gtools")
library("sde")
library("stats4")
library("ggplot2")
library("stats4")
library ("tseries")
library("plyr")


rm(list = ls()) # Clean the workspace 

# Reading and Loading the data

setwd("//Users/JPC/Google Drive/Gas Oil Spread Trade/") # Set here your working directory

prices10 <- read.csv("gasoil10.csv", header = TRUE, sep = ",", quote="\"", dec=".")
prices10 <- prices10[complete.cases(prices10),]
prices10 <- prices10[1:dim(prices10)[1],]

prices11 <- read.csv("gasoil11.csv", header = TRUE, sep = ",", quote="\"", dec=".")
prices11 <- prices11[complete.cases(prices11),]
prices11 <- prices11[1:dim(prices11)[1],]

prices12 <- read.csv("gasoil12.csv", header = TRUE, sep = ",", quote="\"", dec=".")
prices12 <- prices12[complete.cases(prices12),]
prices12 <- prices12[1:dim(prices12)[1],]

prices13 <- read.csv("gasoil13.csv", header = TRUE, sep = ",", quote="\"", dec=".")
prices13 <- prices13[complete.cases(prices13),]
prices13 <- prices13[1:dim(prices13)[1],]


# Auxiliary functions for model calculation

dcOU <- function (x,t,x0,theta,log=FALSE)
{
  Ex <- theta[1] / theta[2] + ( x0 - theta[1] / theta[2]) * exp ( -theta[2] * t )
  Vx <- theta[3]^2 * ( 1 - exp ( -2 * theta[2] * t) ) / ( 2 * theta[2] )
  dnorm(x, mean=Ex, sd=sqrt(Vx), log=log)
}

OU.lik <- function(theta1, theta2, theta3)
{
  n <-length(X)
  dt <- deltat(X)
  -sum(dcOU( X[2:n], dt, X[1:(n-1)], c(theta1, theta2, theta3), log=TRUE))
}


# Regresion for the whole period: we do not trade the trend

spread10 <- 10*182*24*prices10[,2]-10000*prices10[,4] # Gas_Euros - Oil_Euros
spread11 <- 10*182*24*prices11[,2]-10000*prices11[,4] 
spread12 <- 10*182*24*prices12[,2]-10000*prices12[,4]
spread13 <- 10*182*24*prices13[,2]-10000*prices13[,3]/prices13[,4]

spread   <- c(spread10,spread11, spread12, spread13)

par(mfrow=c(3,1))
plot.ts(c(prices10[,2], prices11[,2], prices12[,2], prices13[,2]), main = 'Price evolution TTF Winter 2010 - 2013')
plot.ts(c(prices10[,3], prices11[,3], prices12[,3], prices13[,3]), main = 'Price evolution Oil Euros 2010 - 2013')
plot.ts(c(spread10, spread11, spread12, spread13), main = 'Spread 10k barrels vs 10Mw in Euros 2010 - 2013')


# --------------------------------------------------------------------
# ggplot version
# --------------------------------------------------------------------

x10 <- as.Date(prices10$X,format='%d/%m/%Y')
df_prices10 <- data.frame(x10,prices10$TTF.Winter10)
ggplot(df_prices10, aes(x10,prices10$TTF.Winter10))+geom_line(colour="darkgreen", size=1)
ggplot(df_prices10, aes(x10,prices10$TTF.Winter10))+geom_area()

x11 <- as.Date(prices11$X,format='%d/%m/%Y')
df_prices11 <- data.frame(x11,prices11$TTF.Winter11)
ggplot(df_prices11, aes(x11,prices11$TTF.Winter11))+geom_line(colour="darkgreen", size=1)

x12 <- as.Date(prices12$X,format='%d/%m/%Y')
df_prices12 <- data.frame(x12,prices12$TTF.Winter12)
ggplot(df_prices12, aes(x12,prices12$TTF.Winter12))+geom_line(colour="darkgreen", size=1)

x13 <- as.Date(prices13$Timestamp,format='%d/%m/%Y')
df_prices13 <- data.frame(x13,prices13$TTF.Winter13)
ggplot(df_prices13, aes(x13,prices13$TTF.Winter13))+geom_line(colour="darkgreen", size=1)

# --------------------------------------------------------------------
m <-min(length(prices10$X),length(prices11$X),length(prices12$X),length(prices13$Timestamp))
df_prices <- data.frame(x13,prices10$TTF.Winter10[1:m],prices11$TTF.Winter11[1:m],prices12$TTF.Winter12[1:m],prices13$TTF.Winter13[1:m])
names(df_prices)<- c('x','w10','w11','w12','w13')

ggplot(df_prices, aes(x))+geom_line(aes(y = w13),colour='red',size=1)+
  geom_line(aes(y = w12),colour='blue',size=1)+
  geom_line(aes(y = w11),colour='green',size=1)



# --------------------------------------------------------------------

tt <- rep(1:length(spread))
reg <- lm(spread ~ tt)
spread1 <- spread - reg$coef[1] - reg$coef[2]*rep(1:length(spread))

par(mfrow=c(2,1))
plot.ts(spread, main='Spread and Total Drift')
abline(reg$coef[1],reg$coef[2], lwd=3, col='red')
plot.ts(spread1, main='Spread with no-drift (global)')


par(mfrow=c(3,2))
t10 <- rep(1:length(spread10))
reg <- lm(spread10 ~ t10)
spread101 <- spread10 - reg$coef[1] - reg$coef[2]*rep(1:length(spread10))
plot.ts(spread10, main = 'Original Spread 2010')
abline(reg$coef[1],reg$coef[2], lwd=3, col='red')
plot.ts(spread101, main = 'Spread 2010 with no drift')

t11 <- rep(1:length(spread11))
reg <- lm(spread11 ~ t11)
spread111 <- spread11 - reg$coef[1] - reg$coef[2]*rep(1:length(spread11))
plot.ts(spread11, main = 'Original Spread 2011')
abline(reg$coef[1],reg$coef[2], lwd=3, col='red')
plot.ts(spread111, main = 'Spread 2011 with no drift')

t12 <- rep(1:length(spread12))
reg <- lm(spread12 ~ t12)
spread121 <- spread12 - reg$coef[1] - reg$coef[2]*rep(1:length(spread12))
plot.ts(spread12, main = 'Original Spread 2012')
abline(reg$coef[1],reg$coef[2], lwd=3, col='red')
plot.ts(spread121, main = 'Spread 2012 with no drift')

t13 <- rep(1:length(spread13))
reg <- lm(spread13 ~ t13)
spread131 <- spread13 - reg$coef[1] - reg$coef[2]*rep(1:length(spread13))
plot.ts(spread13, main = 'Original Spread 2013')
abline(reg$coef[1],reg$coef[2], lwd=3, col='red')
plot.ts(spread131, main = 'Spread 2013 with no drift')

spread21   <- c(spread101,spread111, spread121, spread131)

par(mfrow=c(3,1))
ts.plot(as.ts(spread), main = 'Original Spreads')
ts.plot(as.ts(spread1), main ='Spreads with no-drift (global)')
ts.plot(as.ts(spread21), main = 'Spreads with no-drift (local)')



# ---------------------------------------------------------------------
# 2010 Spread Mean-Reverting Model (training in-the-sample) to trade in 2011 
# ---------------------------------------------------------------------

print("-------------------------------------------")
print("Results for 2010")
print("-------------------------------------------")

X <- (spread101) # <- We are using the 2010 Spread with NO DRIFT (local regression)
t <- rep(1:length(X))
print(jarque.bera.test(diff(X)))

mle(OU.lik, start = list(theta1=1, theta2=0.5, theta3=1), method = "L-BFGS-B") -> fit10

print(summary(fit10))

# ---------------------------------------------------------------------
# Trading rules for 2011 based in the 2010 spread model (out-of-sample)
# ---------------------------------------------------------------------

# if spread is too low:   BUY SPREAD  = long in oil   + short in Gas
# if spread is too high:  SELL SPREAD = short in oil  + long in Gas

# spread11 <- prices11[,4]-prices11[,2]

t <- rep(1:length(spread111))

predict  <- rep(spread111[1], times=length(spread111))

mean_10  <- spread101[1]* exp(-coef(fit10)[2]*t) + (coef(fit10)[1] / coef(fit10)[2]) * (1-exp(-coef(fit10)[2]*t))

var_10   <- (coef(fit10)[3]^2 / (2*coef(fit10)[2])) * (1-exp(-2*coef(fit10)[2]*t))
var_10   <- sqrt(var_10)

std = 0.5

par(mfrow=c(2,1))
ts.plot(as.ts(spread10), main='Actual Spread 2010')
ts.plot( as.ts(spread101[1:length(spread111)]), as.ts(mean_10), as.ts(mean_10 + 1 * var_10), as.ts(mean_10 - 1 * var_10), 
         main='2010 Spread without Local Drift and 2 sigma-IC')

mean_10  <- spread111[1]* exp(-coef(fit10)[2]*t) + (coef(fit10)[1] / coef(fit10)[2]) * (1-exp(-coef(fit10)[2]*t))

par(mfrow=c(2,1))
ts.plot(as.ts(spread11), main='Actual Spread 2011')
ts.plot( as.ts(spread111), as.ts(mean_10), as.ts(mean_10 + std * var_10), as.ts(mean_10 - std * var_10), 
         main='2011 Spread without Local Drift')


position11_f    <- rep (0, times=length(spread111) )
position11_m2m  <- rep (0, times=length(spread111) )
results11       <- rep (0, times=length(spread111) )

# account11  <- rep (0, times=length(spread11) )

for (i in 2:length(spread111)) {
  
  predict[i] <- coef(fit10)[1] / coef(fit10)[2] + ( spread111[i-1] -
                                                      coef(fit10)[1] / coef(fit10)[2]) * exp ( -coef(fit10)[2])
  
  # OPENING POSITIONS ------------------------------------------------------------
  
  # if spread is too high:  SELL SPREAD = short in oil  + long in Gas
  
  if  (position11_f[i-1] == 0 && spread111[i] > ( mean_10[i] + std * var_10[i] )) 
  {
    position11_f[i]    = spread11[i]
    position11_m2m[i]  = position11_m2m[i-1] 
  }
  
  # if spread is too low:   BUY SPREAD  = long in oil   + short in Gas
  
  if  (position11_f[i-1] == 0 && spread111[i] < ( mean_10[i] - std * var_10[i] )) 
  {
    position11_f[i]    = - spread11[i]
    position11_m2m[i]  = position11_m2m[i-1] 
  }
  
  # WAITING THE POSITIONS ------------------------------------------------------------  
  
  if  (position11_f[i-1] > 0 && (spread111[i] > ( mean_10[i] - std * var_10[i] )))  
  {
    position11_f[i] = position11_f[i-1]
    position11_m2m[i] = position11_m2m[i-1] + (- spread11[i] + spread11[i-1])
  }
  
  if  (position11_f[i-1] < 0 && (spread111[i] < ( mean_10[i] + std * var_10[i] )))  
  {
    position11_f[i] = position11_f[i-1]
    position11_m2m[i] = position11_m2m[i-1] + ( spread11[i] - spread11[i-1])
  }
  
  # CLOSING POSITIONS ------------------------------------------------------------
  
  # Closing short position
  
  if  (position11_f[i-1] > 0 && (spread111[i] < ( mean_10[i] - std * var_10[i] )))  
  {
    results11[i] = position11_f[i-1] - spread11[i]
    position11_f[i] = -spread11[i]
    position11_m2m[i] = 0
  }
  
  # Closing long position
  
  if  (position11_f[i-1] < 0 && (spread111[i] > ( mean_10[i] + std * var_10[i] )))  
  {
    results11[i] = spread11[i] + position11_f[i-1] 
    position11_f[i] = spread11[i]
    position11_m2m[i] = 0
  }
}

print(results11)

# ---------------------------------------------------------------------  
# 2011 Spread Mean-Reverting Model 
# ---------------------------------------------------------------------

print("-------------------------------------------")
print("Results for 2011")
print("-------------------------------------------")

X <- (spread111)
t <- rep(1:length(X))
print(jarque.bera.test(diff(X)))
mle(OU.lik, start = list(theta1=1, theta2=0.5, theta3=1), method = "L-BFGS-B") -> fit11

# ---------------------------------------------------------------------
# Trading rules for 2012 based in the 2011 spread model (out-of-sample)
# ---------------------------------------------------------------------

# if spread is too low:   BUY SPREAD  = long in oil   + short in Gas
# if spread is too high:  SELL SPREAD = short in oil  + long in Gas

# spread12 <- prices12[,4]-prices12[,2]

predict  <- rep(spread12[1], times=length(spread12))

t <- rep(1:length(spread12))

mean_11  <- spread111[1]* exp(-coef(fit11)[2]*t) + (coef(fit11)[1] / coef(fit11)[2]) * (1-exp(-coef(fit11)[2]*t))

var_11   <- (coef(fit11)[3]^2 / (2*coef(fit11)[2])) * (1-exp(-2*coef(fit11)[2]*t))
var_11   <- sqrt(var_11)

par(mfrow=c(2,1))
ts.plot(as.ts(spread11), main='Actual Spread 2011')
ts.plot( as.ts(spread111), as.ts(mean_11), as.ts(mean_11 + 1 * var_11), as.ts(mean_11 - 1 * var_11), 
         main='2011 Spread without Local Drift and 2sigma-IC')

mean_11  <- spread121[1]* exp(-coef(fit11)[2]*t) + (coef(fit11)[1] / coef(fit11)[2]) * (1-exp(-coef(fit11)[2]*t))

par(mfrow=c(2,1))
ts.plot(as.ts(spread12), main='Actual Spread 2012')
ts.plot( as.ts(spread121), as.ts(mean_11), as.ts(mean_11 + std * var_11), as.ts(mean_11 - std * var_11), 
         main='2012 Spread without Local Drift')

position12_f    <- rep (0, times=length(spread12) )
position12_m2m  <- rep (0, times=length(spread12) )
results12       <- rep (0, times=length(spread12) )

# account11  <- rep (0, times=length(spread11) )

for (i in 2:length(spread12)) {
  
  predict[i] <- coef(fit11)[1] / coef(fit11)[2] + ( spread12[i-1] -
                                                      coef(fit11)[1] / coef(fit11)[2]) * exp ( -coef(fit11)[2])
  
  # OPENING POSITIONS ------------------------------------------------------------
  
  # if spread is too high:  SELL SPREAD = short in oil  + long in Gas
  
  if  (position12_f[i-1] == 0 && spread121[i] > ( mean_11[i] + std * var_11[i] )) 
  {
    position12_f[i]    = spread12[i]
    position12_m2m[i]  = position12_m2m[i-1] 
  }
  
  # if spread is too low:   BUY SPREAD  = long in oil   + short in Gas
  
  if  (position12_f[i-1] == 0 && spread121[i] < ( mean_11[i] - std * var_11[i] )) 
  {
    position12_f[i]    = - spread12[i]
    position12_m2m[i]  = position12_m2m[i-1] 
  }
  
  # WAITING THE POSITIONS ------------------------------------------------------------  
  
  if  (position12_f[i-1] > 0 && (spread121[i] > ( mean_11[i] - std * var_11[i] )))  
  {
    position12_f[i] = position12_f[i-1]
    position12_m2m[i] = position12_m2m[i-1] + (- spread12[i] + spread12[i-1])
  }
  
  if  (position12_f[i-1] < 0 && (spread121[i] < ( mean_11[i] + std * var_11[i] )))  
  {
    position12_f[i] = position12_f[i-1]
    position12_m2m[i] = position12_m2m[i-1] + ( spread12[i] - spread12[i-1])
  }
  
  # CLOSING POSITIONS ------------------------------------------------------------
  
  # Closing short position
  
  if  (position12_f[i-1] > 0 && (spread121[i] < ( mean_11[i] - std * var_11[i] )))  
  {
    results12[i]    = position12_f[i-1] - spread12[i]
    position12_f[i] = -spread12[i]
    position12_m2m[i] = 0
  }
  
  # Closing long position
  
  if  (position12_f[i-1] < 0 && (spread121[i] > ( mean_11[i] + std * var_11[i] )))  
  {
    results12[i]    = spread12[i] + position12_f[i-1] 
    position12_f[i] = spread12[i]
    position12_m2m[i] = 0
  }
}

print(results12)


print(summary(fit11))

##################################################


# ---------------------------------------------------------------------  
# 2012 Spread Mean-Reverting Model 
# ---------------------------------------------------------------------

print("-------------------------------------------")
print("Results for 2012")
print("-------------------------------------------")

X <- (spread121)
t <- rep(1:length(X))
print(jarque.bera.test(diff(X)))
mle(OU.lik, start = list(theta1=1, theta2=0.5, theta3=1), method = "L-BFGS-B") -> fit12

# ---------------------------------------------------------------------
# Trading rules for 2013 based in the 2012 spread model (out-of-sample)
# ---------------------------------------------------------------------

# if spread is too low:   BUY SPREAD  = long in oil   + short in Gas
# if spread is too high:  SELL SPREAD = short in oil  + long in Gas

# spread12 <- prices12[,4]-prices12[,2]

predict  <- rep(spread13[1], times=length(spread13))

t <- rep(1:length(spread13))

mean_12  <- spread121[1]* exp(-coef(fit12)[2]*t) + (coef(fit12)[1] / coef(fit12)[2]) * (1-exp(-coef(fit12)[2]*t))

var_12   <- (coef(fit12)[3]^2 / (2*coef(fit12)[2])) * (1-exp(-2*coef(fit12)[2]*t))
var_12   <- sqrt(var_12)

par(mfrow=c(2,1))
ts.plot(as.ts(spread12), main='Actual Spread 2012')
ts.plot( as.ts(spread121), as.ts(mean_12), as.ts(mean_12 + 1 * var_12), as.ts(mean_12 - 1 * var_12), 
         main='2012 Spread without Local Drift and 2sigma-IC')

mean_12  <- spread131[1]* exp(-coef(fit12)[2]*t) + (coef(fit12)[1] / coef(fit12)[2]) * (1-exp(-coef(fit12)[2]*t))

par(mfrow=c(2,1))
ts.plot(as.ts(spread13), main='Actual Spread 2013')
ts.plot( as.ts(spread131), as.ts(mean_12), as.ts(mean_12 + std * var_12), as.ts(mean_12 - std * var_12), 
         main='2013 Spread without Local Drift')

position13_f    <- rep (0, times=length(spread13) )
position13_m2m  <- rep (0, times=length(spread13) )
results13       <- rep (0, times=length(spread13) )

# account11  <- rep (0, times=length(spread11) )

for (i in 2:length(spread13)) {
  
  predict[i] <- coef(fit12)[1] / coef(fit12)[2] + ( spread13[i-1] -
                                                      coef(fit12)[1] / coef(fit12)[2]) * exp ( -coef(fit12)[2])
  
  # OPENING POSITIONS ------------------------------------------------------------
  
  # if spread is too high:  SELL SPREAD = short in oil  + long in Gas
  
  if  (position12_f[i-1] == 0 && spread131[i] > ( mean_12[i] + std * var_12[i] )) 
  {
    position13_f[i]    = spread13[i]
    position13_m2m[i]  = position13_m2m[i-1] 
  }
  
  # if spread is too low:   BUY SPREAD  = long in oil   + short in Gas
  
  if  (position13_f[i-1] == 0 && spread131[i] < ( mean_12[i] - std * var_12[i] )) 
  {
    position13_f[i]    = - spread13[i]
    position13_m2m[i]  = position13_m2m[i-1] 
  }
  
  # WAITING THE POSITIONS ------------------------------------------------------------  
  
  if  (position13_f[i-1] > 0 && (spread131[i] > ( mean_12[i] - std * var_12[i] )))  
  {
    position13_f[i] = position13_f[i-1]
    position13_m2m[i] = position13_m2m[i-1] + (- spread13[i] + spread13[i-1])
  }
  
  if  (position13_f[i-1] < 0 && (spread131[i] < ( mean_12[i] + std * var_12[i] )))  
  {
    position13_f[i] = position13_f[i-1]
    position13_m2m[i] = position13_m2m[i-1] + ( spread13[i] - spread13[i-1])
  }
  
  # CLOSING POSITIONS ------------------------------------------------------------
  
  # Closing short position
  
  if  (position13_f[i-1] > 0 && (spread131[i] < ( mean_12[i] - std * var_12[i] )))  
  {
    results13[i]    = position13_f[i-1] - spread13[i]
    position13_f[i] = -spread13[i]
    position13_m2m[i] = 0
  }
  
  # Closing long position
  
  if  (position13_f[i-1] < 0 && (spread131[i] > ( mean_12[i] + std * var_12[i] )))  
  {
    results13[i]    = spread13[i] + position13_f[i-1] 
    position13_f[i] = spread13[i]
    position13_m2m[i] = 0
  }
}

print(results13)


print(summary(fit12))





##################################################
print("-------------------------------------------")
print("Results for 2013")
print("-------------------------------------------")
# spread13 <- prices13[,4]-prices13[,2]
X <- (spread131)
print(jarque.bera.test(diff(X)))
mle(OU.lik, start = list(theta1=1, theta2=0.5, theta3=1), method = "L-BFGS-B") -> fit13
print(summary(fit13))

par(mfrow=c(3,1))
#plot.ts(cumsum(results10))
plot.ts(cumsum(results11))
plot.ts(cumsum(results12))
plot.ts(cumsum(results13))

par(mfrow=c(3,1))
#plot.ts(position10_m2m)
plot.ts(position11_m2m, main = 'position11_m2m')
plot.ts(position12_m2m, main = 'position12_m2m')
plot.ts(position13_m2m, main = 'position13_m2m')

par(mfrow=c(3,1))
ts.plot(as.ts(spread11), main='Actual Spread 2011')
ts.plot( as.ts(spread111), as.ts(mean_10), as.ts(mean_10 + std * var_10), as.ts(mean_10 - std * var_10), 
         main='2011 Spread without Local Drift')
ts.plot(as.ts(cumsum(results11) + position11_m2m), as.ts(cumsum(results11)), main = 'P&L 2011')

pl11 <- as.ts(cumsum(results11) + position11_m2m)
pl12 <- as.ts(cumsum(results12) + position12_m2m)
pl13 <- as.ts(cumsum(results13) + position13_m2m)

par(mfrow=c(3,1))
ts.plot(as.ts(spread12), main='Actual Spread 2012')
ts.plot( as.ts(spread121), as.ts(mean_11), as.ts(mean_11 + std * var_11), as.ts(mean_11 - std * var_11), 
         main='2012 Spread without Local Drift')
ts.plot(as.ts(cumsum(results12) + position12_m2m), as.ts(cumsum(results12)), main = 'P&L 2012')

par(mfrow=c(3,1))
ts.plot(as.ts(spread13), main='Actual Spread 2013')
ts.plot( as.ts(spread131), as.ts(mean_12), as.ts(mean_12 + std * var_12), as.ts(mean_12 - std * var_12), 
         main='2013 Spread without Local Drift')
ts.plot(as.ts(cumsum(results13) + position13_m2m), as.ts(cumsum(results13)), main = 'P&L 2013')




par(mfrow=c(3,1))
ts.plot(as.ts(cumsum(results11) + position11_m2m), as.ts(cumsum(results11)), main = 'P&L 2011')
hist(diff(pl11),main = 'Histogram of daily variations of P&L 2011')
hist(results11[which(!results11==0)],main = 'Histogram of Trading Results 2011')

par(mfrow=c(3,1))
ts.plot(as.ts(cumsum(results12) + position12_m2m), as.ts(cumsum(results12)), main = 'P&L 2012')
hist(diff(pl12),main = 'Histogram of daily variations of P&L 2012')
hist(results12[which(!results12==0)],main = 'Histogram of Trading Results 2012')

par(mfrow=c(3,1))
ts.plot(as.ts(cumsum(results13) + position13_m2m), as.ts(cumsum(results13)), main = 'P&L 2013')
hist(diff(pl13),main = 'Histogram of daily variations of P&L 2013')
hist(results13[which(!results13==0)],main = 'Histogram of Trading Results 2013')


print("---------------------------------------------------")
print("--------Summary P&L year 2011 --------")
print(summary(pl11))
print("--------Summary P&L  year 2012 --------")
print(summary(pl12))
print("--------Summary P&L year 2013 --------")
print(summary(pl13))
print("---------------------------------------------------")

print("---------------------------------------------------")
print("--------Summary P&L Daily Variation (2011) --------")
print(summary(diff(pl11)))
print("--------Summary P&L Daily Variation (2012) --------")
print(summary(diff(pl12)))
print("--------Summary P&L Daily Variation (2013) --------")
print(summary(diff(pl13)))



print("---------------------------------------------------")

# par(mfrow=c(2,1))
# #plot.ts(position10_f) 
# plot.ts(position11_f)

# plot.ts(position12_f)