library("gdata")
library("gtools")
library("sde")
library("stats4")
library("ggplot2")
library("stats4")
library ("tseries")
library ("TTR")

rm(list = ls()) # Clean the workspace 

# Reading and Loading the data

setwd("C:/Users/J35041/Desktop/Gas Oil Spread Trade") # Set here your working directory

prices10 <- read.csv("gasoil10.csv", header = TRUE, sep = ",", quote="\"", dec=".")
prices10 <- prices10[complete.cases(prices10),]
prices10 <- prices10[42:dim(prices10)[1],]
   
prices11 <- read.csv("gasoil11.csv", header = TRUE, sep = ",", quote="\"", dec=".")
prices11 <- prices11[complete.cases(prices11),]
prices11 <- prices11[42:dim(prices11)[1],]
  
prices12 <- read.csv("gasoil12.csv", header = TRUE, sep = ",", quote="\"", dec=".")
prices12 <- prices12[complete.cases(prices12),]
prices12 <- prices12[42:dim(prices12)[1],]
  
# Some previous plotting
<<<<<<< HEAD
plot.ts(c(prices10[,2], prices11[,2], prices12[,2]))
plot.ts(c(prices10[,3], prices11[,3], prices12[,3]))
=======
par(mfrow=c(2,1))
plot.ts(c(prices10[,2], prices11[,2], prices12[,2]), main='TTF Winter')
plot.ts(c(prices10[,3], prices11[,3], prices12[,3]), main='Oil in Dollars')
>>>>>>> 9706122272dee6f15253ae52acb7e0c87a60afac

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


# Correlation Analysis

MovingCor10 <- running(as.numeric(prices10[,4]), as.numeric(prices10[,2]), cor)
MovingCor11 <- running(as.numeric(prices11[,4]), as.numeric(prices11[,2]), cor)
MovingCor12 <- running(as.numeric(prices12[,4]), as.numeric(prices12[,2]), cor)


# Regresion for the whole period: we do not trade the trend

spread10 <- prices10[,4] - prices10[,2] # Oil_Euros - Gas_Euros
spread11 <- prices11[,4] - prices11[,2] 
spread12 <- prices12[,4] - prices12[,2] 

<<<<<<< HEAD
SMovAvrg10 <- c(SMA(spread10,20))
SMovAvrg10 <- ifelse(is.na(SMovAvrg10), 0, SMovAvrg10)

SMovAvrg11 <- c(SMA(spread11,20))
SMovAvrg11 <- ifelse(is.na(SMovAvrg11), 0, SMovAvrg11)

SMovAvrg12 <- c(SMA(spread12,20))
=======
SMovAvrg10 <- c(SMA(spread10,30))
SMovAvrg10 <- ifelse(is.na(SMovAvrg10), 0, SMovAvrg10)

SMovAvrg11 <- c(SMA(spread11,30))
SMovAvrg11 <- ifelse(is.na(SMovAvrg11), 0, SMovAvrg11)

SMovAvrg12 <- c(SMA(spread12,30))
>>>>>>> 9706122272dee6f15253ae52acb7e0c87a60afac
SMovAvrg12 <- ifelse(is.na(SMovAvrg12), 0, SMovAvrg12)

EMovAvrg10 <- c(EMA(spread10,10))
EMovAvrg10 <- ifelse(is.na(EMovAvrg10), 0, EMovAvrg10) 

EMovAvrg11 <- c(EMA(spread11,10))
EMovAvrg11 <- ifelse(is.na(EMovAvrg11), 0, EMovAvrg11)

EMovAvrg12 <- c(EMA(spread12,10))                
EMovAvrg12 <- ifelse(is.na(EMovAvrg12), 0, EMovAvrg12)  

SMovAvrg <- c(SMovAvrg10, SMovAvrg11, SMovAvrg12)
EMovAvrg <- c(EMovAvrg10, EMovAvrg11, EMovAvrg12)
                
spread   <- c(spread10,spread11, spread12)
SMovAvrg <- c(SMA(spread10,20),SMA(spread11,20), SMA(spread12,20))
EMovAvrg <- c(EMA(spread10,10),EMA(spread11,10), EMA(spread12,10))

par(mfrow=c(3,1))
<<<<<<< HEAD
ts.plot(as.ts(spread10), as.ts(SMovAvrg10), as.ts(EMovAvrg10))
ts.plot(as.ts(spread11), as.ts(SMovAvrg11), as.ts(EMovAvrg11))
ts.plot(as.ts(spread12), as.ts(SMovAvrg12), as.ts(EMovAvrg12))

par(mfrow=c(1,1))
ts.plot(as.ts(spread), as.ts(SMovAvrg), as.ts(EMovAvrg))
=======
ts.plot(as.ts(spread10), as.ts(SMovAvrg10), as.ts(EMovAvrg10), main = 'Spread 2010')
ts.plot(as.ts(spread11), as.ts(SMovAvrg11), as.ts(EMovAvrg11), main = 'Spread 2011')
ts.plot(as.ts(spread12), as.ts(SMovAvrg12), as.ts(EMovAvrg12), main = 'Spread 2012')

par(mfrow=c(1,1))
ts.plot(as.ts(spread), as.ts(SMovAvrg), as.ts(EMovAvrg), main = 'Spread Evolution from 2010 to 2012')
>>>>>>> 9706122272dee6f15253ae52acb7e0c87a60afac

tt <- rep(1:length(spread))
reg <- lm(spread ~ tt)

<<<<<<< HEAD
plot.ts(spread)
=======
plot.ts(spread, main = 'Spread Evolution from 2010 to 2012 with Drift problem')
>>>>>>> 9706122272dee6f15253ae52acb7e0c87a60afac
abline(reg$coef[1],reg$coef[2], lwd=3, col='red')

spread102 <- spread10 - reg$coef[1] - reg$coef[2]*rep(1:length(spread10))
spread112 <- spread11 - reg$coef[1] - reg$coef[2]*rep(1:length(spread11))
spread122 <- spread12 - reg$coef[1] - reg$coef[2]*rep(1:length(spread12))
spread2  <- c(spread102,spread112, spread122)

par(mfrow=c(2,1))
<<<<<<< HEAD
ts.plot(as.ts(spread2), as.ts(SMovAvrg), as.ts(EMovAvrg))

spread3 <- spread-reg$coef[1] - reg$coef[2]*rep(1:length(spread))
ts.plot(as.ts(spread3), as.ts(c(SMA(spread102,10),SMA(spread112,10),
        SMA(spread122,10))), as.ts(c(EMA(spread102,10),EMA(spread112,10),EMA(spread122,10))))
=======
ts.plot(as.ts(spread2), main = 'Spread Evolution from 2010 to 2012 with no-Drift')

spread3 <- spread-reg$coef[1] - reg$coef[2]*rep(1:length(spread))
ts.plot(as.ts(spread3), as.ts(c(SMA(spread102,10),SMA(spread112,10),
        SMA(spread122,10))), as.ts(c(EMA(spread102,10),EMA(spread112,10),EMA(spread122,10))) )
>>>>>>> 9706122272dee6f15253ae52acb7e0c87a60afac

# plot.ts(spread-reg$coef[1] - reg$coef[2]*rep(1:length(spread)))


# ---------------------------------------------------------------------
# 2010 Spread Mean-Reverting Model (training in-the-sample)
# ---------------------------------------------------------------------

print("-------------------------------------------")
print("Results for 2010")
print("-------------------------------------------")
position10_f    <- rep (0, times=length(spread10) )
position10_m2m  <- rep (0, times=length(spread10) )
results10       <- rep (0, times=length(spread10) )

for (i in 2:length(spread10)) {

  # OPENING POSITIONS ------------------------------------------------------------
  
  # if spread is too high:  SELL SPREAD = short in oil  + long in Gas
  
  if  ((position10_f[i-1] == 0) && (SMovAvrg10[i-1]<EMovAvrg10[i-1]) && (SMovAvrg10[i] > EMovAvrg10[i])) 
  {
    position10_f[i]    =  spread10[i]
    position10_m2m[i]  = position10_m2m[i-1] 
  }
  
  # if spread is too low:   BUY SPREAD  = long in oil   + short in Gas
  
  if  ((position10_f[i-1] == 0) && (SMovAvrg10[i-1]>EMovAvrg10[i-1]) && (SMovAvrg10[i]<EMovAvrg10[i]))  
  {
    position10_f[i]    = - spread10[i]
    position10_m2m[i]  = position10_m2m[i-1] 
  }
  
  # WAITING THE POSITIONS ------------------------------------------------------------  
  
  if  ((position10_f[i-1] > 0) && (SMovAvrg10[i-1] > EMovAvrg10[i-1]) && (SMovAvrg10[i] > EMovAvrg10[i]))   
  {
    position10_f[i] = position10_f[i-1]
    position10_m2m[i] = position10_m2m[i-1] + (- spread10[i] + spread10[i-1])
  }
  
  if  ((position10_f[i-1] < 0) && (SMovAvrg10[i-1] < EMovAvrg10[i-1]) && (SMovAvrg10[i] < EMovAvrg10[i])) 
  {
    position10_f[i] = position10_f[i-1]
    position10_m2m[i] = position10_m2m[i-1] + ( spread10[i] - spread10[i-1])
  }
  
  # CLOSING POSITIONS ------------------------------------------------------------
  
  # Closing short position (SOLD THE SPREAD to BUY it NOW)
  
  if  ((position10_f[i-1] > 0) && (SMovAvrg10[i-1]>EMovAvrg10[i-1]) && (SMovAvrg10[i]<EMovAvrg10[i]))   
  {
    results10[i] = position10_f[i-1] - spread10[i]
    position10_f[i] = 0
    position10_m2m[i] = position10_m2m[i-1] + ( - spread10[i] + spread10[i-1])
  }
  
  # Closing long position
  
  if  ((position10_f[i-1] < 0) && (SMovAvrg10[i-1]<EMovAvrg10[i-1]) && (SMovAvrg10[i]>EMovAvrg10[i]))   
  {
    results10[i] = spread10[i] - position10_f[i-1] 
    position10_f[i] = 0
    position10_m2m[i] = position10_m2m[i-1] + ( spread10[i] - spread10[i-1] )
  }
}

print(cumsum(results10)[length(results10)])



# X <- (spread10)
# t <- rep(1:length(X))
# print(jarque.bera.test(diff(X)))
# 
# mle(OU.lik, start = list(theta1=1, theta2=0.5, theta3=1), method = "L-BFGS-B") -> fit10
# 
# print(summary(fit10))

# ---------------------------------------------------------------------
# Trading rules for 2011 
# ---------------------------------------------------------------------

# if spread is too low:   BUY SPREAD  = long in oil   + short in Gas
# if spread is too high:  SELL SPREAD = short in oil  + long in Gas

# spread11 <- prices11[,4]-prices11[,2]

# t <- rep(1:length(spread11))
# 
# predict  <- rep(spread11[1], times=length(spread11))
# 
# mean_10  <- spread11[1]* exp(-coef(fit10)[2]*t) + (coef(fit10)[1] / coef(fit10)[2]) * (1-exp(-coef(fit10)[2]*t))
#  
# var_10   <- (coef(fit10)[3]^2 / (2*coef(fit10)[2])) * (1-exp(-2*coef(fit10)[2]*t))
# 
# std = 1

position11_f    <- rep (0, times=length(spread11) )
position11_m2m  <- rep (0, times=length(spread11) )
results11       <- rep (0, times=length(spread11) )

# account11  <- rep (0, times=length(spread11) )

for (i in 2:length(spread11)) {

  # predict[i] <- coef(fit10)[1] / coef(fit10)[2] + ( spread11[i-1] -
  #   coef(fit10)[1] / coef(fit10)[2]) * exp ( -coef(fit10)[2])
  
 # if (EMovAvg)

# OPENING POSITIONS ------------------------------------------------------------
  
  # if spread is too high:  SELL SPREAD = short in oil  + long in Gas
  
  if  ((position11_f[i-1] == 0) && (SMovAvrg11[i-1]<EMovAvrg11[i-1]) && (SMovAvrg11[i] > EMovAvrg11[i])) 
    {
      position11_f[i]    =  spread11[i]
      position11_m2m[i]  = position11_m2m[i-1] 
    }
  
  # if spread is too low:   BUY SPREAD  = long in oil   + short in Gas
  
  if  ((position11_f[i-1] == 0) && (SMovAvrg11[i-1]>EMovAvrg11[i-1]) && (SMovAvrg11[i]<EMovAvrg11[i]))  
    {
      position11_f[i]    = spread11[i]
      position11_m2m[i]  = position11_m2m[i-1] 
    }
  
# WAITING THE POSITIONS ------------------------------------------------------------  
  
  if  ((position11_f[i-1] < 0) && (SMovAvrg11[i-1]>EMovAvrg11[i-1]) && (SMovAvrg11[i]>EMovAvrg11[i]))   
    {
      position11_f[i] = position11_f[i-1]
      position11_m2m[i] = position11_m2m[i-1] + (- spread11[i] + spread11[i-1])
    }
  
  if  ((position11_f[i-1] > 0) && (SMovAvrg11[i-1]<EMovAvrg11[i-1]) && (SMovAvrg11[i]<EMovAvrg11[i])) 
    {
      position11_f[i] = position11_f[i-1]
      position11_m2m[i] = position11_m2m[i-1] + ( spread11[i] - spread11[i-1])
    }

# CLOSING POSITIONS ------------------------------------------------------------

# Closing short position
  
if  ((position11_f[i-1] < 0) && (SMovAvrg11[i-1]>EMovAvrg11[i-1]) && (SMovAvrg11[i]<EMovAvrg11[i]))   
  {
    results11[i] = position11_f[i-1] - spread11[i]
    position11_f[i] = 0
    position11_m2m[i] = position11_m2m[i-1] + (- spread11[i] + spread11[i-1])
  }

# Closing long position

if  ((position11_f[i-1] > 0) && (SMovAvrg11[i-1]<EMovAvrg11[i-1]) && (SMovAvrg11[i]>EMovAvrg11[i]))   
  {
    results11[i] = spread11[i] - position11_f[i-1] 
    position11_f[i] = 0
    position11_m2m[i] = position11_m2m[i-1] + ( spread11[i] - spread11[i-1] )
  }
}

print(results11)
  
  # ---------------------------------------------------------------------  
  # 2011 Spread Mean-Reverting Model 
  # ---------------------------------------------------------------------
  
  print("-------------------------------------------")
  print("Results for 2011")
  print("-------------------------------------------")
  
  X <- (spread11)
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

mean_11  <- spread12[1]* exp(-coef(fit11)[2]*t) + (coef(fit11)[1] / coef(fit11)[2]) * (1-exp(-coef(fit11)[2]*t))

var_11   <- (coef(fit11)[3]^2 / (2*coef(fit11)[2])) * (1-exp(-2*coef(fit11)[2]*t))

std = 1

position12_f    <- rep (0, times=length(spread12) )
position12_m2m  <- rep (0, times=length(spread12) )
results12       <- rep (0, times=length(spread12) )

# account11  <- rep (0, times=length(spread11) )

for (i in 2:length(spread12)) {
  
  predict[i] <- coef(fit11)[1] / coef(fit11)[2] + ( spread12[i-1] -
                coef(fit11)[1] / coef(fit11)[2]) * exp ( -coef(fit11)[2])
  
  # OPENING POSITIONS ------------------------------------------------------------
  
  # if spread is too high:  SELL SPREAD = short in oil  + long in Gas
  
  if  (position12_f[i-1] == 0 && spread12[i] > ( mean_11[i] + std * var_11[i] )) 
  {
    position12_f[i]    = - spread12[i]
    position12_m2m[i]  = position12_m2m[i-1] 
  }
  
  # if spread is too low:   BUY SPREAD  = long in oil   + short in Gas
  
  if  (position12_f[i-1] == 0 && spread12[i] < ( mean_11[i] - std * var_11[i] )) 
  {
    position12_f[i]    = spread12[i]
    position12_m2m[i]  = position12_m2m[i-1] 
  }
  
  # WAITING THE POSITIONS ------------------------------------------------------------  
  
  if  (position12_f[i-1] < 0 && (spread12[i] > ( mean_11[i] - std * var_11[i] )))  
  {
    position12_f[i] = position12_f[i-1]
    position12_m2m[i] = position12_m2m[i-1] + (- spread12[i] + spread12[i-1])
  }
  
  if  (position12_f[i-1] > 0 && (spread12[i] < ( mean_11[i] + std * var_11[i] )))  
  {
    position12_f[i] = position12_f[i-1]
    position12_m2m[i] = position12_m2m[i-1] + ( spread12[i] - spread12[i-1])
  }
  
  # CLOSING POSITIONS ------------------------------------------------------------
  
  # Closing short position
  
  if  (position12_f[i-1] < 0 && (spread12[i] < ( mean_11[i] - std * var_11[i] )))  
  {
    results12[i] = - position12_f[i-1] - spread12[i]
    position12_f[i] = 0
    position12_m2m[i] = position12_m2m[i-1] + (- spread12[i] + spread12[i-1])
  }
  
  # Closing long position
  
  if  (position12_f[i-1] > 0 && (spread12[i] > ( mean_11[i] + std * var_11[i] )))  
  {
    results12[i] = spread12[i] - position12_f[i-1] 
    position12_f[i] = 0
    position12_m2m[i] = position12_m2m[i-1] + ( spread12[i] - spread12[i-1] )
  }
}

print(results12)


  print(summary(fit11))
  
  print("-------------------------------------------")
  print("Results for 2012")
  print("-------------------------------------------")
  # spread12 <- prices12[,4]-prices12[,2]
  X <- (spread12)
  print(jarque.bera.test(diff(X)))
  mle(OU.lik, start = list(theta1=1, theta2=0.5, theta3=1), method = "L-BFGS-B") -> fit12
  print(summary(fit12))