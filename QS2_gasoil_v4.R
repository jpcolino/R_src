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

setwd("//Users/JPC/Google Drive/Gas Oil Spread Trade/") # Set here your working directory

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
par(mfrow=c(2,1))
plot.ts(c(prices10[,2], prices11[,2], prices12[,2]))
plot.ts(c(prices10[,3], prices11[,3], prices12[,3]))



# Correlation Analysis

par(mfrow=c(1,1))
MovingCor10 <- running(as.numeric(prices10[,4]), as.numeric(prices10[,2]), cor)
MovingCor11 <- running(as.numeric(prices11[,4]), as.numeric(prices11[,2]), cor)
MovingCor12 <- running(as.numeric(prices12[,4]), as.numeric(prices12[,2]), cor)
plot.ts(c(MovingCor10, MovingCor11, MovingCor12))

# Regresion for the whole period: we do not trade the trend

spread10 <- 0.39*prices10[,4] - 0.61*prices10[,2] # 0.39*Oil_Euros - 0.61*Gas_Euros
spread11 <- 0.39*prices11[,4] - 0.61*prices11[,2]
spread12 <- 0.39*prices12[,4] - 0.61*prices12[,2]

SMovAvrg10 <- c(SMA(spread10,20))
SMovAvrg10 <- ifelse(is.na(SMovAvrg10), 0, SMovAvrg10)

SMovAvrg11 <- c(SMA(spread11,20))
SMovAvrg11 <- ifelse(is.na(SMovAvrg11), 0, SMovAvrg11)

SMovAvrg12 <- c(SMA(spread12,20))
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
ts.plot(as.ts(spread10), as.ts(SMovAvrg10), as.ts(EMovAvrg10))
ts.plot(as.ts(spread11), as.ts(SMovAvrg11), as.ts(EMovAvrg11))
ts.plot(as.ts(spread12), as.ts(SMovAvrg12), as.ts(EMovAvrg12))

par(mfrow=c(1,1))
ts.plot(as.ts(spread), as.ts(SMovAvrg), as.ts(EMovAvrg))

tt <- rep(1:length(spread))
reg <- lm(spread ~ tt)

plot.ts(spread)
abline(reg$coef[1],reg$coef[2], lwd=3, col='red')

spread102 <- spread10 - reg$coef[1] - reg$coef[2]*rep(1:length(spread10))
spread112 <- spread11 - reg$coef[1] - reg$coef[2]*rep(1:length(spread11))
spread122 <- spread12 - reg$coef[1] - reg$coef[2]*rep(1:length(spread12))
spread2  <- c(spread102,spread112, spread122)

par(mfrow=c(2,1))
ts.plot(as.ts(spread2), as.ts(SMovAvrg), as.ts(EMovAvrg))

spread3 <- spread-reg$coef[1] - reg$coef[2]*rep(1:length(spread))
ts.plot(as.ts(spread3), as.ts(c(SMA(spread102,10),SMA(spread112,10),
                                SMA(spread122,10))), as.ts(c(EMA(spread102,10),EMA(spread112,10),EMA(spread122,10))))


# ---------------------------------------------------------------------
# 2010 Directional Spread Model (training and testing in-the-sample)
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
  
  if  ((position10_f[i-1] == 0) && (SMovAvrg10[i-1] < EMovAvrg10[i-1]) && (SMovAvrg10[i] > EMovAvrg10[i]))
  {
    position10_f[i]    =  spread10[i]
    position10_m2m[i]  = position10_m2m[i-1]
  }
  
  # if spread is too low:   BUY SPREAD  = long in oil   + short in Gas
  
  if  ((position10_f[i-1] == 0) && (SMovAvrg10[i-1] > EMovAvrg10[i-1]) && (SMovAvrg10[i] < EMovAvrg10[i])) 
  {
    position10_f[i]    = - spread10[i]
    position10_m2m[i]  = position10_m2m[i-1]
  }
  
  # WAITING THE POSITIONS ------------------------------------------------------------ 
  # Holding a short position
  if  ((position10_f[i-1] > 0) && (SMovAvrg10[i-1] > EMovAvrg10[i-1]) && (SMovAvrg10[i] > EMovAvrg10[i]))  
  {
    position10_f[i] = position10_f[i-1]
    position10_m2m[i] = position10_m2m[i-1] + (- spread10[i] + spread10[i-1] )
  }
  # Holding a long position
  if  ((position10_f[i-1] < 0) && (SMovAvrg10[i-1] < EMovAvrg10[i-1]) && (SMovAvrg10[i] < EMovAvrg10[i]))
  {
    position10_f[i] = position10_f[i-1]
    position10_m2m[i] = position10_m2m[i-1] + ( spread10[i] - spread10[i-1] )
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
    results10[i] = spread10[i] + position10_f[i-1]
    position10_f[i] = 0
    position10_m2m[i] = position10_m2m[i-1] + ( spread10[i] - spread10[i-1] )
  }
}

print(cumsum(results10)[length(results10)])


print("-------------------------------------------")
print("Results for 2011")
print("-------------------------------------------")
# ---------------------------------------------------------------------
# Trading rules for 2011
# ---------------------------------------------------------------------


position11_f    <- rep (0, times=length(spread11) )
position11_m2m  <- rep (0, times=length(spread11) )
results11       <- rep (0, times=length(spread11) )

# account11  <- rep (0, times=length(spread11) )

for (i in 2:length(spread11)) {
  
  # OPENING POSITIONS ------------------------------------------------------------
  
  # if spread is too high:  SELL SPREAD = short in oil  + long in Gas
  
  if  ((position11_f[i-1] == 0) && (SMovAvrg11[i-1] < EMovAvrg11[i-1]) && (SMovAvrg11[i] > EMovAvrg11[i]))
  {
    position11_f[i]    =  spread11[i]
    position11_m2m[i]  = position11_m2m[i-1]
  }
  
  # if spread is too low:   BUY SPREAD  = long in oil   + short in Gas
  
  if  ((position11_f[i-1] == 0) && (SMovAvrg11[i-1] > EMovAvrg11[i-1]) && (SMovAvrg11[i] < EMovAvrg11[i])) 
  {
    position11_f[i]    = - spread11[i]
    position11_m2m[i]  = position11_m2m[i-1]
  }
  
  # WAITING THE POSITIONS ------------------------------------------------------------ 
  # Holding a short position
  if  ((position11_f[i-1] > 0) && (SMovAvrg11[i-1] > EMovAvrg11[i-1]) && (SMovAvrg11[i] > EMovAvrg11[i]))  
  {
    position11_f[i] = position11_f[i-1]
    position11_m2m[i] = position11_m2m[i-1] + (- spread11[i] + spread11[i-1] )
  }
  # Holding a long position
  if  ((position11_f[i-1] < 0) && (SMovAvrg11[i-1] < EMovAvrg11[i-1]) && (SMovAvrg11[i] < EMovAvrg11[i]))
  {
    position11_f[i] = position11_f[i-1]
    position11_m2m[i] = position11_m2m[i-1] + ( spread11[i] - spread11[i-1] )
  }
  
  # CLOSING POSITIONS ------------------------------------------------------------
  
  # Closing short position (SOLD THE SPREAD to BUY it NOW)
  
  if  ((position11_f[i-1] > 0) && (SMovAvrg11[i-1]>EMovAvrg11[i-1]) && (SMovAvrg11[i]<EMovAvrg11[i]))  
  {
    results11[i] = position11_f[i-1] - spread11[i]
    position11_f[i] = 0
    position11_m2m[i] = position11_m2m[i-1] + ( - spread11[i] + spread11[i-1])
  }
  
  # Closing long position
  
  if  ((position11_f[i-1] < 0) && (SMovAvrg11[i-1]<EMovAvrg11[i-1]) && (SMovAvrg11[i]>EMovAvrg11[i]))  
  {
    results11[i] = spread11[i] + position11_f[i-1]
    position11_f[i] = 0
    position11_m2m[i] = position11_m2m[i-1] + ( spread11[i] - spread11[i-1] )
  }
}

print(cumsum(results11)[length(results11)])


print("-------------------------------------------")
print("Results for 2012")
print("-------------------------------------------")


# ---------------------------------------------------------------------
# Trading rules for 2012 
# ---------------------------------------------------------------------

# if spread is too low:   BUY SPREAD  = long in oil   + short in Gas
# if spread is too high:  SELL SPREAD = short in oil  + long in Gas

# spread12 <- prices12[,4]-prices12[,2]

position12_f    <- rep (0, times=length(spread12) )
position12_m2m  <- rep (0, times=length(spread12) )
results12       <- rep (0, times=length(spread12) )

for (i in 2:length(spread12)) {
  
  # OPENING POSITIONS ------------------------------------------------------------
  
  # if spread is too high:  SELL SPREAD = short in oil  + long in Gas
  
  if  ((position12_f[i-1] == 0) && (SMovAvrg12[i-1] < EMovAvrg12[i-1]) && (SMovAvrg12[i] > EMovAvrg12[i]))
  {
    position12_f[i]    =  spread12[i]
    position12_m2m[i]  = position12_m2m[i-1]
  }
  
  # if spread is too low:   BUY SPREAD  = long in oil   + short in Gas
  
  if  ((position12_f[i-1] == 0) && (SMovAvrg12[i-1] > EMovAvrg12[i-1]) && (SMovAvrg12[i] < EMovAvrg12[i])) 
  {
    position12_f[i]    = - spread12[i]
    position12_m2m[i]  = position12_m2m[i-1]
  }
  
  # WAITING THE POSITIONS ------------------------------------------------------------ 
  # Holding a short position
  if  ((position12_f[i-1] > 0) && (SMovAvrg12[i-1] > EMovAvrg12[i-1]) && (SMovAvrg12[i] > EMovAvrg12[i]))  
  {
    position12_f[i] = position12_f[i-1]
    position12_m2m[i] = position12_m2m[i-1] + (- spread12[i] + spread12[i-1] )
  }
  # Holding a long position
  if  ((position12_f[i-1] < 0) && (SMovAvrg12[i-1] < EMovAvrg12[i-1]) && (SMovAvrg12[i] < EMovAvrg12[i]))
  {
    position12_f[i] = position12_f[i-1]
    position12_m2m[i] = position12_m2m[i-1] + ( spread12[i] - spread12[i-1] )
  }
  
  # CLOSING POSITIONS ------------------------------------------------------------
  
  # Closing short position (SOLD THE SPREAD to BUY it NOW)
  
  if  ((position12_f[i-1] > 0) && (SMovAvrg12[i-1]>EMovAvrg12[i-1]) && (SMovAvrg12[i]<EMovAvrg12[i]))  
  {
    results12[i] = position12_f[i-1] - spread12[i]
    position12_f[i] = 0
    position12_m2m[i] = position12_m2m[i-1] + ( - spread12[i] + spread12[i-1])
  }
  
  # Closing long position
  
  if  ((position12_f[i-1] < 0) && (SMovAvrg12[i-1]<EMovAvrg12[i-1]) && (SMovAvrg12[i]>EMovAvrg12[i]))  
  {
    results12[i] = spread12[i] + position12_f[i-1]
    position12_f[i] = 0
    position12_m2m[i] = position12_m2m[i-1] + ( spread12[i] - spread12[i-1] )
  }
}

print(cumsum(results12)[length(results12)])

par(mfrow=c(3,1))
plot.ts(cumsum(results10))
plot.ts(cumsum(results11))
plot.ts(cumsum(results12))

par(mfrow=c(3,1))
plot.ts(position10_m2m)
plot.ts(position11_m2m)
plot.ts(position12_m2m)

par(mfrow=c(3,1))
plot.ts(position10_f)
plot.ts(position11_f)
plot.ts(position12_f)
