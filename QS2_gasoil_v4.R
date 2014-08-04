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
prices10 <- prices10[23:dim(prices10)[1],]

prices11 <- read.csv("gasoil11.csv", header = TRUE, sep = ",", quote="\"", dec=".")
prices11 <- prices11[complete.cases(prices11),]
prices11 <- prices11[23:dim(prices11)[1],]

prices12 <- read.csv("gasoil12.csv", header = TRUE, sep = ",", quote="\"", dec=".")
prices12 <- prices12[complete.cases(prices12),]
prices12 <- prices12[23:dim(prices12)[1],]

# Spreads for 10k barrels versus 10Mw TTF Winter

spread10 <- 10*182*24* prices10[,2] - 10000* prices10[,4] # 0.61*Gas_Euros - 0.39*Oil_Euros
spread11 <- 10*182*24* prices11[,2] - 10000* prices11[,4] 
spread12 <- 10*182*24* prices12[,2] - 10000* prices12[,4]


# Some previous plotting
par(mfrow=c(3,1))
plot.ts(c(prices10[,2], prices11[,2], prices12[,2]), main = 'Price evolution TTF Winter 2010 - 2012')
plot.ts(c(prices10[,3], prices11[,3], prices12[,3]), main = 'Price evolution Oil Euros 2010 - 2012')
plot.ts(c(spread10, spread11, spread12), main = 'Spread 10k barrels vs 10Mw in Euros 2010 - 2012')


# Correlation Analysis

par(mfrow=c(2,1))
MovingCor10 <- running(as.numeric(prices10[,4]), as.numeric(prices10[,2]), cor)
MovingCor11 <- running(as.numeric(prices11[,4]), as.numeric(prices11[,2]), cor)
MovingCor12 <- running(as.numeric(prices12[,4]), as.numeric(prices12[,2]), cor)
plot.ts(c(MovingCor10, MovingCor11, MovingCor12), main ='Correlation Oil-Gas evolution 2010 - 2012')

MovingCor10 <- running(as.numeric(diff(log(prices10[,4]))), as.numeric(diff(log(prices10[,2]))), cor)
MovingCor11 <- running(as.numeric(diff(log(prices11[,4]))), as.numeric(diff(log(prices11[,2]))), cor)
MovingCor12 <- running(as.numeric(diff(log(prices12[,4]))), as.numeric(diff(log(prices12[,2]))), cor)
plot.ts(c(MovingCor10, MovingCor11, MovingCor12), main ='Correlation log. ret. Oil-Gas evolution 2010 - 2012')

SMovAvrg101 <- c(SMA(spread10,25))
SMovAvrg10 <- ifelse(is.na(SMovAvrg101), 0, SMovAvrg101)

SMovAvrg111 <- c(SMA(spread11,25))
SMovAvrg11 <- ifelse(is.na(SMovAvrg111), 0, SMovAvrg111)

SMovAvrg121 <- c(SMA(spread12,25))
SMovAvrg12 <- ifelse(is.na(SMovAvrg121), 0, SMovAvrg121)

EMovAvrg101 <- c(EMA(spread10,10))
EMovAvrg10 <- ifelse(is.na(EMovAvrg101), 0, EMovAvrg101)

EMovAvrg111 <- c(EMA(spread11,10))
EMovAvrg11 <- ifelse(is.na(EMovAvrg111), 0, EMovAvrg111)

EMovAvrg121 <- c(EMA(spread12,10))               
EMovAvrg12 <- ifelse(is.na(EMovAvrg121), 0, EMovAvrg121) 

SMovAvrg <- c(SMovAvrg10, SMovAvrg11, SMovAvrg12)
EMovAvrg <- c(EMovAvrg10, EMovAvrg11, EMovAvrg12)

spread   <- c(spread10,spread11, spread12)
SMovAvrg <- c(SMA(spread10,20), SMA(spread11,20), SMA(spread12,20))
EMovAvrg <- c(EMA(spread10,10), EMA(spread11,10), EMA(spread12,10))


par(mfrow=c(3,1))
ts.plot(as.ts(spread10), as.ts(SMovAvrg101), as.ts(EMovAvrg101), main = 'Spread 2010')
ts.plot(as.ts(spread11), as.ts(SMovAvrg111), as.ts(EMovAvrg111), main = 'Spread 2011')
ts.plot(as.ts(spread12), as.ts(SMovAvrg121), as.ts(EMovAvrg121), main = 'Spread 2012')

par(mfrow=c(1,1))
ts.plot(as.ts(spread), as.ts(SMovAvrg), as.ts(EMovAvrg), main = 'Spread Evolution from 2010 to 2012 for 10k barrels - 10Mwh')


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
  
  if  ((position10_f[i-1] > 0) && (SMovAvrg10[i-1] > EMovAvrg10[i-1]) && (SMovAvrg10[i] < EMovAvrg10[i]))  
  {
    results10[i] = position10_f[i-1] - spread10[i]
    position10_f[i] = - spread10[i]
    position10_m2m[i] =  0 # position10_m2m[i-1] + ( - spread10[i] + spread10[i-1])
  }
  
  # Closing long position
  
  if  ((position10_f[i-1] < 0) && (SMovAvrg10[i-1] < EMovAvrg10[i-1]) && (SMovAvrg10[i] > EMovAvrg10[i]))  
  {
    results10[i] = spread10[i] + position10_f[i-1]
    position10_f[i] = spread10[i]
    position10_m2m[i] =  0 # position10_m2m[i-1] + ( spread10[i] - spread10[i-1] )
  }
}

print(cumsum(results10)[length(results10)]+position10_m2m[length(position10_m2m)])


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
  
  if  ((position11_f[i-1] > 0) && (SMovAvrg11[i-1] > EMovAvrg11[i-1]) && (SMovAvrg11[i] < EMovAvrg11[i]))  
  {
    results11[i] = position11_f[i-1] - spread11[i]
    position11_f[i] = - spread11[i]
    position11_m2m[i] =  0 # position11_m2m[i-1] + ( - spread11[i] + spread11[i-1])
  }
  
  # Closing long position
  
  if  ((position11_f[i-1] < 0) && (SMovAvrg11[i-1] < EMovAvrg11[i-1]) && (SMovAvrg11[i] > EMovAvrg11[i]))  
  {
    results11[i] = spread11[i] + position11_f[i-1]
    position11_f[i] = spread11[i]
    position11_m2m[i] =  0 # position11_m2m[i-1] + ( spread11[i] - spread11[i-1] )
  }
}

print(cumsum(results11)[length(results11)]+position11_m2m[length(position11_m2m)])


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
  
  if  ((position12_f[i-1] > 0) && (SMovAvrg12[i-1] > EMovAvrg12[i-1]) && (SMovAvrg12[i] < EMovAvrg12[i]))  
  {
    results12[i] = position12_f[i-1] - spread12[i]
    position12_f[i] = - spread12[i]
    position12_m2m[i] =  0 # position12_m2m[i-1] + ( - spread12[i] + spread12[i-1])
  }
  
  # Closing long position and opening short
  
  if  ((position12_f[i-1] < 0) && (SMovAvrg12[i-1] < EMovAvrg12[i-1]) && (SMovAvrg12[i] > EMovAvrg12[i]))  
  {
    results12[i] = spread12[i] + position12_f[i-1]
    position12_f[i] = spread12[i]
    position12_m2m[i] = 0 # position12_m2m[i-1] + ( spread12[i] - spread12[i-1] )
  }
}

print(cumsum(results12)[length(results12)]+ position12_m2m[length(position12_m2m)])

# par(mfrow=c(3,1))
# plot.ts(position10_m2m)
# plot.ts(position11_m2m)
# plot.ts(position12_m2m)

par(mfrow=c(3,1))
ts.plot(as.ts(cumsum(results10) + position10_m2m), as.ts(cumsum(results10)), main = 'P&L 2010')
ts.plot(as.ts(cumsum(results11) + position11_m2m), as.ts(cumsum(results11)), main = 'P&L 2011')
ts.plot(as.ts(cumsum(results12) + position12_m2m), as.ts(cumsum(results12)), main = 'P&L 2012')

