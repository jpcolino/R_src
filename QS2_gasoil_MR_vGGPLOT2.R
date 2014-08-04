library("gdata")
library("gtools")
library("sde")
library("stats4")
library("ggplot2")
library("stats4")
library ("tseries")
library("plyr")
library("TTR")


rm(list = ls()) # Clean the workspace 
# ================================================================================
# Reading and Loading the data
# ================================================================================
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

# ================================================================================
# Auxiliary functions for model calculation
# ================================================================================
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


MovingCor <- function(x, y, window.size=21, method="pearson") {
  # Computes moving correlations between two vectors with symmetrical windows.
  #
  # Args:
  #   x: One of the two vectors whose correlation is to be calculated.
  #   y: The other vector. Note that it must be of the same length as x.
  #   window.size: The size of windows to be used for each calculated
  #                correlation. Note that if even numbers are chosen, the
  #                window will not be skewed as there will be one extra value
  #                on the upper-side of the window. Default size is 21.
  #   method: The method of correlation. May be: "pearson", "kendall", or
  #           "spearman". Default is "pearson".
  #
  # Returns:
  #   A vector of the moving correlations.
  n <- length(x)
  # Setup a few catches for error handling.
  if (TRUE %in% is.na(y) || TRUE %in% is.na(x)) {
    stop("Arguments x and y cannot have missing values.")
  }
  if (n == 1 || n != length(y)) {
    stop("Arguments x and y have different lengths: ",
         length(x), " and ", length(y), ".")
  }
  out <- rep(NA, round(window.size/2))  # Stuffing the returned vector.
  for (value in seq(from = 1, to = n - (window.size - 1))) {
    value.end = value + (window.size - 1)
    out <- append(out, cor(x[value:value.end],
                           y[value:value.end],
                           method = method))
  }
  out <- append(out, rep(NA, n - length(out)))  # Finish stuffing.
  return(out)
}


# ================================================================================
## Auxiliary functions for Multiple plot function
# ================================================================================
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
# ================================================================================
# Regresion for the whole period: we do not trade the trend
# ================================================================================
spread10 <- 10*182*24*prices10[,2]-10000*prices10[,4] # Gas_Euros - Oil_Euros
spread11 <- 10*182*24*prices11[,2]-10000*prices11[,4] 
spread12 <- 10*182*24*prices12[,2]-10000*prices12[,4]
spread13 <- 10*182*24*prices13[,2]-10000*prices13[,3]/prices13[,4]

spread   <- c(spread10,spread11, spread12, spread13)

# ================================================================================
# PLOTTING the PRICES by YEAR (with ggplot)
# ================================================================================
x10 <- as.Date(prices10$X,format='%d/%m/%Y')
df_prices10 <- data.frame(x10,prices10$TTF.Winter10)
print (ggplot(df_prices10, aes(x10,prices10$TTF.Winter10))+geom_line(colour="darkgreen", size=1)+ggtitle("TTF Winter Prices for 2010"))
print(ggplot(df_prices10, aes(x10,prices10$TTF.Winter10))+geom_area())


x11 <- as.Date(prices11$X,format='%d/%m/%Y')
df_prices11 <- data.frame(x11,prices11$TTF.Winter11)
print(ggplot(df_prices11, aes(x11,prices11$TTF.Winter11))+geom_line(colour="darkgreen", size=1)+ggtitle("TTF Winter Prices for 2011"))

x12 <- as.Date(prices12$X,format='%d/%m/%Y')
df_prices12 <- data.frame(x12,prices12$TTF.Winter12)
print(ggplot(df_prices12, aes(x12,prices12$TTF.Winter12))+geom_line(colour="darkgreen", size=1)+ggtitle("TTF Winter Prices for 2012"))

x13 <- as.Date(prices13$Timestamp,format='%d/%m/%Y')
df_prices13 <- data.frame(x13,prices13$TTF.Winter13)
print(ggplot(df_prices13, aes(x13,prices13$TTF.Winter13))+geom_line(colour="darkgreen", size=1)+ggtitle("TTF Winter Prices for 2013"))

# ================================================================================
# PLOTTING ALL the PRICES
# ================================================================================

m <-min(length(prices10$X),length(prices11$X),length(prices12$X),length(prices13$Timestamp))

df_prices <- data.frame(x13,prices10$TTF.Winter10[1:m],prices11$TTF.Winter11[1:m],prices12$TTF.Winter12[1:m],prices13$TTF.Winter13[1:m])
names(df_prices)<- c('x','w10','w11','w12','w13')

print(ggplot(df_prices, aes(x))+geom_line(aes(y = w13),colour='red',size=1)+
  geom_line(aes(y = w12),colour='blue',size=1)+
  geom_line(aes(y = w11),colour='green',size=1)+
  ggtitle("TTF Winter Prices for 2011-12-13"))

# ================================================================================
# PLOTTING THE SPREADSu
# ================================================================================
m <-min(length(spread10),length(spread11),length(spread12),length(spread13))

df_spreads <- data.frame(x13,spread10[1:m],spread11[1:m],spread12[1:m],spread13[1:m])
names(df_spreads)<- c('x','s10','s11','s12','s13')

D_spreads <- data.frame(x13[2:m],diff(spread10[1:m]),diff(spread11[1:m]),diff(spread12[1:m]),diff(spread13[1:m]))
names(D_spreads)<- c('x','Ds10','Ds11','Ds12','Ds13')

sma10<-SMA(df_spreads$s10, n=10)
sma11<-SMA(df_spreads$s11, n=10)
sma12<-SMA(df_spreads$s12, n=10)
sma13<-SMA(df_spreads$s13, n=10)

g <- ggplot(df_spreads, aes(x=df_spreads$x))

print(g+
	geom_line(aes(y = sma13),colour='red',size=1.2)+
	geom_line(aes(y = df_spreads$s13),linetype="dashed",colour='darkred',size=.1)+
	geom_line(aes(y = sma12),colour='blue',size=1.2)+
  geom_line(aes(y = df_spreads$s12),linetype="dashed",colour='darkblue',size=.1)+
  geom_line(aes(y = sma11),colour='green',size=1.2)+
  geom_line(aes(y = df_spreads$s11),linetype="dashed",colour='darkgreen',size=.1)+
  geom_line(aes(y = sma10),colour='grey',size=1.2)+
  geom_line(aes(y = df_spreads$s10),linetype="dashed",colour='darkgrey',size=.1)+
  scale_colour_brewer(palette="Set1")+
  ggtitle("Oil_Brent-TTF Winter Spreads for 2010-to-13"))


print(g+
  #geom_line(aes(y = sma13),size=1.2)+
  #geom_area(aes(y = s10), fill="white", alpha=0.8)+
  #geom_line(aes(y = sma12),size=1.2)+
  geom_area(aes(y = df_spreads$s11), fill="green", alpha=0.6)+
  #geom_line(aes(y = sma11),size=1.2)+
  geom_area(aes(y = df_spreads$s12),  fill="red", alpha=0.4)+
  #geom_line(aes(y = sma10),size=1.2)+
  geom_area(aes(y = df_spreads$s13), fill="blue", alpha=0.2)+
  # scale_fill_brewer(palette="Blues") +
  scale_colour_brewer(palette="Set1")+
  ggtitle("Oil_Brent-TTF Winter Spreads for 2011-to-2013"))
#qplot(df_spreads$s10, df_spreads$s11, df_spreads$s12, df_spreads$s13, geom = 'boxplot')

# print( g + geom_boxplot(aes(y = s13)) +  geom_boxplot(aes(y = s12)) +  geom_boxplot(aes(y = s11)) )

print(ggplot(df_spreads, aes(x=df_spreads$s10)) + 
geom_histogram(fill="white", colour="black") + 
ggtitle("Histogram Spreads for 2010"))


print(ggplot(df_spreads, aes(x=df_spreads$s10)) + 
geom_histogram(aes(x=df_spreads$s10),fill="white") + 
geom_histogram(aes(x=df_spreads$s11),fill="green", alpha=0.6) + 
geom_histogram(aes(x=df_spreads$s12),fill="red", alpha=0.4) + 
geom_histogram(aes(x=df_spreads$s13),fill="blue", alpha=0.2) + 
ggtitle("Histogram Spreads for 2010 - 2011 - 2012 - 2014"))

d1<- ggplot(df_spreads, aes(x=df_spreads$s10)) + 
     geom_density(aes(x=df_spreads$s10),fill="white") + 
     geom_density(aes(x=df_spreads$s11),fill="green", alpha=0.6) + 
     geom_density(aes(x=df_spreads$s12),fill="red", alpha=0.4) + 
     geom_density(aes(x=df_spreads$s13),fill="blue", alpha=0.2) + 
     ggtitle("Densities of the Spreads for 2010(white) - 2011(green) - 2012(red) - 2014(blue)")

d2<-ggplot(D_spreads, aes(x=D_spreads$Ds10)) + 
    geom_density(aes(x=D_spreads$Ds10),fill="white") + 
    geom_density(aes(x=D_spreads$Ds11),fill="green", alpha=0.6) + 
    geom_density(aes(x=D_spreads$Ds12),fill="red", alpha=0.4) + 
    geom_density(aes(x=D_spreads$Ds13),fill="blue", alpha=0.2) + 
    ggtitle("Densities of the Diff of Spreads for 2010(white) - 2011(green) - 2012(red) - 2014(blue)")



# ================================================================================
# Volatility and Correlation Analysis
# ================================================================================
# --------------------------------------------------------------------
# 1. Moving Volatility
# --------------------------------------------------------------------
MovingVol10 <- volatility(abs(df_spreads$s10), n=20, calc='close')
MovingVol11 <- volatility(df_spreads$s11, n=20, calc='close')
MovingVol12 <- volatility(df_spreads$s12, n=20, calc='close')
MovingVol13 <- volatility(df_spreads$s13, n=20, calc='close')

m <-min(length(MovingVol10),length(MovingVol11),length(MovingVol12),length(MovingVol13))

df_vols <- data.frame(x13,MovingVol10[1:m],MovingVol11[1:m],MovingVol12[1:m],MovingVol13[1:m])
names(df_vols)<- c('x','v10','v11','v12','v13')

g <- ggplot(df_vols, aes(x=x))

print(g+
  geom_line(aes(y = v13),colour='red',size=1.2)+
  geom_line(aes(y = v12),colour='blue',size=1.2)+
  geom_line(aes(y = v11),colour='green',size=1.2)+
  # geom_line(aes(y = v10),colour='grey',size=1.2)+
  # scale_colour_brewer(palette="Set1")+
  ggtitle("Oil_Brent-TTF Winter Volatilities for 2011-to-13"))


d3 <- ggplot(df_vols, aes(x=df_vols$v12))+ 
        #geom_density(aes(x=df_vols$v10),fill="white") + 
        geom_density(aes(x=df_vols$v11),fill="green", alpha=0.6) + 
        geom_density(aes(x=df_vols$v12),fill="red", alpha=0.4) + 
        geom_density(aes(x=df_vols$v13),fill="blue", alpha=0.2) + 
        ggtitle("Densities of vols over the Diff(prices) for 2010(white) - 2011(green) - 2012(red) - 2014(blue)")


# --------------------------------------------------------------------
# 2. Moving Correlation
# --------------------------------------------------------------------

MovingCor10 <-MovingCor(as.numeric(diff(prices10[,4])), as.numeric(diff(prices10[,2])),window.size=21, method="pearson" )
MovingCor11 <-MovingCor(as.numeric(diff(prices11[,4])), as.numeric(diff(prices11[,2])),window.size=21, method="pearson" )
MovingCor12 <-MovingCor(as.numeric(diff(prices12[,4])), as.numeric(diff(prices12[,2])),window.size=21, method="pearson" )
MovingCor13 <-MovingCor(as.numeric(diff(prices13[,3]/prices13[,4])), as.numeric(diff(prices13[,2])),window.size=21, method="pearson" )


#MovingCor10 <- running(as.numeric(diff(prices10[,4])), as.numeric(diff(prices10[,2])), cor)
#MovingCor11 <- running(as.numeric(diff(prices11[,4])), as.numeric(diff(prices11[,2])), cor)
#MovingCor12 <- running(as.numeric(diff(prices12[,4])), as.numeric(diff(prices12[,2])), cor)
#MovingCor13 <- running(as.numeric(diff(prices13[,3]/prices13[,4])), as.numeric(prices13[,2]), cor)

m <-min(length(MovingCor10),length(MovingCor11),length(MovingCor12),length(MovingCor13))

df_Cor <- data.frame( x13[1:m], as.numeric(MovingCor10[1:m]), as.numeric(MovingCor11[1:m]),+
                        as.numeric(MovingCor12[1:m]), as.numeric(MovingCor13[1:m]) ) 
names(df_Cor)<- c('x','c10', 'c11','c12','c13')

g <- ggplot(df_Cor, aes(x=df_Cor$x))

print(g+
      geom_line (aes(y = df_Cor$c13), colour='blue',  size=1.2)+
      geom_line (aes(y = df_Cor$c12), colour='red', size=1.2)+
      geom_line (aes(y = df_Cor$c11), colour='green',size=1.2)+
      ggtitle("Oil_Brent-TTF Winter Correlation for 2011-to-13"))


d4 <- ggplot(df_Cor, aes(x=df_Cor$c10)) + 
      geom_density(aes(x=df_Cor$c10),fill="white") + 
      geom_density(aes(x=df_Cor$c11),fill="green", alpha=0.6) + 
      geom_density(aes(x=df_Cor$c12),fill="red", alpha=0.4) + 
      geom_density(aes(x=df_Cor$c13),fill="blue", alpha=0.2) + 
      ggtitle("Densities of Correlation over the Diff(prices) for 2010(white) - 2011(green) - 2012(red) - 2014(blue)")


multiplot(d1, d2, d3, d4, cols = 2)


# ================================================================================
# Computing the IC and plotting Out of sample
# ================================================================================
# 2010
# ================================================================================

t10 <- rep(1:length(spread10))
reg <- lm(spread10 ~ t10)

spread101 <- spread10 - reg$coef[1] - reg$coef[2]*rep(1:length(spread10))

X <- (spread101) # <- We are using the 2010 Spread with NO DRIFT (local regression)
t <- rep(1:length(X))

mle(OU.lik, start = list(theta1=1, theta2=0.5, theta3=1), method = "L-BFGS-B") -> fit10

print(summary(fit10))

mean_10  <- spread101[1]* exp(-coef(fit10)[2]*t) + (coef(fit10)[1] / coef(fit10)[2]) * (1-exp(-coef(fit10)[2]*t))

var_10   <- (coef(fit10)[3]^2 / (2*coef(fit10)[2])) * (1-exp(-2*coef(fit10)[2]*t))
var_10   <- sqrt(var_10)

df_spread10<-data.frame(t10, spread10, spread101, mean_10, var_10)

p10 <-   ggplot(df_spread10, aes(x = df_spread10$t10)) + 
         geom_line (aes(y = df_spread10$spread101), colour='black',  size=0.8) + 
         geom_line (aes(y = df_spread10$mean_10), linetype="dashed", colour='darkgrey',  size=0.8) + 
         geom_ribbon (aes(ymin = (df_spread10$mean_10 - .5 * df_spread10$var_10), 
                          ymax = (df_spread10$mean_10 + .5 * df_spread10$var_10)), alpha=0.6) +
         geom_ribbon (aes(ymin = (df_spread10$mean_10 - 1.5 * df_spread10$var_10), 
                          ymax = (df_spread10$mean_10 + 1.5 * df_spread10$var_10)), alpha=0.4) + 
         geom_ribbon (aes(ymin = (df_spread10$mean_10 - 2 * df_spread10$var_10), 
                          ymax = (df_spread10$mean_10 + 2 * df_spread10$var_10)), alpha=0.2) +
         ggtitle('2010 Gas-Oil Spread and MeanRev IC')


# ================================================================================
# 2011
# ================================================================================

t11 <- rep(1:length(spread11))
reg <- lm(spread11 ~ t11)

spread111 <- spread11 - reg$coef[1] - reg$coef[2]*rep(1:length(spread11))

X <- (spread111) # <- We are using the 2010 Spread with NO DRIFT (local regression)
t <- rep(1:length(X))

mle(OU.lik, start = list(theta1=1, theta2=0.5, theta3=1), method = "L-BFGS-B") -> fit11

print(summary(fit11))

mean_11  <- spread111[1]* exp(-coef(fit11)[2]*t) + (coef(fit11)[1] / coef(fit11)[2]) * (1-exp(-coef(fit11)[2]*t))

var_11   <- (coef(fit11)[3]^2 / (2*coef(fit11)[2])) * (1-exp(-2*coef(fit11)[2]*t))
var_11   <- sqrt(var_11)

df_spread11<-data.frame(t11, spread11, spread111, mean_11, var_11)

p11 <-   ggplot(df_spread11, aes(x = df_spread11$t11)) + 
         geom_line (aes(y = df_spread11$spread111), colour='black',  size=0.8) + 
         geom_line (aes(y = df_spread11$mean_11), linetype="dashed", colour='darkgrey',  size=0.8) + 
         geom_ribbon (aes(ymin = (df_spread11$mean_11 - .5 * df_spread11$var_11), 
                          ymax = (df_spread11$mean_11 + .5 * df_spread11$var_11)), alpha=0.6) +
         geom_ribbon (aes(ymin = (df_spread11$mean_11 - 1.5 * df_spread11$var_11), 
                          ymax = (df_spread11$mean_11 + 1.5 * df_spread11$var_11)), alpha=0.4) + 
         geom_ribbon (aes(ymin = (df_spread11$mean_11 - 2 * df_spread11$var_11), 
                          ymax = (df_spread11$mean_11 + 2 * df_spread11$var_11)), alpha=0.2) +
         ggtitle('2011 Gas-Oil Spread and MeanRev IC')
# =====================================================================
# 2012
# =====================================================================


t12 <- rep(1:length(spread12))
reg <- lm(spread12 ~ t12)

spread121 <- spread12 - reg$coef[1] - reg$coef[2]*rep(1:length(spread12))

X <- (spread121) # <- We are using the 2010 Spread with NO DRIFT (local regression)
t <- rep(1:length(X))

mle(OU.lik, start = list(theta1=1, theta2=0.5, theta3=1), method = "L-BFGS-B") -> fit12

print(summary(fit12))

mean_12  <- spread121[1]* exp(-coef(fit12)[2]*t) + (coef(fit12)[1] / coef(fit12)[2]) * (1-exp(-coef(fit12)[2]*t))

var_12   <- (coef(fit12)[3]^2 / (2*coef(fit12)[2])) * (1-exp(-2*coef(fit12)[2]*t))
var_12   <- sqrt(var_12)

df_spread12<-data.frame(t12, spread12, spread121, mean_12, var_12)

p12 <-   ggplot(df_spread12, aes(x = df_spread12$t12)) + 
         geom_line (aes(y = df_spread12$spread121), colour='black',  size=0.8) + 
         geom_line (aes(y = df_spread12$mean_12), linetype="dashed", colour='darkgrey',  size=0.8) + 
         geom_ribbon (aes(ymin = (df_spread12$mean_12 - .5 * df_spread12$var_12), 
                          ymax = (df_spread12$mean_12 + .5 * df_spread12$var_12)), alpha=0.6) +
         geom_ribbon (aes(ymin = (df_spread12$mean_12 - 1.5 * df_spread12$var_12), 
                          ymax = (df_spread12$mean_12 + 1.5 * df_spread12$var_12)), alpha=0.4) + 
         geom_ribbon (aes(ymin = (df_spread12$mean_12 - 2 * df_spread12$var_12), 
                          ymax = (df_spread12$mean_12 + 2 * df_spread12$var_12)), alpha=0.2) +
         ggtitle('2012 Gas-Oil Spread and MeanRev IC')

# =====================================================================
# 2013
# =====================================================================


t13 <- rep(1:length(spread13))
reg <- lm(spread13 ~ t13)

spread131 <- spread13 - reg$coef[1] - reg$coef[2]*rep(1:length(spread13))

X <- (spread131) # <- We are using the 2010 Spread with NO DRIFT (local regression)
t <- rep(1:length(X))

mle(OU.lik, start = list(theta1=1, theta2=0.5, theta3=1), method = "L-BFGS-B") -> fit13

print(summary(fit13))

mean_13  <- spread131[1]* exp(-coef(fit13)[2]*t) + (coef(fit13)[1] / coef(fit13)[2]) * (1-exp(-coef(fit13)[2]*t))

var_13   <- (coef(fit13)[3]^2 / (2*coef(fit13)[2])) * (1-exp(-2*coef(fit13)[2]*t))
var_13   <- sqrt(var_13)

df_spread13<-data.frame(t13, spread13, spread131, mean_13, var_13)

p13 <-   ggplot(df_spread13, aes(x = df_spread13$t13)) + 
         geom_line (aes(y = df_spread13$spread131), colour='black',  size=0.8) + 
         geom_line (aes(y = df_spread13$mean_13), linetype="dashed", colour='darkgrey',  size=0.8) + 
         geom_ribbon (aes(ymin = (df_spread13$mean_13 - .5 * df_spread13$var_13), 
                          ymax = (df_spread13$mean_13 + .5 * df_spread13$var_13)), alpha=0.6) +
         geom_ribbon (aes(ymin = (df_spread13$mean_13 - 1.5 * df_spread13$var_13), 
                          ymax = (df_spread13$mean_13 + 1.5 * df_spread13$var_13)), alpha=0.4) + 
         geom_ribbon (aes(ymin = (df_spread13$mean_13 - 2 * df_spread13$var_13), 
                          ymax = (df_spread13$mean_13 + 2 * df_spread13$var_13)), alpha=0.2) +
         ggtitle('2013 Gas-Oil Spread and MeanRev IC')

# ---------------------------------------------------------------------
multiplot(p10, p11, p12, p13, cols = 2)
# multiplot(d1, d2, d3, d4, cols = 2)
# ---------------------------------------------------------------------



# ---------------------------------------------------------------------
# Trading rules for 2011 based in the 2010 spread model (out-of-sample)
# ---------------------------------------------------------------------

# if spread is too low:   BUY SPREAD  = long in oil   + short in Gas
# if spread is too high:  SELL SPREAD = short in oil  + long in Gas

# spread11 <- prices11[,4]-prices11[,2]

# ---------------------------------------------------------------------
# Trading rules for 2011 based in the 2010 spread model (out-of-sample)
# ---------------------------------------------------------------------

# if spread is too low:   BUY SPREAD  = long in oil   + short in Gas
# if spread is too high:  SELL SPREAD = short in oil  + long in Gas

# spread11 <- prices11[,4]-prices11[,2]

predict  <- rep(spread111[1], times=length(spread111))




std <- function(beta_0, beta_1, movingvol) {
  out <- beta_0 - beta_1*movingvol
  return(out)
}



# ==================================================================
# Recalculation of Means and Variances
# ==================================================================

beta_0 <- 1.4
beta_1 <- 1


mean_10  <- spread111[1]* exp(-coef(fit10)[2]*t11) + (coef(fit10)[1] / coef(fit10)[2]) * (1-exp(-coef(fit10)[2]*t11))
var_10   <- (coef(fit10)[3]^2 / (2*coef(fit10)[2])) * (1-exp(-2*coef(fit10)[2]*t11))
var_10   <- sqrt(var_10)

# x11 <- data.frame(x11 <- as.Date(prices11$X,format='%d/%m/%Y')
df_prices211 <- data.frame(x11,df_spread11$spread11,df_spread11$spread111, mean_10, var_10)
names(df_prices211)<- c('x', 'spread11', 'spread111', 'mean_10', 'var_10')


p21 <-   ggplot(df_prices211, aes(x = df_prices211$x)) + 
         geom_line (aes(y = df_prices211$spread111), colour='black',  size=0.8) + 
         geom_line (aes(y = df_prices211$mean_10), linetype="dashed", colour='darkgrey',  size=0.8) + 
         geom_ribbon (aes(ymin = (df_prices211$mean_10 - .5 * df_prices211$var_10), 
                          ymax = (df_prices211$mean_10 + .5 * df_prices211$var_10)), alpha=0.6) +
         geom_ribbon (aes(ymin = (df_prices211$mean_10 - 1.5 * df_prices211$var_10), 
                          ymax = (df_prices211$mean_10 + 1.5 * df_prices211$var_10)), alpha=0.4) + 
         geom_ribbon (aes(ymin = (df_prices211$mean_10 - 2 * df_prices211$var_10), 
                          ymax = (df_prices211$mean_10 + 2 * df_prices211$var_10)), alpha=0.2) +
         ggtitle('2011 Gas-Oil Spread and MeanRev IC from 2010')


v_std <- std(beta_0,beta_1,na.fill(MovingVol11,"extend"))

p211 <-  ggplot(df_prices211, aes(x = df_prices211$x)) + 
         geom_line (aes(y = df_prices211$spread111), colour='black',  size=0.8) + 
         geom_line (aes(y = df_prices211$mean_10), linetype="dashed", colour='darkgrey',  size=0.8) + 
         geom_ribbon (aes(ymin = (df_prices211$mean_10 - .5 * df_prices211$var_10), 
                          ymax = (df_prices211$mean_10 + .5 * df_prices211$var_10)), alpha=0.6) +
         geom_ribbon (aes(ymin = (df_prices211$mean_10 - 1.5 * df_prices211$var_10), 
                          ymax = (df_prices211$mean_10 + 1.5 * df_prices211$var_10)), alpha=0.4) + 
         geom_ribbon (aes(ymin = (df_prices211$mean_10 - v_std * df_prices211$var_10), 
                          ymax = (df_prices211$mean_10 + v_std * df_prices211$var_10)), fill='red',alpha=0.2) +
         ggtitle('2011 Gas-Oil Spread and MeanRev IC from 2010')


mean_11  <- spread121[1]* exp(-coef(fit11)[2]*t12) + (coef(fit11)[1] / coef(fit11)[2]) * (1-exp(-coef(fit11)[2]*t12))
var_11   <- (coef(fit11)[3]^2 / (2*coef(fit11)[2])) * (1-exp(-2*coef(fit11)[2]*t12))
var_11   <- sqrt(var_11)

df_prices212 <- data.frame(x12,df_spread12$spread12,df_spread12$spread121, mean_11, var_11)
names(df_prices212)<- c('x', 'spread12', 'spread121', 'mean_11', 'var_11')


p22 <-   ggplot(df_prices212, aes(x = df_prices212$x)) + 
         geom_line (aes(y = df_prices212$spread121), colour='black',  size=0.8) + 
         geom_line (aes(y = df_prices212$mean_11), linetype="dashed", colour='darkgrey',  size=0.8) + 
         geom_ribbon (aes(ymin = (df_prices212$mean_11 - .5 * df_prices212$var_11), 
                          ymax = (df_prices212$mean_11 + .5 * df_prices212$var_11)), alpha=0.6) +
         geom_ribbon (aes(ymin = (df_prices212$mean_11 - 1.5 * df_prices212$var_11), 
                          ymax = (df_prices212$mean_11 + 1.5 * df_prices212$var_11)), alpha=0.4) + 
         geom_ribbon (aes(ymin = (df_prices212$mean_11 - 2 * df_prices212$var_11), 
                          ymax = (df_prices212$mean_11 + 2 * df_prices212$var_11)), alpha=0.2) +
         ggtitle('2012 Gas-Oil Spread and MeanRev IC from 2011')

v_std <- std(beta_0,beta_1,na.fill(MovingVol12,"extend"))

p212 <-  ggplot(df_prices212, aes(x = df_prices212$x)) + 
         geom_line (aes(y = df_prices212$spread121), colour='black',  size=0.8) + 
         geom_line (aes(y = df_prices212$mean_11), linetype="dashed", colour='darkgrey',  size=0.8) + 
         geom_ribbon (aes(ymin = (df_prices212$mean_11 - .5 * df_prices212$var_11), 
                          ymax = (df_prices212$mean_11 + .5 * df_prices212$var_11)), alpha=0.6) +
         geom_ribbon (aes(ymin = (df_prices212$mean_11 - 1.5 * df_prices212$var_11), 
                          ymax = (df_prices212$mean_11 + 1.5 * df_prices212$var_11)), alpha=0.4) + 
         geom_ribbon (aes(ymin = (df_prices212$mean_11 - v_std * df_prices212$var_11), 
                          ymax = (df_prices212$mean_11 + v_std * df_prices212$var_11)), fill = 'red', alpha=0.2) +
         ggtitle('2011 Gas-Oil Spread and MeanRev IC from 2010')

mean_12  <- spread131[1]* exp(-coef(fit12)[2]*t13) + (coef(fit12)[1] / coef(fit12)[2]) * (1-exp(-coef(fit12)[2]*t13))
var_12  <- (coef(fit12)[3]^2 / (2*coef(fit12)[2])) * (1-exp(-2*coef(fit12)[2]*t13))
var_12   <- sqrt(var_12)

df_prices213 <- data.frame(x13,df_spread13$spread13, df_spread13$spread131, mean_12, var_12)
names(df_prices213)<- c('x', 'spread13', 'spread131', 'mean_12', 'var_12')


p23 <-   ggplot(df_prices213, aes(x = df_prices213$x)) + 
         geom_line (aes(y = df_prices213$spread131), colour='black',  size=0.8) + 
         geom_line (aes(y = df_prices213$mean_12), linetype="dashed", colour='darkgrey',  size=0.8) + 
         geom_ribbon (aes(ymin = (df_prices213$mean_12 - .5 * df_prices213$var_12), 
                          ymax = (df_prices213$mean_12 + .5 * df_prices213$var_12)), alpha=0.6) +
         geom_ribbon (aes(ymin = (df_prices213$mean_12 - 1.5 * df_prices213$var_12), 
                          ymax = (df_prices213$mean_12 + 1.5 * df_prices213$var_12)), alpha=0.4) + 
         geom_ribbon (aes(ymin = (df_prices213$mean_12 - 2 * df_prices213$var_12), 
                          ymax = (df_prices213$mean_12 + 2 * df_prices213$var_12)), alpha=0.2) +
         ggtitle('2013 Gas-Oil Spread and MeanRev IC from 2012')

v_std <- std(beta_0, beta_1, na.fill(MovingVol13,"extend"))

p213 <-  ggplot(df_prices213, aes(x = df_prices213$x)) + 
         geom_line (aes(y = df_prices213$spread131), colour='black',  size=0.8) + 
         geom_line (aes(y = df_prices213$mean_12), linetype="dashed", colour='darkgrey',  size=0.8) + 
         geom_ribbon (aes(ymin = (df_prices213$mean_12 - .5 * df_prices213$var_12), 
                          ymax = (df_prices213$mean_12 + .5 * df_prices213$var_12)), alpha=0.6) +
         geom_ribbon (aes(ymin = (df_prices213$mean_12 - 1.5 * df_prices213$var_12), 
                          ymax = (df_prices213$mean_12 + 1.5 * df_prices213$var_12)), alpha=0.4) + 
         geom_ribbon (aes(ymin = (df_prices213$mean_12 - v_std * df_prices213$var_12), 
                          ymax = (df_prices213$mean_12 + v_std * df_prices213$var_12)), fill = 'red', alpha=0.2) +
         ggtitle('2013 Gas-Oil Spread and MeanRev IC from 2012')


multiplot(p21, p22, p23, p211, p212, p213, cols = 2)

# ===============================
# EXPONENTIAL CASE: Std2
# ===============================

beta_0 <- 2.75
beta_1 <- 1.25


std2 <- function(beta_0, beta_1, movingvol) {
  out <- abs(beta_0 * exp(-beta_1*movingvol))
  return(out)
}



p21 <-   ggplot(df_prices211, aes(x = df_prices211$x)) + 
         geom_line (aes(y = df_prices211$spread111), colour='black',  size=0.8) + 
         geom_line (aes(y = df_prices211$mean_10), linetype="dashed", colour='darkgrey',  size=0.8) + 
         geom_ribbon (aes(ymin = (df_prices211$mean_10 - .5 * df_prices211$var_10), 
                          ymax = (df_prices211$mean_10 + .5 * df_prices211$var_10)), alpha=0.6) +
         geom_ribbon (aes(ymin = (df_prices211$mean_10 - 1.5 * df_prices211$var_10), 
                          ymax = (df_prices211$mean_10 + 1.5 * df_prices211$var_10)), alpha=0.4) + 
         geom_ribbon (aes(ymin = (df_prices211$mean_10 - 2 * df_prices211$var_10), 
                          ymax = (df_prices211$mean_10 + 2 * df_prices211$var_10)), alpha=0.2) +
         ggtitle('2011 Gas-Oil Spread and MeanRev IC from 2010')


v_std11 <- std2(beta_0,beta_1,na.fill(MovingVol11,"extend"))

p211 <-  ggplot(df_prices211, aes(x = df_prices211$x)) + 
         geom_line (aes(y = df_prices211$spread111), colour='black',  size=0.8) + 
         geom_line (aes(y = df_prices211$mean_10), linetype="dashed", colour='darkgrey',  size=0.8) + 
         geom_ribbon (aes(ymin = (df_prices211$mean_10 - .5 * df_prices211$var_10), 
                          ymax = (df_prices211$mean_10 + .5 * df_prices211$var_10)), alpha=0.6) +
         geom_ribbon (aes(ymin = (df_prices211$mean_10 - 1.5 * df_prices211$var_10), 
                          ymax = (df_prices211$mean_10 + 1.5 * df_prices211$var_10)), alpha=0.4) + 
         geom_ribbon (aes(ymin = (df_prices211$mean_10 - v_std11 * df_prices211$var_10), 
                          ymax = (df_prices211$mean_10 + v_std11 * df_prices211$var_10)), fill='green',alpha=0.2) +
         ggtitle('2011 Gas-Oil Spread and MeanRev IC from 2010')


#mean_11  <- spread121[1]* exp(-coef(fit11)[2]*t12) + (coef(fit11)[1] / coef(fit11)[2]) * (1-exp(-coef(fit11)[2]*t12))
#var_11   <- (coef(fit11)[3]^2 / (2*coef(fit11)[2])) * (1-exp(-2*coef(fit11)[2]*t12))
#var_11   <- sqrt(var_11)

#df_prices212 <- data.frame(x12,df_spread12$spread12,df_spread12$spread121, mean_11, var_11)
#names(df_prices212)<- c('x', 'spread12', 'spread121', 'mean_11', 'var_11')


p22 <-   ggplot(df_prices212, aes(x = df_prices212$x)) + 
         geom_line (aes(y = df_prices212$spread121), colour='black',  size=0.8) + 
         geom_line (aes(y = df_prices212$mean_11), linetype="dashed", colour='darkgrey',  size=0.8) + 
         geom_ribbon (aes(ymin = (df_prices212$mean_11 - .5 * df_prices212$var_11), 
                          ymax = (df_prices212$mean_11 + .5 * df_prices212$var_11)), alpha=0.6) +
         geom_ribbon (aes(ymin = (df_prices212$mean_11 - 1.5 * df_prices212$var_11), 
                          ymax = (df_prices212$mean_11 + 1.5 * df_prices212$var_11)), alpha=0.4) + 
         geom_ribbon (aes(ymin = (df_prices212$mean_11 - 2 * df_prices212$var_11), 
                          ymax = (df_prices212$mean_11 + 2 * df_prices212$var_11)), alpha=0.2) +
         ggtitle('2012 Gas-Oil Spread and MeanRev IC from 2011')

v_std12 <- std2(beta_0,beta_1,na.fill(MovingVol12,"extend"))

p212 <-  ggplot(df_prices212, aes(x = df_prices212$x)) + 
         geom_line (aes(y = df_prices212$spread121), colour='black',  size=0.8) + 
         geom_line (aes(y = df_prices212$mean_11), linetype="dashed", colour='darkgrey',  size=0.8) + 
         geom_ribbon (aes(ymin = (df_prices212$mean_11 - .5 * df_prices212$var_11), 
                          ymax = (df_prices212$mean_11 + .5 * df_prices212$var_11)), alpha=0.6) +
         geom_ribbon (aes(ymin = (df_prices212$mean_11 - 1.5 * df_prices212$var_11), 
                          ymax = (df_prices212$mean_11 + 1.5 * df_prices212$var_11)), alpha=0.4) + 
         geom_ribbon (aes(ymin = (df_prices212$mean_11 - v_std12 * df_prices212$var_11), 
                          ymax = (df_prices212$mean_11 + v_std12 * df_prices212$var_11)), fill = 'red', alpha=0.2) +
         ggtitle('2012 Gas-Oil Spread and MeanRev IC from 2011')

#mean_12  <- spread131[1]* exp(-coef(fit12)[2]*t13) + (coef(fit12)[1] / coef(fit12)[2]) * (1-exp(-coef(fit12)[2]*t13))
#var_12  <- (coef(fit12)[3]^2 / (2*coef(fit12)[2])) * (1-exp(-2*coef(fit12)[2]*t13))
#var_12   <- sqrt(var_12)

#df_prices213 <- data.frame(x13,df_spread13$spread13, df_spread13$spread131, mean_12, var_12)
#names(df_prices213)<- c('x', 'spread13', 'spread131', 'mean_12', 'var_12')


p23 <-   ggplot(df_prices213, aes(x = df_prices213$x)) + 
         geom_line (aes(y = df_prices213$spread131), colour='black',  size=0.8) + 
         geom_line (aes(y = df_prices213$mean_12), linetype="dashed", colour='darkgrey',  size=0.8) + 
         geom_ribbon (aes(ymin = (df_prices213$mean_12 - .5 * df_prices213$var_12), 
                          ymax = (df_prices213$mean_12 + .5 * df_prices213$var_12)), alpha=0.6) +
         geom_ribbon (aes(ymin = (df_prices213$mean_12 - 1.5 * df_prices213$var_12), 
                          ymax = (df_prices213$mean_12 + 1.5 * df_prices213$var_12)), alpha=0.4) + 
         geom_ribbon (aes(ymin = (df_prices213$mean_12 - 2 * df_prices213$var_12), 
                          ymax = (df_prices213$mean_12 + 2 * df_prices213$var_12)), alpha=0.2) +
         ggtitle('2013 Gas-Oil Spread and MeanRev IC from 2012')

v_std13 <- std2(beta_0, beta_1, na.fill(MovingVol13,"extend"))

p213 <-  ggplot(df_prices213, aes(x = df_prices213$x)) + 
         geom_line (aes(y = df_prices213$spread131), colour='black',  size=0.8) + 
         geom_line (aes(y = df_prices213$mean_12), linetype="dashed", colour='darkgrey',  size=0.8) + 
         geom_ribbon (aes(ymin = (df_prices213$mean_12 - .5 * df_prices213$var_12), 
                          ymax = (df_prices213$mean_12 + .5 * df_prices213$var_12)), alpha=0.6) +
         geom_ribbon (aes(ymin = (df_prices213$mean_12 - 1.5 * df_prices213$var_12), 
                          ymax = (df_prices213$mean_12 + 1.5 * df_prices213$var_12)), alpha=0.4) + 
         geom_ribbon (aes(ymin = (df_prices213$mean_12 - v_std13 * df_prices213$var_12), 
                          ymax = (df_prices213$mean_12 + v_std13 * df_prices213$var_12)), fill = 'blue', alpha=0.2) +
         ggtitle('2013 Gas-Oil Spread and MeanRev IC from 2012')


multiplot(p21, p22, p23, p211, p212, p213, cols = 2)

# =====================================================================================
## TRADING RULES
# =====================================================================================


# MovingVol10_2 <- na.fill(volatility(spread10, n=20, calc='close'),"extend")
MovingVol11_2 <- na.fill(volatility(spread11, n=20, calc='close'),"extend")
MovingVol12_2 <- na.fill(volatility(spread12, n=20, calc='close'),"extend")
MovingVol13_2 <- na.fill(volatility(spread13, n=20, calc='close'),"extend")

v_std11 <- std2(beta_0,beta_1,MovingVol11_2)
v_std12 <- std2(beta_0,beta_1,MovingVol12_2)
v_std13 <- std2(beta_0,beta_1,MovingVol13_2)

position11_f    <- rep (0, times=length(spread111) )
position11_m2m  <- rep (0, times=length(spread111) )
results11       <- rep (0, times=length(spread111) )

# account11  <- rep (0, times=length(spread11) )

for (i in 2:length(spread111)) {
  
  predict[i] <- coef(fit10)[1] / coef(fit10)[2] + ( spread111[i-1] -
                           coef(fit10)[1] / coef(fit10)[2]) * exp ( -coef(fit10)[2])
  
  # ================================================================================
  # OPENING POSITIONS ------------------------------------------------------------
  # ================================================================================
  # if spread is too high:  SELL SPREAD = short in oil  + long in Gas
  
  if  (position11_f[i-1] == 0 && spread111[i] > ( mean_10[i] + v_std11[i] * var_10[i] )) 
  {
    position11_f[i]    = spread11[i]
    position11_m2m[i]  = position11_m2m[i-1] 
  }
  
  # if spread is too low:   BUY SPREAD  = long in oil   + short in Gas
  
  if  (position11_f[i-1] == 0 && spread111[i] < ( mean_10[i] - v_std11[i] * var_10[i] )) 
  {
    position11_f[i]    = - spread11[i]
    position11_m2m[i]  = position11_m2m[i-1] 
  }
  
  # ================================================================================
  # WAITING THE POSITIONS ------------------------------------------------------------  
  # ================================================================================
  if  (position11_f[i-1] > 0 && (spread111[i] > ( mean_10[i] - v_std11[i] * var_10[i] )))  
  {
    position11_f[i] = position11_f[i-1]
    position11_m2m[i] = position11_m2m[i-1] + (- spread11[i] + spread11[i-1])
  }
  
  if  (position11_f[i-1] < 0 && (spread111[i] < ( mean_10[i] + v_std11[i] * var_10[i] )))  
  {
    position11_f[i] = position11_f[i-1]
    position11_m2m[i] = position11_m2m[i-1] + ( spread11[i] - spread11[i-1])
  }
  
  # ================================================================================
  # CLOSING POSITIONS ------------------------------------------------------------
  # ================================================================================
  # Closing short position
  
  if  (position11_f[i-1] > 0 && (spread111[i] < ( mean_10[i] - v_std11[i] * var_10[i] )))  
  {
    results11[i] = position11_f[i-1] - spread11[i]
    position11_f[i] = -spread11[i]
    position11_m2m[i] = 0
  }
  
  # Closing long position
  
  if  (position11_f[i-1] < 0 && (spread111[i] > ( mean_10[i] + v_std11[i] * var_10[i] )))  
  {
    results11[i] = spread11[i] + position11_f[i-1] 
    position11_f[i] = spread11[i]
    position11_m2m[i] = 0
  }
}



# ================================================================================
# Trading rules for 2012 based in the 2011 spread model (out-of-sample)
# ================================================================================

# if spread is too low:   BUY SPREAD  = long in oil   + short in Gas
# if spread is too high:  SELL SPREAD = short in oil  + long in Gas

# spread12 <- prices12[,4]-prices12[,2]

predict  <- rep(spread12[1], times=length(spread12))

t <- rep(1:length(spread12))


position12_f    <- rep (0, times=length(spread12) )
position12_m2m  <- rep (0, times=length(spread12) )
results12       <- rep (0, times=length(spread12) )

# account11  <- rep (0, times=length(spread11) )

for (i in 2:length(spread12)) {
  
  predict[i] <- coef(fit11)[1] / coef(fit11)[2] + ( spread12[i-1] -
                                                      coef(fit11)[1] / coef(fit11)[2]) * exp ( -coef(fit11)[2])
  
  # OPENING POSITIONS ------------------------------------------------------------
  
  # if spread is too high:  SELL SPREAD = short in oil  + long in Gas
  
  if  (position12_f[i-1] == 0 && spread121[i] > ( mean_11[i] + v_std12[i]* var_11[i] )) 
  {
    position12_f[i]    = spread12[i]
    position12_m2m[i]  = position12_m2m[i-1] 
  }
  
  # if spread is too low:   BUY SPREAD  = long in oil   + short in Gas
  
  if  (position12_f[i-1] == 0 && spread121[i] < ( mean_11[i] - v_std12[i] * var_11[i] )) 
  {
    position12_f[i]    = - spread12[i]
    position12_m2m[i]  = position12_m2m[i-1] 
  }
  
  # WAITING THE POSITIONS ------------------------------------------------------------  
  
  if  (position12_f[i-1] > 0 && (spread121[i] > ( mean_11[i] - v_std12[i] * var_11[i] )))  
  {
    position12_f[i] = position12_f[i-1]
    position12_m2m[i] = position12_m2m[i-1] + (- spread12[i] + spread12[i-1])
  }
  
  if  (position12_f[i-1] < 0 && (spread121[i] < ( mean_11[i] + v_std12[i] * var_11[i] )))  
  {
    position12_f[i] = position12_f[i-1]
    position12_m2m[i] = position12_m2m[i-1] + ( spread12[i] - spread12[i-1])
  }
  
  # CLOSING POSITIONS ------------------------------------------------------------
  
  # Closing short position
  
  if  (position12_f[i-1] > 0 && (spread121[i] < ( mean_11[i] - v_std12[i] * var_11[i] )))  
  {
    results12[i]    = position12_f[i-1] - spread12[i]
    position12_f[i] = -spread12[i]
    position12_m2m[i] = 0
  }
  
  # Closing long position
  
  if  (position12_f[i-1] < 0 && (spread121[i] > ( mean_11[i] + v_std12[i] * var_11[i] )))  
  {
    results12[i]    = spread12[i] + position12_f[i-1] 
    position12_f[i] = spread12[i]
    position12_m2m[i] = 0
  }
}



# ================================================================================
# Trading rules for 2013 based in the 2012 spread model (out-of-sample)
# ================================================================================

# if spread is too low:   BUY SPREAD  = long in oil   + short in Gas
# if spread is too high:  SELL SPREAD = short in oil  + long in Gas

# spread12 <- prices12[,4]-prices12[,2]

predict  <- rep(spread13[1], times=length(spread13))

position13_f    <- rep (0, times=length(spread13) )
position13_m2m  <- rep (0, times=length(spread13) )
results13       <- rep (0, times=length(spread13) )

# account11  <- rep (0, times=length(spread11) )

for (i in 2:length(spread13)) {
  
  predict[i] <- coef(fit12)[1] / coef(fit12)[2] + ( spread13[i-1] -
                                                      coef(fit12)[1] / coef(fit12)[2]) * exp ( -coef(fit12)[2])
  # ================================================================================
  # OPENING POSITIONS ------------------------------------------------------------
  # ================================================================================
  # if spread is too high:  SELL SPREAD = short in oil  + long in Gas
  
  if  (position12_f[i-1] == 0 && spread131[i] > ( mean_12[i] + v_std13[i] * var_12[i] )) 
  {
    position13_f[i]    = spread13[i]
    position13_m2m[i]  = position13_m2m[i-1] 
  }
  
  # if spread is too low:   BUY SPREAD  = long in oil   + short in Gas
  
  if  (position13_f[i-1] == 0 && spread131[i] < ( mean_12[i] - v_std13[i] * var_12[i] )) 
  {
    position13_f[i]    = - spread13[i]
    position13_m2m[i]  = position13_m2m[i-1] 
  }
  # ================================================================================
  # WAITING THE POSITIONS ------------------------------------------------------------  
  # ================================================================================
  if  (position13_f[i-1] > 0 && (spread131[i] > ( mean_12[i] - v_std13[i] * var_12[i] )))  
  {
    position13_f[i] = position13_f[i-1]
    position13_m2m[i] = position13_m2m[i-1] + (- spread13[i] + spread13[i-1])
  }
  
  if  (position13_f[i-1] < 0 && (spread131[i] < ( mean_12[i] + v_std13[i] * var_12[i] )))  
  {
    position13_f[i] = position13_f[i-1]
    position13_m2m[i] = position13_m2m[i-1] + ( spread13[i] - spread13[i-1])
  }
  # ================================================================================
  # CLOSING POSITIONS ------------------------------------------------------------
  # ================================================================================
  # Closing short position
  
  if  (position13_f[i-1] > 0 && (spread131[i] < ( mean_12[i] - v_std13[i] * var_12[i] )))  
  {
    results13[i]    = position13_f[i-1] - spread13[i]
    position13_f[i] = -spread13[i]
    position13_m2m[i] = 0
  }
  
  # Closing long position
  
  if  (position13_f[i-1] < 0 && (spread131[i] > ( mean_12[i] + v_std13[i] * var_12[i] )))  
  {
    results13[i]    = spread13[i] + position13_f[i-1] 
    position13_f[i] = spread13[i]
    position13_m2m[i] = 0
  }
}

# ================================================================================
# PLOTTING THE RESULTS
# ================================================================================


#par(mfrow=c(3,1))
#plot.ts(cumsum(results10))
# x10 <- as.Date(prices10$X,format='%d/%m/%Y')
cum_results11 <- cumsum(results11)
cum_results12 <- cumsum(results12)
cum_results13 <- cumsum(results13)

dposition11_m2m <- c(0, diff(position11_m2m))
dposition12_m2m <- c(0, diff(position12_m2m))
dposition13_m2m <- c(0, diff(position13_m2m))

dspread11 <- c(0,diff(spread11))
dspread12 <- c(0,diff(spread12))
dspread13 <- c(0,diff(spread13))

pl11 <- as.ts(cumsum(results11) + position11_m2m)
pl12 <- as.ts(cumsum(results12) + position12_m2m)
pl13 <- as.ts(cumsum(results13) + position13_m2m)

df_results11 <- data.frame( x11, dspread11, cum_results11, pl11, dposition11_m2m)
df_results12 <- data.frame( x12, dspread12, cum_results12, pl12, dposition12_m2m)
df_results13 <- data.frame( x13, dspread13, cum_results13, pl13, dposition13_m2m) 

pp1<- ggplot(df_results11, aes(x=x11))+
      geom_line(aes(y=cum_results11),colour="darkgreen", size=1)+
      geom_line(aes(y=pl11),colour="darkgreen", size=0.5)+
      ggtitle("M2M Position and Realized P&L for 2011")
pp2<- ggplot(df_results12, aes(x=x12))+
      geom_line(aes(y=cum_results12),colour="darkred", size=1)+
      geom_line(aes(y=pl12),colour="darkred", size=0.5)+
      ggtitle("M2M Position and Realized P&L for 2012")
pp3<- ggplot(df_results13, aes(x=x13))+
      geom_line(aes(y=cum_results13),colour="darkblue", size=1)+
      geom_line(aes(y=pl13),colour="darkblue", size=0.5)+
      ggtitle("M2M Position and Realized P&L for 2013")

#pp2<-ggplot(df_results12, aes(x12,cum_results12))+
#      geom_line(colour="darkred", size=1)+ggtitle("Realized P&L for 2012")
#pp3<-ggplot(df_results13, aes(x13,cum_results13))+
#      geom_line(colour="darkblue", size=1)+ggtitle("Realized P&L for 2013")

pp4<-ggplot(df_results11, aes(df_results11$x11,df_results11$dposition11_m2m))+geom_line(colour="darkgreen", size=1)+ggtitle("Daily Returns for 2011")
pp5<-ggplot(df_results12, aes(x12,dposition12_m2m))+geom_line(colour="darkred", size=1)+ggtitle("Daily Returns for 2012")
pp6<-ggplot(df_results13, aes(x13,dposition13_m2m))+geom_line(colour="darkblue", size=1)+ggtitle("Daily Returns for 2013")

pp7<-ggplot(df_results11, aes(dposition11_m2m))+
      geom_density(aes(x=dspread11),fill="darkgreen", alpha=0.6)+
      geom_density(aes(x=dposition11_m2m),fill="green", alpha=0.4)+
      ggtitle("Density of D.Returns Mkt vs. Strat for 2011")

pp8<-ggplot(df_results12, aes(dposition12_m2m))+
    geom_density(aes(x=dspread12),fill="darkred", alpha=0.6)+
    geom_density(aes(x=dposition12_m2m),fill="red", alpha=0.4)+
    ggtitle("Density of D.Returns Mkt vs. Strat for 2012")

pp9<-ggplot(df_results13, aes(dposition13_m2m))+
    geom_density(aes(x=dspread13),fill="darkblue", alpha=0.6)+
    geom_density(aes(x=dposition13_m2m),fill="blue", alpha=0.4)+
    ggtitle("Density of D.Returns Mkt vs. Strat for 2013")

multiplot(p211, p212, p213, pp1, pp2, pp3, pp7, pp8, pp9, cols=3)
#multiplot(pp4,pp5,pp6, columns=1)




par(mfrow=c(3,1))
ts.plot(as.ts(spread11), main='Actual Spread 2011')
ts.plot( as.ts(spread111), as.ts(mean_10), as.ts(mean_10 + v_std11 * var_10), as.ts(mean_10 - v_std11 * var_10), 
         main='2011 Spread without Local Drift')
ts.plot(as.ts(cumsum(results11) + position11_m2m), as.ts(cumsum(results11)), main = 'P&L 2011')

par(mfrow=c(3,1))
ts.plot(as.ts(spread12), main='Actual Spread 2012')
ts.plot( as.ts(spread121), as.ts(mean_11), as.ts(mean_11 + v_std12 * var_11), as.ts(mean_11 - v_std12 * var_11), 
         main='2012 Spread without Local Drift')
ts.plot(as.ts(cumsum(results12) + position12_m2m), as.ts(cumsum(results12)), main = 'P&L 2012')

par(mfrow=c(3,1))
ts.plot(as.ts(spread13), main='Actual Spread 2013')
ts.plot( as.ts(spread131), as.ts(mean_12), as.ts(mean_12 + v_std13 * var_12), as.ts(mean_12 - v_std13 * var_12), 
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

