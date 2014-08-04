
library(quantmod)
library(TTR)

getSymbols("GS") 

barChart(GS,theme='white.mono',bar.type='hlc') 

candleChart(GS,multi.col=TRUE,theme='white')

lineChart(GS,line.type='h',TA=NULL) 

# the whole series
chartSeries(GS) 
addMACD()
addBBands()
# (December '07 to the last observation in '08) 
candleChart(GS,subset='2012-12::2013-12') 

 # slightly different syntax - after the fact. 
 # also changing the x-axis labeling 

candleChart(GS,theme='white', type='candles') 
reChart(major.ticks='months',subset='last 16 weeks') 

# Now with some indicators applied  
#chartSeries(GS, theme="white", TA="addVo();addBBands();addCCI()") 

# The same result could be accomplished a 
# bit more interactively: 
# 
chartSeries(GS, theme="white") #draw the chart 
addVo() #add volume 
addBBands() #add Bollinger Bands
addCCI() #add Commodity Channel Index

getSymbols("YHOO") #Yahoo! OHLC from yahoo
chartSeries(YHOO, TA=NULL) 

#Then add the Open to Close price change 
#using the quantmod OpCl function 

addTA(OpCl(YHOO),col='blue', type='h') 

addOpCl <- newTA(OpCl,col='green',type='h') 

addOpCl() 