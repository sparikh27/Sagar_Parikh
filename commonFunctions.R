#common functions file
#libraries####
library(mosaic)
library(R2PPT)
library(R2HTML)
library(fImport)
library(ggmap)
library(RODBC)
library(RMySQL)
library(Amelia)
library(gmodels)
library(polycor)
library(ggm)
library(quantmod)
library(XML)
library(rjson)
#functions####

#reverse - will reverse the order of the data points in a vector
reverse = function(vect) {
  toreturn = c()
  for(i in 1:length(vect))
  {
    index = length(vect) - i + 1
    toreturn = c(toreturn, vect[index])
  }
  return(toreturn)
}

#pctreturn - will return the percentage increase between each data point in a vector (a 2% increase will return 0.02)
pctreturn = function(vect){
  return((vect[-1]-vect[-length(vect)])/vect[-length(vect)])
}

#pcttracker - will return the cumulative percentage return of a vector (a 10% gain and a 10% loss will return c(1.1,0.99))
pcttracker = function(vect){
  vect = vect + 1
  toreturn = c(vect[1])
  for(i in 2:length(vect))
  {
    toreturn = c(toreturn, toreturn[i-1]*vect[i])
  }
  return(toreturn)
}

#indexcompare - will compare the returns of a stock to the returns of an index over a specified period
indexcompare = function(ticker, index, fromdate, todate){
  if(todate == "today")
    todate = Sys.timeDate()
  tickerdata = pcttracker(pctreturn(reverse(data.frame(yahooSeries(c(ticker), from = fromdate, to = todate))[,6])))
  indexdata = pcttracker(pctreturn(reverse(data.frame(yahooSeries(c(index), from = fromdate, to = todate))[,6])))
  plot(tickerdata, col = "white")
  lines(tickerdata, col = "red")
  lines(indexdata, col = "black")
  toreturn = data.frame(cbind(tickerdata,indexdata))
  colnames(toreturn) = c(paste(ticker, "data", sep = ""),paste(index, "data", sep=""))
  return(toreturn)
}

#multicompare - will compare the returns of AGG, TLT, and SPY to the returns of a ticker
multicompare = function(ticker, fromdate, todate){
  if(todate == "today")
    todate = Sys.timeDate()
  tickerdata = pcttracker(pctreturn(reverse(data.frame(yahooSeries(c(ticker), from = fromdate, to = todate))[,6])))
  indexdata = pcttracker(pctreturn(reverse(data.frame(yahooSeries(c("SPY"), from = fromdate, to = todate))[,6])))
  indexdata2 = pcttracker(pctreturn(reverse(data.frame(yahooSeries(c("AGG"), from = fromdate, to = todate))[,6])))
  indexdata3 = pcttracker(pctreturn(reverse(data.frame(yahooSeries(c("TLT"), from = fromdate, to = todate))[,6])))
  plot(tickerdata, col = "white")
  lines(tickerdata, col = "red")
  lines(indexdata, col = "black")
  lines(indexdata2, col = "blue") #AGG
  lines(indexdata3, col = "green") #TLT
  toreturn = data.frame(cbind(tickerdata,SPYdata = indexdata,AGGdata = indexdata2, TLTdata = indexdata3))
  colnames(toreturn) = c(paste(ticker, "data", sep=""),"SPYdata","AGGdata","TLTdata")
  return(toreturn)
}

#stratcompare - will compare the compiled returns of a non-cumulative (i.e., pcttracker has not been used on the data series) data series against an index over a specified period
stratcompare = function(stratdata, index, fromdate, todate){
  if(todate == "today")
    todate = Sys.timeDate()
  tickerdata = pcttracker(stratdata)
  indexdata = pcttracker(pctreturn(reverse(data.frame(yahooSeries(c(index), from = fromdate, to = todate))[,6])))
  plot(tickerdata, col = "white")
  lines(tickerdata, col = "red")
  lines(indexdata, col = "black")
  toreturn = data.frame(cbind(tickerdata,indexdata))
  colnames(toreturn) = c(paste(ticker, "data", sep = ""),paste(index, "data", sep=""))
  return(toreturn)
}

#multistratcompare - will compare the returns of a non-cumulative data series to the returns of AGG, TLT, and SPY
multistratcompare = function(stratdata, fromdate, todate){
  if(todate == "today")
    todate = Sys.timeDate()
  tickerdata = stratdata
  indexdata = pcttracker(pctreturn(reverse(data.frame(yahooSeries(c("SPY"), from = fromdate, to = todate))[,6])))
  indexdata2 = pcttracker(pctreturn(reverse(data.frame(yahooSeries(c("AGG"), from = fromdate, to = todate))[,6])))
  indexdata3 = pcttracker(pctreturn(reverse(data.frame(yahooSeries(c("TLT"), from = fromdate, to = todate))[,6])))
  plot(tickerdata, col = "white")
  lines(tickerdata, col = "red")
  lines(indexdata, col = "black")
  lines(indexdata2, col = "blue") #AGG
  lines(indexdata3, col = "green") #TLT
  return(data.frame(cbind(stratdata,SPYdata = indexdata,AGGdata = indexdata2, TLTdata = indexdata3)))
}

#getcalls - will retrieve the call sequence for a single ticker from Yahoo! Finance
getcalls = function(ticker, expiry){
  if(expiry == "")
  {
    opchain = getOptionChain(ticker)
  }
  else if(expiry == "NULL")
  {
    opchain = getOptionChain(ticker, Exp = NULL)
  }
  else
  {
    opchain = getOptionChain(ticker, Exp = expiry)
  }
  return(data.frame(opchain[1]))
}

#getputs - will retrieve the call sequence for a single ticker from Yahoo! Finance
getputs = function(ticker, expiry){
  if(expiry == "")
  {
    opchain = getOptionChain(ticker)
  }
  else if(expiry == "NULL")
  {
    opchain = getOptionChain(ticker, Exp = NULL)
  }
  else
  {
    opchain = getOptionChain(ticker, Exp = expiry)
  }
  return(data.frame(opchain[2]))
}

#fsgrabber - will retrieve an annual financial statement (BS, IS, or CF) for a particular ticker for the most recent 4 years reported
fsgrabber = function(ticker, statement){
  fins = na.omit(viewFinancials(get(getFinancials(ticker)),type = c(statement),period = c('A')))
  return(data.frame(fins))
}

#fssetgrabber - will retrieve all cleaned financial statements in form of a list of 3 data frames (ordered balance sheet, income statement, cash flow)
fssetgrabber = function(ticker){
  incstatement = fsgrabber(ticker, "IS")
  balsheet = fsgrabber(ticker,"BS")
  cashflow = fsgrabber(ticker,"CF")
  toReturn = list(balsheet,incstatement,cashflow)
  return(toReturn)
}

#bookvalue - will retrieve the balance sheet book value per share
bookvalue = function(ticker){
  data = fsgrabber(ticker, "BS")
  return(as.numeric(data["Total Equity",1]/data["Total Common Shares Outstanding",1]))
}

#price - will retrieve the current price of a stock
price = function(ticker){
  data = yahooSeries(ticker, nDaysBack = 3)
  price = data[1,4]
}
