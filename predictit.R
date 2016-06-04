#common functions file copy through line 167
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

#libraries, functions, and files, in rough order of dependencies####

library(rjson)
library(mailR)

#emails: vector of all email recipients
emails = c("sparikh27@gmail.com", "niraj.parekh@utexas.edu","anncai@utexas.edu","danielchen5@gmail.com")

#getdata - extracts the current trade data from PredictIt using a given ticker (string)
getdata = function(ticker){
  url = paste("https://www.predictit.org/api/marketdata/ticker/",ticker, sep="")
  data = readLines(url)
  cleandata = fromJSON(data)
  cleandata = cleandata$Contracts
  filler = c("0","0","0","0","0","0","0")
  datasummary = data.frame(rbind(filler), stringsAsFactors = FALSE)
  for(i in 2:length(cleandata))
  {
    datasummary = rbind(datasummary, filler, stringsAsFactors = FALSE)
  }
  cols = c("Name", "Last Trade Price", "Buy Yes Cost", "Buy No Cost", "Sell Yes Cost", "Sell No Cost", "Last Close Price")
  datapointindices = c(5,10,11,12,13,14,15)
  
  for(i in 1:length(cleandata))
  {
    for(j in 1:length(datapointindices))
    {
      datasummary[i,j] = as.character(cleandata[[i]][datapointindices[j]])
    }
  }
  trunc = length(cleandata)
  cuts = length(datasummary[,1])
  if(cuts>trunc)
  {
    for(i in 1:(cuts-trunc))
    {
      datasummary = datasummary[-cuts-1+i,]
    }  
  }
  rownames(datasummary) = c(1:length(datasummary[,1]))
  colnames(datasummary) = cols
  returndata = FALSE
  for(i in 1:length(datasummary[,1]))
  {
    if((datasummary[i,4]) != "NULL")
    {
      returndata = TRUE
    }
  }
  if(returndata)
    return(datasummary)
  if(!returndata)
    return("NULL")
}

#arbcalcamt - calculates the dollar value of the arbitrage opportunity on a given ticker (string)
arbcalcamt = function(ticker){
  data = suppressWarnings(getdata(ticker))
  if(data == "NULL")
    return()
  if(length(data[,1]) == 1)
    return()
  if(length(data[,1]) > 1)
  {
    data = data[,-7]
    data = data[,-6]
    data = data[,-5]
    data = data[,-2]
    payoff = 1
    yescol = data[,2]
    nocol = data[,3]
    datayes = data
    datano = data
    yesremove = c()
    for(i in 1:length(yescol))
      if(yescol[i] == "NULL")
        yesremove = c(yesremove, i)
    noremove = c()
    for(i in 1:length(nocol))
      if(nocol[i] == "NULL")
        noremove = c(noremove, i)
    if(length(yesremove)>0)
    {
      for(i in 1:length(yesremove))
      {
        yescol = yescol[-yesremove[i]]
        datayes = datayes[-yesremove[i],]
        yesremove = yesremove-1
      }
    }
    if(length(noremove)>0)
    {
      for(i in 1:length(noremove))
      {
        nocol = nocol[-noremove[i]]
        datano = datano[-noremove[i],]
        noremove = noremove-1    
      }
    }
    
    fee = 0.1        
    #     #arbitrage on yes
    
    #     yescol = as.numeric(yescol)
    #     yescol = sort(yescol, decreasing = TRUE)
    #     revsyes = 1 - max(yescol)
    #     lossyes = sum(yescol[-1])
    #     grossprofityes = revsyes*(1-fee)
    #     arbyes = grossprofityes - lossyes
    #     if(arbyes>0)
    #       print(paste(ticker, ": Potential Arbitrage on Yes || ", "Size: ", round(arbyes,5)*100, " cents", sep = ""))
    #arbitrage on no
    nocol = as.numeric(nocol)
    nocol = sort(nocol, decreasing = TRUE)
    lossno = nocol[1]
    nocol = nocol[-1]
    revsno = length(nocol) - sum(nocol)
    grossprofitno = revsno*(1-fee)
    arbno = grossprofitno-lossno
    if(arbno>0)
      return(arbno)
    return(0)
  }
}

#setvalid - enforces that tickers be actively traded and not "maxed" before being stored in a local frame
setvalid = function(tickers, maxedtickers){
  tickerlist = data.frame(as.character(as.vector(tickers[,1])))
  j = 1
  term = length(tickers[,1])
  for(i in 1:term)
  {
    if(j > length(tickerlist[,1]))
      break
    if(is.null(arbcalcamt(tickers[i,1])))
    {
      tickerlist = data.frame(tickerlist[-j,])
      j = j - 1
    }
    j = j + 1
  }
  maxedmarkets = data.frame(as.character(as.vector(maxedtickers[,1])))
  for(i in 1:length(maxedmarkets[,1]))
  {
    for(j in 1:length(tickerlist[,1]))
    {
      tobreak = FALSE
      if(as.character(tickerlist[j,1]) == as.character(maxedmarkets[i,1]))
      {
        tickerlist = data.frame(tickerlist[-j,])
        tobreak = TRUE
      }
      if(tobreak)
        break
    }
  }
  return(tickerlist)
}

#tickers: file of all active and unmaximized tickers
updatetickers = function(){
  return(suppressWarnings(setvalid(read.table("~/Personal/Trading/PredictItActiveTickers.csv", quote="\"", comment.char=""),read.table("~/Personal/Trading/maxedmarkets.csv", quote="\"", comment.char=""))))
}

#arbcalc - determines whether or not there is an arbitrage opportunity on a given ticker (string)
arbcalc = function(ticker){
  data = suppressWarnings(getdata(ticker))
  if(data == "NULL")
    return()
  if(length(data[,1]) == 1)
    return()
  if(length(data[,1]) > 1)
  {
    data = data[,-7]
    data = data[,-6]
    data = data[,-5]
    data = data[,-2]
    payoff = 1
    yescol = data[,2]
    nocol = data[,3]
    datayes = data
    datano = data
    yesremove = c()
    for(i in 1:length(yescol))
      if(yescol[i] == "NULL")
        yesremove = c(yesremove, i)
    noremove = c()
    for(i in 1:length(nocol))
      if(nocol[i] == "NULL")
        noremove = c(noremove, i)
    if(length(yesremove)>0)
    {
      for(i in 1:length(yesremove))
      {
        yescol = yescol[-yesremove[i]]
        datayes = datayes[-yesremove[i],]
        yesremove = yesremove-1
      }
    }
    if(length(noremove)>0)
    {
      for(i in 1:length(noremove))
      {
        nocol = nocol[-noremove[i]]
        datano = datano[-noremove[i],]
        noremove = noremove-1    
      }
    }
    
    fee = 0.1        
    #     #arbitrage on yes
    
    #     yescol = as.numeric(yescol)
    #     yescol = sort(yescol, decreasing = TRUE)
    #     revsyes = 1 - max(yescol)
    #     lossyes = sum(yescol[-1])
    #     grossprofityes = revsyes*(1-fee)
    #     arbyes = grossprofityes - lossyes
    #     if(arbyes>0)
    #       print(paste(ticker, ": Potential Arbitrage on Yes || ", "Size: ", round(arbyes,5)*100, " cents", sep = ""))
    #arbitrage on no
    nocol = as.numeric(nocol)
    nocol = sort(nocol, decreasing = TRUE)
    lossno = nocol[1]
    nocol = nocol[-1]
    revsno = length(nocol) - sum(nocol)
    grossprofitno = revsno*(1-fee)
    arbno = grossprofitno-lossno
    if(arbno>0)
      print(paste(ticker, ": Arbitrage on No || ", "Size: ",round(arbno,5)*100, " cents", sep = ""))
  }
}

#hedgedarbvalue - determines the size of the hedged arbitrage on a given ticker assuming no shares have been previously purchased
hedgedarbvalue = function(ticker){
  data = suppressWarnings(getdata(ticker))
  if(data == "NULL")
    return()
  buynodata = data.frame(cbind(data[,1],data[,4]))
  remove = c()
  for(i in 1:length(buynodata[,1]))
  {
    if(as.character(buynodata[i,2]) == "NULL")
      remove = c(remove, i)
  }
  if(length(remove)>0)
    for(i in 1:length(remove))
    {
      buynodata = buynodata[-remove[i],]
      remove = remove - 1
    }
  buyno = as.numeric(as.character(buynodata[,2]))
  shares = trunc(850/max(buyno),0)
  ifyes = -1*shares*buyno
  ifno = 0.9*(1-buyno)*shares
  risk = c()
  for(i in 1:length(ifyes))
    risk = c(risk, sum(ifno) + ifyes[i] - ifno[i])
  adj = min(risk)
  payoff = risk - adj
  shares = trunc(payoff/(buyno+0.9*(1-buyno)),0) + shares
  ifyes = -1*shares*buyno
  ifno = 0.9*(1-buyno)*shares
  risk = c()
  for(i in 1:length(ifyes))
    risk = c(risk, sum(ifno) + ifyes[i] - ifno[i])
  adj = min(risk)
  return(adj)
}
  
#kellycriterion - approximates the long-run wealth-maximizing position size of a particular arbitrage at a given point in time
kellycriterion = function(ticker){
  reference = hedgedarbvalue(ticker)
  tickerdata = read.table("~/Personal/Trading/TickerHistory.csv",stringsAsFactors = FALSE)
  index = 0
  for(i in 1:length(tickerdata[,1]))
    if(tickerdata[i,1] == ticker)
    {
      index = i
      break
    }
  relevantrow = tickerdata[index,]
  relevantrow = relevantrow[-1]
  relevantrow = as.numeric(as.vector(sort(relevantrow,decreasing = FALSE)))
  newrelrow = c()
  for(i in 1:length(relevantrow))
  {
    if(relevantrow[i] > 0)
      newrelrow = c(newrelrow, relevantrow[i])
  }
  relevantrow = newrelrow
  values = length(relevantrow)
  flag = 0
  if(values>0)
  {
    for(i in 1:values)
    {
      if(reference < relevantrow[i])
      {
        flag = i
        break
      }
    }
  }
  if(flag == 0)
    return(0)
  if(values == 0)
    return(1)
  percentile = (flag/values)
  kelly = 2*percentile - 1
  if(kelly > 0)
    return(kelly)
  return(0)
}

#loadhistory - loads the history of PredictIt hedged arbitrages
loadhistory = function(){
  return(read.table("~/Personal/Trading/TickerHistory.csv",stringsAsFactors = FALSE))
}

#checkhedgedarb - determines whether or not and what quantity of hedged arbitrage value there is on a ticker, subtracting any already-received gains from previous purchases
checkhedgedarb = function(ticker,sharequant, emails, emailgate){
  data = suppressWarnings(getdata(ticker))
  if(data == "NULL")
    return()
  buynodata = data.frame(cbind(data[,1],data[,4]))
  remove = c()
  for(i in 1:length(buynodata[,1]))
  {
    if(as.character(buynodata[i,2]) == "NULL")
      remove = c(remove, i)
  }
  if(length(remove)>0)
  {
    for(i in 1:length(remove))
    {
      buynodata = buynodata[-remove[i],]
      remove = remove - 1
    }
  }
  buyno = as.numeric(as.character(buynodata[,2]))
  shares = trunc(850/max(buyno),0)
  ifyes = -1*shares*buyno
  ifno = 0.9*(1-buyno)*shares
  risk = c()
  for(i in 1:length(ifyes))
  {
    risk = c(risk, sum(ifno) + ifyes[i] - ifno[i])
  }
  adj = min(risk)
  payoff = risk - adj
  
  shares = trunc(payoff/(buyno+0.9*(1-buyno)),0) + shares
  ifyes = -1*shares*buyno
  ifno = 0.9*(1-buyno)*shares
  risk = c()
  for(i in 1:length(ifyes))
  {
    risk = c(risk, sum(ifno) + ifyes[i] - ifno[i])
  }
  adj = min(risk)
  invpercontract = shares*buyno
  if(arbcalcamt(ticker)>0)
    adj = adj - (sharequant*(arbcalcamt(ticker)))
  if(adj > 0)
  {
    kellyvalue = kellycriterion(ticker)
    if(max(invpercontract) >= 850)
    {
      cat("Reduce Number of Shares")
    }
    print(paste("Hedged Arbitrage of $",round(adj,5), " on: ",ticker,sep = ""))
    toprint = data.frame(cbind(as.character(buynodata[,1]),shares-sharequant, buyno))
    colnames(toprint) = c("Contracts","Shares","Prices")
    print(toprint)
    print(paste("Size bet as the lowest of ", round(kellyvalue*100,2), "% of maximum position, maximum available depth, and maximum remaining position size",sep = ""))
    histframe = loadhistory()
    find = 0
    for(i in 1:length(histframe[,1]))
    {
      if(ticker == histframe[i,1])
      {
        find = i
        break
      }
    }
    if(find != 0)
    {
      plot(as.numeric(histframe[i,][,-1]), main = paste("Ticker: ",ticker, sep = ""), xlab = "Time Index (a.u.)", ylab = "Hedged Arbitrage Value ($)")
    }
    if(emailgate)
    {
      toemail = paste("   [", "Contracts", "\t", "Shares", "\t", "Prices","]   ",sep = "")
      toemail = paste(toemail, "\n")
      for(i in 1:length(buynodata[,1]))
      {
        toemail = paste(toemail, "   [", toprint[i,1], "\t", toprint[i,2], "\t", toprint[i,3],"]   ",sep = "")
        toemail = paste(toemail, "\n")
      }
      toemail = paste(toemail, "Size bet as the lowest of ", round(kellyvalue*100,2), "% of maximum position, maximum available depth, and maximum remaining position size", sep = "")
      msg = toemail
      sender = "predictitarbseeker@gmail.com"
      recipients = emails
      send.mail(from = sender, to = recipients, subject = paste("PredictIt Arbitrage Alert: $", round(adj,5), " on ", ticker, sep = ""), body = msg, smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "predictitarbseeker", passwd = "predictit", ssl = TRUE), authenticate = TRUE)
    }
  }
}

#checkclintonarb - determines whether there is an arbitrage opportunity on woman president vs. clinton on USPREZ16 markets
checkclintonarb = function(){
  woman = suppressWarnings(getdata("WOMAN.USPREZ16"))
  pres = suppressWarnings(getdata("USPREZ16"))
  if(woman == "NULL")
    return()
  if(pres == "NULL")
    return()
  index = 0
  for(i in 1:length(pres[,1]))
  {
    if(pres[i,1] == "Hillary Clinton")
      index = i
  }
  costpresno = as.numeric(pres[index,4])
  costwoman = as.numeric(woman[index,3])
  costwomanno = as.numeric(woman[index,4])
  costpres = as.numeric(pres[index,3])
  
  prof1 = (1-costpresno)*0.9 - costwoman
  prof2 = (1-costwoman)*0.9 - costpresno
  prof3 = (1-costpres)*0.9 - costwomanno
  prof4 = (1-costwomanno)*0.9 - costpres
  
  if(prof1 > 0 & prof2 > 0)
    cat("Arbitrage on Clinton: Buy Woman President and No Clinton President")
  if(prof3 > 0 & prof4 > 0)
    cat("Arbitrage on Clinton: Buy Clinton President and No Woman President")
}

#checkallarbs - determines in a single iteration whether there is arbitrage on the inputted set of tickers
checkallarbs = function(tickers){
  print(Sys.time())
  for(i in 1:length(tickers[,1])){
    suppressWarnings(checkhedgedarb(as.character(tickers[i,1]),0))
  } #hedged
  for(i in 1:length(tickers[,1])){
    suppressWarnings(arbcalc(as.character(tickers[i,1])))
  }
}

#updatehistory - updates a historical spreadsheet of arbitrage data to facilitate kelly criterion based position sizing
updatehistory = function(){
  filler = c()
  tickerdata = read.table("~/Personal/Trading/TickerHistory.csv",stringsAsFactors = FALSE)
  tickers = suppressWarnings(setvalid(read.table("~/Personal/Trading/PredictItActiveTickers.csv", quote="\"", comment.char="",stringsAsFactors = FALSE),read.table("~/Personal/Trading/maxedmarkets.csv", quote="\"", comment.char="",stringsAsFactors = FALSE)))
  additive = vector(mode = "numeric", length = length(tickerdata[1,]) - 1)
  #if tickerdata is not in tickers, omit
  toremove = c()
  for(i in 1:length(tickerdata[,1]))
  {
    contained = FALSE
    for(j in 1:length(tickers[,1]))
    {
      if(as.character(tickerdata[i,1]) == as.character(tickers[j,1]))
      {
        contained = TRUE
        break
      }
    }
    if(!contained)
      toremove = c(toremove, i)
  }
  toremove = reverse(toremove)
  if(length(toremove)>0)
    for(i in 1:length(toremove))
      tickerdata = data.frame(tickerdata[-toremove[i],])
  cnames = colnames(tickerdata)
  cnames = cnames[-1]
  data = data.frame(tickerdata[,-1])
  colnames(data) = cnames
  names = data.frame(tickerdata[,1], stringsAsFactors = FALSE)
  toadd = c()
  if(length(tickerdata[,1]) > 0)
  {
    for(i in 1:length(tickers[,1]))
    {
      included = FALSE
      for(j in 1:length(tickerdata[,1]))
      {
        if(as.character(tickerdata[j,1]) == as.character(tickers[i,1]))
        {
          included = TRUE
          break
        }
      }
      if(!included)
        toadd = c(toadd, as.character(tickers[i,1]))
    }
  }
  if(length(tickerdata[,1]) == 0)
    toadd = c(as.character(tickers[,1]))
  newnames = c()
  for(i in 1:length(names[,1]))
    newnames = c(newnames, as.character(names[i,1]))
  for(i in 1:length(toadd))
    newnames = c(newnames, toadd[i])
  toclean = newnames
  newnames = c()
  for(i in 1:length(toclean))
  {
    if(!is.na(toclean[i]))
      newnames = c(newnames, toclean[i])
  }
  #adjust data to include zeros as rows
  if(length(data[1,]) == 0)
    data = cbind(data,0)
  trackrecord = length(data[1,])
  if(length(toadd)>0)
    for(i in 1:length(toadd))
      data = rbind(data, vector(mode ="numeric", length = trackrecord))
  newdata = cbind(newnames, data)
  #add a new column
  for(i in 1:length(tickers[,1]))
  {
    filler = c(filler, suppressWarnings(hedgedarbvalue(as.character(tickers[i,1]))))
    if(length(filler) != i)
      break
  }
  newdata = cbind(newdata, filler)
  colnames(newdata)[length(colnames(newdata))] = as.character.Date(Sys.timeDate())
  write.table(newdata, file = "~/Personal/Trading/TickerHistory.csv")
}

#liveupdatearbs - provides a live stream of updates and an email for any appearing arbitrage opportunities; iterations take ~14.5 seconds to run
liveupdatearbs = function(tickers,emails){
  tickers = tickers
  while(TRUE)
  {
    print(Sys.time())
    for(i in 1:length(tickers[,1])){
      suppressWarnings(checkhedgedarb(as.character(tickers[i,1]),0, emails, TRUE))
    } #hedged
    for(i in 1:length(tickers[,1])){
      suppressWarnings(arbcalc(as.character(tickers[i,1])))
    } #purearb
    cat("------------------------------------------------------------")
    for(i in 1:3){cat("\n")}
    updatehistory()
  }
}

#localupdatearbs - provides a live stream of updates for any appearing arbitrage opportunities; iterations take ~14.5 seconds to run 
localupdatearbs = function(tickers,emails){
  while(TRUE)
  {
    print(Sys.time())
    for(i in 1:length(tickers[,1])){
      suppressWarnings(checkhedgedarb(as.character(tickers[i,1]),0, emails, FALSE))
    } #hedged
    for(i in 1:length(tickers[,1])){
      suppressWarnings(arbcalc(as.character(tickers[i,1])))
    } #purearb
    cat("------------------------------------------------------------")
    for(i in 1:3){cat("\n")}
    updatehistory()
  }
}

#addmaxed - adds a ticker (string) to the list of maxed out markets
addmaxed = function(ticker){
  maxedtickers = read.table("~/Personal/Trading/maxedmarkets.csv", quote="\"", comment.char="",stringsAsFactors = FALSE)
  maxedmarkets = data.frame(as.character(as.vector(maxedtickers[,1])), stringsAsFactors = FALSE)
  maxedmarkets = data.frame(rbind(maxedmarkets, ticker),stringsAsFactors = FALSE)
  write.table(maxedmarkets, "~/Personal/Trading/maxedmarkets.csv")
}

#addticker - adds a ticker (string) to the master list of tickers
addticker = function(ticker){
  tickerlist = read.table("~/Personal/Trading/PredictItActiveTickers.csv", quote="\"", comment.char="",stringsAsFactors = FALSE)
  tickers = data.frame(as.character(as.vector(tickerlist[,1])), stringsAsFactors = FALSE)
  tickers = data.frame(rbind(tickers, ticker),stringsAsFactors = FALSE)
  write.table(tickers, "~/Personal/Trading/PredictItActiveTickers.csv")
}
