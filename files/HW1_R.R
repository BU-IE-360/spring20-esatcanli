require(readxl)
require(data.table)
require(zoo)
require(xts)

Interest_Rates<-read_excel('Personal_Bank_Loan_Interest_Rates.xlsx',n_max=538)
Interest_Rates<-data.table(Interest_Rates)
names<-c("Date","Rate")
setnames(Interest_Rates,names(Interest_Rates),names)
Interest_Rates$Date<-as.Date(Interest_Rates$Date,format="%d-%m-%Y")
Interest_Rates$Rate<-as.numeric(Interest_Rates$Rate)
str(Interest_Rates)
head(Interest_Rates)
plot(Interest_Rates,main="Change of Personal Bank Loan Interest Rate Turkey  over time", xlab="Years" ,ylab="Amount of TL",pch=19,col="4",cex=0.3,frame=FALSE)
lines(lowess(Interest_Rates),col=2)

Money_Supply<-read_excel('Money_Supply_M1.xlsx',n_max=538)
Money_Supply<-data.table(Money_Supply)
str(Money_Supply)
names<-c("Date","Amount")
setnames(Money_Supply,names(Money_Supply),names)
Money_Supply$Date<-as.Date(Money_Supply$Date,format="%d-%m-%Y")
Money_Supply$Amount<-as.numeric(Money_Supply$Amount)
str(Money_Supply)
head(Money_Supply)
plot(Money_Supply,main=" Money Supply in the Turkish Economy over time",xlab="Years",ylab="Amount of TL",pch=19,col="4",cex=0.3,frame=FALSE)
lines(lowess(Money_Supply),col=2)

Exchange_Rate_USD<-read_excel('Exchange_Rate_USD.xlsx',n_max=538)
Exchange_Rate_USD<-data.table(Exchange_Rate_USD)
str(Exchange_Rate_USD)
names<-c("Date","Rate")
setnames(Exchange_Rate_USD,names(Exchange_Rate_USD),names)
Exchange_Rate_USD$Date<-as.Date(Exchange_Rate_USD$Date,format="%d-%m-%Y")
Exchange_Rate_USD$Rate<-as.numeric(Exchange_Rate_USD$Rate)
str(Exchange_Rate_USD)
head(Exchange_Rate_USD)
plot(Exchange_Rate_USD,main="Exchange Parity USD/TL over time",xlab="Years",ylab="USD/TL",pch=19,col="4",cex=0.3,frame=FALSE)
lines(lowess(Exchange_Rate_USD),col=2)


plot(Interest_Rates$Rate,Exchange_Rate_USD$Rate,main="Relationship between Interest Rate and Exchange Rate",xlab = "Interest Rate",ylab="Exchange Rate",pch=19,col="blue",cex=0.8,frame=FALSE)
plot(Money_Supply$Amount,Exchange_Rate_USD$Rate,main="Relationship between Money Supply and Exchange Rate",xlab = "Money Supply",ylab="Exchange Rate",pch=19,col="blue",cex=0.8,frame=FALSE)


Interest_Rates_xts<-xts(Interest_Rates$Rate,order.by = Interest_Rates$Date)
Interest_Rates_xts<-na.locf(Interest_Rates_xts) 

Exchange_Rate_USD_xts<-xts(Exchange_Rate_USD$Rate,order.by = Exchange_Rate_USD$Date)
Exchange_Rate_USD_xts<-na.locf(Exchange_Rate_USD_xts) 

Money_Supply_xts<-xts(Money_Supply$Amount,order.by = Money_Supply$Date)
Money_Supply_xts<-na.locf(Money_Supply_xts) 

Interest_Rate_OHLC<-to.monthly(Interest_Rates_xts)
Exchange_Rate_OHLC<-to.monthly(Exchange_Rate_USD_xts)

plot(Exchange_Rate_OHLC,legend.loc = "topleft",frame=FALSE)
plot.xts(Interest_Rate_OHLC,legend.loc = "topleft",frame=FALSE)


Interest_Rate_OHLC_yearly<-to.yearly(Interest_Rates_xts)
Exchange_Rate_OHLC_yearly<-to.yearly(Exchange_Rate_USD_xts)


plot.xts(Exchange_Rate_OHLC_yearly,legend.loc = "topleft",frame=FALSE)
plot.xts(Interest_Rate_OHLC_yearly,legend.loc = "topleft",frame=FALSE)


Interest_Rates$InterestDiff<-diff(Interest_Rates_xts,lag=1)
Exchange_Rate_USD$ExchangeDiff<-diff(Exchange_Rate_USD_xts,lag=1)
Money_Supply$AmountDiff<-diff(Money_Supply_xts)


Interest_Rates$InterestChange<-100*(Interest_Rates_xts/stats::lag(Interest_Rates_xts,-1) - 1)
Exchange_Rate_USD$ExchangeChange<-100*(Exchange_Rate_USD_xts/stats::lag(Exchange_Rate_USD_xts,-1) - 1)
Money_Supply$AmountChange<-100*(Money_Supply_xts/stats::lag(Money_Supply_xts,-1) - 1)

AllData<-merge(Interest_Rates,Exchange_Rate_USD,by="Date")
AllData<-merge(AllData,Money_Supply,by="Date")
setnames(AllData,"Rate.x","Interest Rate")
setnames(AllData,"Rate.y","Exchange Rate")


plot(AllData[,-c("Date")],main="Relationship Between All Variables", pch=19,col="blue",cex=0.2)


Covariances<-as.data.frame(cor(AllData[,-c("Date")],use="complete.obs",method="pearson"))
Covariances

Interest_Rate_Weekly_Change<-xts(100*(Interest_Rates_xts/stats::lag(Interest_Rates_xts,-1) - 1))
Money_Supply_Weekly_Change<-xts(100*(Money_Supply_xts/stats::lag(Money_Supply_xts,-1) - 1))
Exchange_Rate_USD_Weekly_Change<-xts(100*(Exchange_Rate_USD_xts/stats::lag(Exchange_Rate_USD_xts,-1) - 1))
Interest_versus_Exchange_Change_Weekly<-merge(Interest_Rate_Weekly_Change,Exchange_Rate_USD_Weekly_Change,join="inner")
Money_versus_Exchange_Change_Weekly<-merge(Money_Supply_Weekly_Change,Exchange_Rate_USD_Weekly_Change,join="inner")


plot.xts(Interest_versus_Exchange_Change_Weekly,main="Weekly Interest Rate Changes and Exchange Rate Changes")
addLegend(legend.loc = "topleft",
          legend.names =c("Percent Change in Interest Rate","Percent Change in Exchange Rate"),
          col=c("red","black"),
          lty=c(1,1), 
          lwd=c(1,1),
          cex=0.65,
          bty="o")


plot.xts(Money_versus_Exchange_Change_Weekly,main="Weekly Money Amount Changes and Exchange Rate Changes")
addLegend(legend.loc = "topleft",
          legend.names =c("Percent Change in Money Supply","Percent Change in Exchange Rate"),
          col=c("red","black"),
          lty=c(1,1), 
          lwd=c(1,1),
          cex=0.65,
          bty="o")



#Seperate IR to months
ir_monthly<-endpoints(Interest_Rates_xts,on="months")
Interest_Rate_Monthly<-period.apply(Interest_Rates_xts,INDEX=ir_monthly,FUN=mean)
#Percent Change in Mean 
Interest_Rate_Monthly_Change<-xts(100*(Interest_Rate_Monthly/stats::lag(Interest_Rate_Monthly,-1) - 1))


#Seperate ER to months
er_monthly<-endpoints(Exchange_Rate_USD_xts,on="months")
Exchange_Rate_USD_monthly<-period.apply(Exchange_Rate_USD_xts,INDEX=er_monthly,FUN=mean)
#Percent Change in Mean 
Exchange_Rate_USD_Monthly_Change<-xts(100*(Exchange_Rate_USD_monthly/stats::lag(Exchange_Rate_USD_monthly,-1) - 1))


#Seperate MS to months
ms_monthly<-endpoints(Money_Supply_xts,on="months")
Money_Supply_Monthly<-period.apply(Money_Supply_xts,INDEX=ms_monthly,FUN=mean)
#Percent Change in Mean 
Money_Supply_Monthly_Change<-xts(100*(Money_Supply_Monthly/stats::lag(Money_Supply_Monthly,-1) - 1))

#Merge
Interest_versus_Exchange_Change_Monthly<-merge(Interest_Rate_Monthly_Change,Exchange_Rate_USD_Monthly_Change)
Interest_Versus_Money_Change_Monthly<-merge(Interest_Rate_Monthly_Change,Money_Supply_Monthly_Change)
Money_versus_Exchange_Change_Monthly<-merge(Money_Supply_Monthly_Change,Exchange_Rate_USD_Monthly_Change)

plot.xts(Interest_versus_Exchange_Change_Monthly,main="Montly Mean Change in Interest Rate versus Exchange Rate")
addLegend(legend.loc = "topleft",
          legend.names =c("Percent Change in Interest Rate","Percent Change in Exchange Rate"),
          col=c("red","black"),
          lty=c(1,1), 
          lwd=c(1,1),
          cex=0.65,
          bty="o")


plot.xts(Money_versus_Exchange_Change_Monthly,main="Monthly Mean Change in Money Supply versus Exchange Rate")
addLegend(legend.loc = "topleft",
          legend.names =c("Mean Percent Change in Money Supply","Mean Percent Change in Exchange Rate"),
          col=c("red","black"),
          lty=c(1,1), 
          lwd=c(1,1),
          cex=0.65,
          bty="o")



Exchange_Rate_USD_Weekly_Diff_Change<-xts(diff(100*(Exchange_Rate_USD_xts/stats::lag(Exchange_Rate_USD_xts,-1) - 1)))
Interest_Rate_Weekly_Diff_Change<-xts(diff(100*(Interest_Rates_xts/stats::lag(Interest_Rates_xts,-1) - 1)))
Money_Supply_Weekly_Diff_Change<-xts(diff(100*(Money_Supply_xts/stats::lag(Money_Supply_xts,-1) - 1)))


Interest_Rate_versus_Exchange_Weekly_Diff_Change<-merge(Exchange_Rate_USD_Weekly_Diff_Change,Interest_Rate_Weekly_Diff_Change,join="inner")
Money_versus_Exchange_Change_Weekly_Diff_Change<-merge(Exchange_Rate_USD_Weekly_Diff_Change,Money_Supply_Weekly_Diff_Change,join="inner")



plot(Interest_Rate_versus_Exchange_Weekly_Diff_Change,main="Weekly Percent Changes in Interest Rate and Exchange Rate")
addLegend(legend.loc = "topleft",
          legend.names =c("Weekly Percent Change in Differences of Exchange Rates","Weekly Percent Change in Differences of Interest Rates"),
          col=c("red","black"),
          lty=c(1,1), 
          lwd=c(1,1), 
          cex=0.65,
          bty="o")

plot.xts(Money_versus_Exchange_Change_Weekly_Diff_Change)
addLegend(legend.loc = "topleft",
          legend.names =c("Weekly Percent Change in Differences of Exchange Rates","Weekly Percent Changes in Money Supply Amounts"),
          col=c("red","black"),
          lty=c(1,1), 
          lwd=c(1,1),
          cex=0.65,
          bty="o")