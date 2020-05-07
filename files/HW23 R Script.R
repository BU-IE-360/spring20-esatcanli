



require(readxl)
require(data.table)
require(zoo)
require(xts)
require(forecast)
require(stats)
require(lubridate)
require(dplyr)
require(ggplot2)
require(scales)





Electric_Usage_Data<-read.csv('GercekZamanliTuketim-01012016-26042020.csv',header = T)
head(Electric_Usage_Data)
Electric_Usage_Data$Date<-paste(Electric_Usage_Data$Tarih,Electric_Usage_Data$Saat)



names<-c("Date","Hour","Usage(MwH)","Date&Hour")
setnames(x = Electric_Usage_Data,old = names(Electric_Usage_Data),new = names)
str(Electric_Usage_Data)




Electric_Usage_Data$`Usage(MwH)`<-as.numeric(Electric_Usage_Data$`Usage(MwH)`)
str(Electric_Usage_Data)







Electric_Usage_XTS_Hourly<-xts(Electric_Usage_Data$`Usage(MwH)`,
                               order.by=as.Date(Electric_Usage_Data$`Date&Hour`,"%d.%m.%Y %H:%M:%S"))




nmonths(Electric_Usage_XTS_Hourly)
ndays(Electric_Usage_XTS_Hourly)




Electric_Usage_TS_Hourly<-ts(Electric_Usage_XTS_Hourly,start=c(2016,1),freq=24)




ts.plot(Electric_Usage_TS_Hourly,main="Hourly Electric Usage in MwH", ylab="Mwh", xlab="")
acf(Electric_Usage_TS_Hourly,main="Auto Correlation for the Hourly Electric Usage")





Electric_Usage_TS_Hourly_Multiplicative<-decompose(Electric_Usage_TS_Hourly,type="multiplicative")
Electric_Usage_TS_Hourly_Additive<-decompose(Electric_Usage_TS_Hourly,type="additive")





plot(Electric_Usage_TS_Hourly_Multiplicative)
plot(Electric_Usage_TS_Hourly_Additive)





plot(Electric_Usage_TS_Hourly_Multiplicative$random,
     frame=FALSE,
     col="black",
     lwd=0.1,
     main="Hourly Multiplicative Method Random Noise",
     ylab= "Random Noise",
     xlab= "Time")
plot(Electric_Usage_TS_Hourly_Additive$random,
     frame=FALSE,
     col="black",
     lwd=0.1,
     main="Hourly Additive Method Random Noise",
     ylab= "Random Noise",
     xlab= "Time")










eu_daily<-endpoints(Electric_Usage_XTS_Hourly,on="days")
Electric_Usage_XTS_Daily<-period.apply(Electric_Usage_XTS_Hourly,INDEX=eu_daily,FUN=mean)
periodicity(Electric_Usage_XTS_Daily)




Electric_Usage_TS_Daily<-ts(Electric_Usage_XTS_Daily,start=c(2016,1),frequency = 7)
length(Electric_Usage_TS_Daily)




ts.plot(Electric_Usage_TS_Daily, main="Daily Electric Usage in MwH")
acf(Electric_Usage_TS_Daily,main="Auto Correlation for the Daily Electric Usage")


Electric_Usage_TS_Daily_Multiplicative<-decompose(Electric_Usage_TS_Daily,type="multiplicative")
Electric_Usage_TS_Daily_Additive<-decompose(Electric_Usage_TS_Daily,type="additive")



plot(Electric_Usage_TS_Daily_Multiplicative)
plot(Electric_Usage_TS_Daily_Additive)

plot(Electric_Usage_TS_Daily_Multiplicative$random,
     frame=FALSE,
     col="black",
     lwd=0.1,
     main="Multiplicative Method Random Noise",
     ylab= "Random Noise",
     xlab= "Time")
# Let's get a closer look into randomness
plot(Electric_Usage_TS_Daily_Additive$random,
     frame=FALSE,
     col="black",
     lwd=0.1,
     main="Additive Method Random Noise",
     ylab= "Random Noise",
     xlab= "Time")




eu_weekly<-endpoints(Electric_Usage_XTS_Hourly,on="weeks")
Electric_Usage_XTS_Weekly<-period.apply(Electric_Usage_XTS_Hourly,INDEX=eu_weekly,FUN=mean)
periodicity(Electric_Usage_XTS_Weekly)




Electric_Usage_TS_Weekly<-ts(Electric_Usage_XTS_Weekly,start=c(2016,1),frequency = 4)
length(Electric_Usage_TS_Weekly)





ts.plot(Electric_Usage_TS_Weekly,main="Weekly Electric Usage in MwH")
acf(Electric_Usage_TS_Weekly,main="Auto Correlation for the Weekly Electric Usage")


Electric_Usage_TS_Weekly_Multiplicative<-decompose(Electric_Usage_TS_Weekly,type="multiplicative")
Electric_Usage_TS_Weekly_Additive<-decompose(Electric_Usage_TS_Weekly,type="additive")





plot(Electric_Usage_TS_Weekly_Multiplicative)
plot(Electric_Usage_TS_Weekly_Additive)


plot(Electric_Usage_TS_Weekly_Multiplicative$random,
     frame=FALSE,
     col="black",
     lwd=0.1,
     main="Multiplicative Method Random Noise",
     ylab= "Random Noise",
     xlab= "Time")
plot(Electric_Usage_TS_Weekly_Additive$random,
     frame=FALSE,
     col="black",
     lwd=0.1,
     main="Additive Method Random Noise",
     ylab= "Random Noise",
     xlab= "Time")





eu_monthly<-endpoints(Electric_Usage_XTS_Hourly,on="months")
Electric_Usage_XTS_Monthly<-period.apply(Electric_Usage_XTS_Hourly,INDEX=eu_monthly,FUN=mean)
periodicity(Electric_Usage_XTS_Monthly)


Electric_Usage_TS_Monthly<-ts(Electric_Usage_XTS_Monthly,start=c(2016,1),frequency = 12)
length(Electric_Usage_TS_Monthly)



ts.plot(Electric_Usage_TS_Monthly,main="Monthly Electric Usage in MwH")
acf(Electric_Usage_TS_Monthly,main="Auto Correlation for the Monthly Electric Usage")





Electric_Usage_TS_Monthly_Multiplicative<-decompose(Electric_Usage_TS_Monthly,type="multiplicative")
Electric_Usage_TS_Monthly_Additive<-decompose(Electric_Usage_TS_Monthly,type="additive")




plot(Electric_Usage_TS_Monthly_Multiplicative)
plot(Electric_Usage_TS_Monthly_Additive)




plot(Electric_Usage_TS_Monthly_Multiplicative$random,
     frame=FALSE,
     col="black",
     lwd=0.1,
     main="Multiplicative Method Random Noise",
     ylab= "Random Noise",
     xlab= "Time")
plot(Electric_Usage_TS_Monthly_Additive$random,
     frame=FALSE,
     col="black",
     lwd=0.1,
     main="Additive Method Random Noise",
     ylab= "Random Noise",
     xlab= "Time")







Electric_Usage_TS_168<-ts(Electric_Usage_XTS_Hourly,start=c(2016,1),frequency =168)
length(Electric_Usage_TS_168)



ts.plot(Electric_Usage_TS_168,main="168-hourElectric Usage in MwH" )
acf(Electric_Usage_TS_168,main="Auto Correlation for the 168-hour Electric Usage")



Electric_Usage_TS_168_Additive<-decompose(Electric_Usage_TS_168,type="additive")
plot(Electric_Usage_TS_168_Additive)

Electric_Usage_TS_168_Additive_Deseasonalized<-Electric_Usage_TS_168-Electric_Usage_TS_168_Additive$seasonal





plot(Electric_Usage_TS_168_Additive_Deseasonalized)
acf(Electric_Usage_TS_168_Additive_Deseasonalized)


Electric_Usage_TS_168_Additive_Deseasonalized_Detrended<-Electric_Usage_TS_168_Additive_Deseasonalized-Electric_Usage_TS_168_Additive$trend




plot(Electric_Usage_TS_168_Additive_Deseasonalized_Detrended)
acf(Electric_Usage_TS_168_Additive_Deseasonalized_Detrended,na.action = na.omit)



Model_1<-arima(Electric_Usage_TS_168_Additive_Deseasonalized_Detrended, order=c(1,0,0))
AIC(Model_1)
BIC(Model_1)

Model_2<-arima(Electric_Usage_TS_168_Additive_Deseasonalized_Detrended, order=c(0,0,1))
AIC(Model_2)
BIC(Model_2)



Final_Model<-arima(Electric_Usage_TS_168_Additive_Deseasonalized_Detrended, order=c(1,0,0))
Fitted_Model <- Electric_Usage_TS_168 - residuals(Final_Model)




ts.plot(Electric_Usage_TS_168, xlab = "Year", ylab = "Electricity Consumption",main="Consumption 2016-2020",xlim=c(2016,2020))
points(Fitted_Model, type = "l", col = 2, lty = 2)





Final_Model_Forecast <- predict(Final_Model, n.ahead = 24)$pred
Final_Model_Forecast_SE<-predict(Final_Model, n.ahead = 24)$se
print(as.data.frame(Final_Model_Forecast)) 






Trend_Values<-as.numeric(tail(na.omit(Electric_Usage_TS_168_Additive$trend),1)) #2614387
Trend_Values # Just one value actually



Forecast_Values<-as.numeric(Final_Model_Forecast)


Taking the Figure Values from the first 24 values in the time series object since we are forecasting the next 24 values after the end of 168 hours. 


Figure_Values<-head(Electric_Usage_TS_168_Additive$figure,24)


Adding all the values together and coming up with the actual or realized forecasts


Realized_Forecast<-2614387+Forecast_Values+Figure_Values




SE_Lower_Bound<-Final_Model_Forecast-1.96*Final_Model_Forecast_SE

SE_Lower_Bound<-SE_Lower_Bound+2614387+Figure_Values


SE_Upper_Bound<-Final_Model_Forecast+1.96*Final_Model_Forecast_SE ## Upper Bound estimate on the forecast
SE_Upper_Bound<-SE_Upper_Bound+2614387+Figure_Values ## Actual Realized Upper Bound estimate on the forecast






mean(tail(Electric_Usage_Data$`Usage(MwH)`,24)) # Will be used for plotting






plot(Realized_Forecast, cex=1.7, pch=19,frame=FALSE,main="24 Hour Forecast Values in MwH", ylab="Energy Use", xlab="Time of Day", col="blue")
abline(h=2331767,col="red")
points(SE_Lower_Bound,type = "l", col = 2, lty = 2)





Forecast_Data<-cbind(SE_Lower_Bound,Realized_Forecast,SE_Upper_Bound)

Forecast_Data<-as.data.frame(Forecast_Data)
head(Forecast_Data)




# Setting the dates for the forecasted period
dates<-seq(as.POSIXct("2020-04-27 00:00:00"), as.POSIXct("2020-04-27 23:00:00"), by="hour")

#Converting to XTS for better manipulation 
Forecast_Data_XTS<-xts(Forecast_Data,order.by = dates)
head(Forecast_Data_XTS)





plot(Forecast_Data_XTS,main="Bound Comparison on the Forecasted Values")



addLegend(legend.loc = "topleft",
          legend.names =c("Upper Bound","Realized Forecast","Lower Bound"),
          col=c("green","red","black"),
          lty=c(1,1), 
          lwd=c(1,1),
          cex=1,


   
   

Electric_Usage_Forecast_1_Train %>% 
        tail(24*7*4) %>% 
        decompose() %>% 
        autoplot()






Electric_Usage_Forecast_1_msts<-Electric_Usage_XTS_Hourly %>% msts( seasonal.periods = c(24, 24*7))
Electric_Usage_Forecast_1_msts %>% head(  24 *7 *4 ) %>% mstl() %>% autoplot() 




Electric_Usage_Forecast_1_Train_msts_train <- head(Electric_Usage_Forecast_1_msts, length(Electric_Usage_Forecast_1_msts) - 24*7*12)
Electric_Usage_Forecast_1_Train_msts_test <- tail(Electric_Usage_Forecast_1_msts,  24*7*12)








Electric_Usage_Forecast_1_Train_msts_train <- tail(Electric_Usage_Forecast_1_Train_msts_train, 24*7*4)
autoplot(Electric_Usage_Forecast_1_Train_msts_train)


Forecast_1_Model <- Electric_Usage_Forecast_1_Train_msts_train %>%
        stlm(lambda = 0) %>% 
        forecast(h = 24) 
plot(Forecast_1_Model, main="Forecasting with STML model using daily and weekly seasonality")



Forecast_1_Model_Data_Frame<-as.data.frame(Forecast_1_Model)

dates<-seq(as.POSIXct("2020-04-27 00:00:00"), as.POSIXct("2020-04-27 23:00:00"), by="hour")

Forecast_Data_XTS_stlm<-xts(Forecast_1_Model_Data_Frame,order.by = dates)

print(as.data.frame(Forecast_Data_XTS_stlm))





plot(Forecast_Data_XTS_stlm,main="Bound Comparison on the Forecasted Values using STLM ")



addLegend(legend.loc = "topleft",
          legend.names =c("Point Forecast","Lower 80% ","Higher 80%","Lower 95%", "Higher 95%"),
          lty=c(1,1), 
          lwd=c(1,1),
          cex=1,
          bty="o")

                
                                                                                                                                                                   
                                                                                                                                                                   