# install the required packages first
require(jsonlite)
require(httr)
require(data.table)

get_token <- function(username, password, url_site){
  
  post_body = list(username=username,password=password)
  post_url_string = paste0(url_site,'/token/')
  result = POST(post_url_string, body = post_body)
  
  # error handling (wrong credentials)
  if(result$status_code==400){
    print('Check your credentials')
    return(0)
  }
  else if (result$status_code==201){
    output = content(result)
    token = output$key
  }
  
  return(token)
}

get_data <- function(start_date='2020-03-20', token, url_site){
  
  post_body = list(start_date=start_date,username=username,password=password)
  post_url_string = paste0(url_site,'/dataset/')
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  result = GET(post_url_string, header, body = post_body)
  output = content(result)
  data = data.table::rbindlist(output)
  data[,event_date:=as.Date(event_date)]
  data = data[order(product_content_id,event_date)]
  return(data)
}


send_submission <- function(predictions, token, url_site, submit_now=F){
  
  format_check=check_format(predictions)
  if(!format_check){
    return(FALSE)
  }
  
  post_string="list("
  for(i in 1:nrow(predictions)){
    post_string=sprintf("%s'%s'=%s",post_string,predictions$product_content_id[i],predictions$forecast[i])
    if(i<nrow(predictions)){
      post_string=sprintf("%s,",post_string)
    } else {
      post_string=sprintf("%s)",post_string)
    }
  }
  
  submission = eval(parse(text=post_string))
  json_body = jsonlite::toJSON(submission, auto_unbox = TRUE)
  submission=list(submission=json_body)
  
  print(submission)
  # {"31515569":2.4,"32939029":2.4,"4066298":2.4,"6676673":2.4,"7061886":2.4,"85004":2.4} 
  
  if(!submit_now){
    print("You did not submit.")
    return(FALSE)      
  }
  
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  post_url_string = paste0(url_site,'/submission/')
  result = POST(post_url_string, header, body=submission)
  
  if (result$status_code==201){
    print("Successfully submitted. Below you can see the details of your submission")
  } else {
    print("Could not submit. Please check the error message below, contact the assistant if needed.")
  }
  
  print(content(result))
  
}

check_format <- function(predictions){
  
  if(is.data.frame(predictions) | is.data.frame(predictions)){
    if(all(c('product_content_id','forecast') %in% names(predictions))){
      if(is.numeric(predictions$forecast)){
        print("Format OK")
        return(TRUE)
      } else {
        print("forecast information is not numeric")
        return(FALSE)                
      }
    } else {
      print("Wrong column names. Please provide 'product_content_id' and 'forecast' columns")
      return(FALSE)
    }
    
  } else {
    print("Wrong format. Please provide data.frame or data.table object")
    return(FALSE)
  }
  
}

# this part is main code
subm_url = 'http://167.172.183.67'

u_name = "Group13"
p_word = "O73H4cqzoZUntzYW"
submit_now = FALSE

username = u_name
password = p_word

token = get_token(username=u_name, password=p_word, url=subm_url)
data = get_data(token=token,url=subm_url)

predictions=unique(data[,list(product_content_id)])
predictions[,forecast:=2.3]

send_submission(predictions, token, url=subm_url, submit_now=F)



####

require(ggplot2)
library(caTools)
library(xts)
library(zoo)
library(forecast)

data    
summary(data)

#Mayis 31 (t-1) >> length 397/ 1 Haziran(t)  /   2 Haziran (t+1) 

n<-408

dates<- seq(as.Date("2019-04-30"), length = n, by = "days")
dates


validation_dates<-seq(as.Date("2020-03-01"), length = n-306, by = "days")
validation_dates







########################3904356 Giyim Mont Koton##############################################################################

mont<-subset(data, data$product_content_id==3904356)
summary(mont)

mont_xts<-xts(mont,order.by=dates)


mont_train_xts<-mont_xts[index(mont_xts)<"2020-03-01"]
mont_valid_xts<-mont_xts[index(mont_xts)>="2020-03-01"]

#To creata data table of extra regressor for model
mont_xreg1<-cbind(mont[1:306,"favored_count"],
                  mont[1:306,"basket_count"])

mont_xreg2<-cbind(mont[307:n,"favored_count"],
                  mont[307:n,"basket_count"])


arima_mont<-Arima(as.numeric(mont_train_xts$sold_count),xreg=as.matrix(mont_xreg1),order=c(0,0,1))

forecast_arima_mont<-forecast(arima_mont,xreg=as.matrix(mont_xreg2))

forecast_arima_mont_xts<-xts(forecast_arima_mont$mean,order.by=validation_dates)


plot(dates,mont$sold_count)
lines(validation_dates,forecast_arima_mont_xts)
###########################

mont_lm_train<-mont[1:306,]
mont_lm_valid<-mont[307:n,]


mont_lm_model<-lm(sold_count~ favored_count+basket_count,data=mont_lm_train)

predict_mont_lm<-predict(mont_lm_model,mont_lm_valid)

predict_mont_lm

predict_mont_lm_xts<- xts(predict_mont_lm,order.by=validation_dates)


plot(dates,mont$sold_count)
lines(validation_dates,predict_mont_lm_xts)
#####Submission Future Xreg
mont_favored_xreg<-as.numeric(mont_xts$favored_count)
mont_favored_model<-auto.arima(mont_favored_xreg)
mont_favored_forecast<-forecast(mont_favored_model,h=2)

mont_basket_xreg<-as.numeric(mont_xts$basket_count)
mont_basket_model<-auto.arima(mont_basket_xreg)
mont_basket_forecast<-forecast(mont_basket_model,h=2)

mont_submission_xreg1<-cbind(mont[1:n,"favored_count"],
                             mont[1:n,"basket_count"])
mont_submission_xreg2<-data.table("favored_count"=mont_favored_forecast$mean,
                                  "basket_count"=mont_basket_forecast$mean)

########ARIMA submission forecast 
mont_submission_xts<-as.numeric(mont_xts$sold_count)

mont_submission_arima_model<-Arima(mont_submission_xts,
                                   xreg=as.matrix(mont_submission_xreg1),
                                   order=c(0,0,1))

mont_submission_forecast_arima<-forecast(mont_submission_arima_model,
                                         xreg=as.matrix(mont_submission_xreg2))
########Reg Submission

mont_lm_submission<-lm(sold_count~favored_count+basket_count,data=mont)

pred_mont_lm_submodel<-predict(mont_lm_submission,mont_submission_xreg2)


mont_submission_arima<-0.5*(mont_submission_forecast_arima$mean[2]+pred_mont_lm_submodel[2])
mont_submission_arima

################################################## MONT ENDS##################################################################



#########################32939029 Oral B dis fircasi###########################################################################


toothbrush<-subset(data, data$product_content_id==32939029)

toothbrush

plot(dates,toothbrush$sold_count)


toothbrush_xts<-xts(toothbrush,order.by=dates)


toothbrush_train_xts<-toothbrush_xts[index(toothbrush_xts)<"2020-03-01" & index(toothbrush_xts)>"2019-11-22"]

toothbrush_valid_xts<-toothbrush_xts[index(toothbrush_xts)>="2020-03-01"]

#To creata data table of extra regressor for model
toothbrush_xreg1<-cbind( toothbrush[208:306,"visit_count"],
                         toothbrush[208:306,"favored_count"],
                         toothbrush[208:306,"basket_count"],
                         toothbrush[208:306,"price"])

toothbrush_xreg2<-cbind( toothbrush[307:n,"visit_count"],
                         toothbrush[307:n,"favored_count"],
                         toothbrush[307:n,"basket_count"],
                         toothbrush[307:n,"price"])



auto.arima(as.numeric(toothbrush_train_xts$sold_count))

toothbrush_arima_model<-Arima(as.numeric(toothbrush_train_xts$sold_count),
                              xreg=as.matrix(toothbrush_xreg1),
                              order=c(1,0,1))


AIC(toothbrush_arima_model)


toothbrush_forecast_arima<-forecast(toothbrush_arima_model,xreg=as.matrix(toothbrush_xreg2))

toothbrush_forecast_arima_xts<-xts(toothbrush_forecast_arima$mean,order.by=validation_dates)



plot(dates,toothbrush$sold_count)

lines(validation_dates,toothbrush_forecast_arima_xts,col="blue")


toothbrush_ARIMA_MAPE<-100*mean(abs((toothbrush_forecast_arima_xts-as.numeric(toothbrush_valid_xts$sold_count))/as.numeric(toothbrush_valid_xts$sold_count)))
toothbrush_ARIMA_MAPE

######################
toothbrush_lm_train<-toothbrush[208:306,]
toothbrush_lm_valid<-toothbrush[307:n,]


toothbrush_lm_model<-lm(sold_count~visit_count+favored_count+basket_count+price,data=toothbrush_lm_train)

pred_toothbrush_lm_model<-predict(toothbrush_lm_model,toothbrush_lm_valid)


pred_toothbrush_lm_model_xts<-xts(pred_toothbrush_lm_model,order.by = validation_dates)

plot(pred_toothbrush_lm_model_xts)

plot(dates,toothbrush$sold_count)
lines(validation_dates,pred_toothbrush_lm_model_xts,col="blue")

toothbrush_reg_MAPE<-100*mean(abs((pred_toothbrush_lm_model_xts-as.numeric(toothbrush_valid_xts$sold_count))/as.numeric(toothbrush_valid_xts$sold_count)))
toothbrush_reg_MAPE
###Tootbrush Sub Xreg
toothbrush_visit_xreg<-as.numeric(toothbrush_xts$visit_count)
toothbrush_visit_model<-auto.arima(toothbrush_visit_xreg)
toothbrush_visit_forecast<-forecast(toothbrush_visit_model,h=2)

toothbrush_favored_xreg<-as.numeric(toothbrush_xts$favored_count)
toothbrush_favored_model<-auto.arima(toothbrush_favored_xreg)
toothbrush_favored_forecast<-forecast(toothbrush_favored_model,h=2)

toothbrush_basket_xreg<-as.numeric(toothbrush_xts$basket_count)
toothbrush_basket_model<-auto.arima(toothbrush_basket_xreg)
toothbrush_basket_forecast<-forecast(toothbrush_basket_model,h=2)

toothbrush_price_xreg<-as.numeric(toothbrush_xts$price)
toothbrush_price_model<-auto.arima(toothbrush_price_xreg)
toothbrush_price_forecast<-forecast(toothbrush_price_model,h=2)

##################################################################

toothbrush_submission_xreg1<-cbind(toothbrush[1:n,"visit_count"],
                                   toothbrush[1:n,"favored_count"],
                                   toothbrush[1:n,"basket_count"],
                                   toothbrush[1:n,"price"])
toothbrush_submission_xreg2<-data.table("visit_count"=toothbrush_visit_forecast$mean,
                                        "favored_count"=toothbrush_favored_forecast$mean,
                                        "basket_count"=toothbrush_basket_forecast$mean,
                                        "price"=toothbrush_price_forecast$mean)



######ARIMA submission forecast
toothbrush_submission_xts<-as.numeric(toothbrush_xts$sold_count)

toothbrush_submission_arima_model<-Arima(toothbrush_submission_xts,
                                         xreg=as.matrix(toothbrush_submission_xreg1),
                                         order=c(1,0,1))

toothbrush_submission_forecast_arima<-forecast(toothbrush_submission_arima_model,
                                               xreg=as.matrix(toothbrush_submission_xreg2))

#########Regression submission
toothbrush_lm_submission<-lm(sold_count~visit_count+favored_count+basket_count+price,data=toothbrush)

pred_toothbrush_lm_submodel<-predict(toothbrush_lm_submission,toothbrush_submission_xreg2)


toothbrush_submission_arima<-0.5*(toothbrush_submission_forecast_arima$mean[2]+pred_toothbrush_lm_submodel[2])

toothbrush_submission_arima




############################################ORALB ENDS########################################################################




##########################4066298	Islak mendil havlu ###############################################################

napkin<-subset(data, data$product_content_id==4066298)
napkin



napkin_xts<-xts(napkin,order.by=dates)


napkin_train_xts<-napkin_xts[index(napkin_xts)<"2020-03-01"]
napkin_valid_xts<-napkin_xts[index(napkin_xts)>="2020-03-01"]

#To creata data table of extra regressor for model
napkin_xreg1<-cbind(napkin[1:306,"visit_count"],
                    napkin[1:306,"basket_count"],
                    napkin[1:306,"price"])


napkin_xreg2<-cbind(napkin[307:n,"visit_count"],
                    napkin[307:n,"basket_count"],
                    napkin[307:n,"price"])



auto.arima(as.numeric(napkin_train_xts$sold_count))

napkin_arima_model<-Arima(as.numeric(napkin_train_xts$sold_count),
                          xreg=as.matrix(napkin_xreg1),
                          order=c(3,1,3))

AIC(napkin_arima_model)

forecast_napkin<-forecast(napkin_arima_model,xreg=as.matrix(napkin_xreg2))

forecast_napkin_xts<-xts(forecast_napkin$mean,order.by=validation_dates)

plot(dates,napkin$sold_count)
lines(validation_dates,forecast_napkin$mean,col="blue")

napkin_ARIMA_MAPE<-100*mean(abs((forecast_napkin_xts-as.numeric(napkin_valid_xts$sold_count))/as.numeric(napkin_valid_xts$sold_count)))
napkin_ARIMA_MAPE


###########Napkin regression    

napkin_lm_train<-napkin[1:306,]
napkin_lm_valid<-napkin[307:n,]


napkin_lm_model<-lm(sold_count~visit_count+basket_count+price,data=napkin_lm_train)

pred_napkin_lm_model<-predict(napkin_lm_model,napkin_lm_valid)

pred_napkin_lm_model_xts<-xts(pred_napkin_lm_model,order.by=validation_dates)

plot(pred_napkin_lm_model_xts)

plot(dates,napkin$sold_count)
lines(validation_dates,pred_napkin_lm_model_xts,col="blue")


napkin_reg_MAPE<-100*mean(abs((pred_napkin_lm_model_xts-as.numeric(napkin_valid_xts$sold_count))/as.numeric(napkin_valid_xts$sold_count)))
napkin_reg_MAPE

###Earbud ARIMA Submission Forecast
napkin_price_xreg<-as.numeric(napkin_xts$price)
napkin_price_model<-auto.arima(napkin_price_xreg)
napkin_price_forecast<-forecast(napkin_price_model,h=2)

napkin_visit_xreg<-as.numeric(napkin_xts$visit_count)
napkin_visit_model<-auto.arima(napkin_visit_xreg)
napkin_visit_forecast<-forecast(napkin_visit_model,h=2)

napkin_basket_xreg<-as.numeric(napkin_xts$basket_count)
napkin_basket_model<-auto.arima(napkin_basket_xreg)
napkin_basket_forecast<-forecast(napkin_basket_model,h=2)
##################################################################

napkin_submission_xreg1<-cbind(napkin[1:n,"price"],
                               napkin[1:n,"visit_count"],
                               napkin[1:n,"basket_count"])
napkin_submission_xreg2<-data.table("price"=napkin_price_forecast$mean,
                                    "visit_count"=napkin_visit_forecast$mean,
                                    "basket_count"=napkin_basket_forecast$mean)


###ARIMA Submission FOrecast
napkin_submission_xts<-as.numeric(napkin_xts$sold_count)

napkin_submission_arima_model<-Arima(napkin_submission_xts,
                                     xreg=as.matrix(napkin_submission_xreg1),
                                     order=c(3,0,3))

napkin_submission_forecast_arima<-forecast(napkin_submission_arima_model,
                                           xreg=as.matrix(napkin_submission_xreg2))

#########Regression submission
napkin_lm_submission<-lm(sold_count~price+visit_count+basket_count,data=napkin)

pred_napkin_lm_submodel<-predict(napkin_lm_submission,napkin_submission_xreg2)

##
napkin_submission_arima<-0.5*(napkin_submission_forecast_arima$mean[2]+pred_napkin_lm_submodel[2])
napkin_submission_arima

###############################################NAPKIN ENDS###########################################################















##############################################85004 Kozmetik Y¨uz Temizleyici La Roche Posay###############################################################################



cosmetic<-subset(data, data$product_content_id==85004)

cosmetic

plot(dates,cosmetic$sold_count)

cosmetic_xts<-xts(cosmetic,order.by=dates)

cosmetic_validation_dates<-seq(as.Date("2020-05-01"), length = 41, by = "days")
cosmetic_validation_dates


cosmetic_train_xts<-cosmetic_xts[index(cosmetic_xts)<"2020-05-01"  & index(cosmetic_xts)>="2020-03-01"]
cosmetic_valid_xts<-cosmetic_xts[index(cosmetic_xts)>="2020-05-01"]

#To creata data table of extra regressor for model
cosmetic_xreg1<-cbind(cosmetic[307:367,"visit_count"],
                      cosmetic[307:367,"basket_count"],
                      cosmetic[307:367,"price"])

cosmetic_xreg2<-cbind(cosmetic[368:n,"visit_count"],
                      cosmetic[368:n,"basket_count"],
                      cosmetic[368:n,"price"])

cosmetic_arima_model<-Arima(as.numeric(cosmetic_train_xts$sold_count),
                            xreg=as.matrix(cosmetic_xreg1),
                            order=c(0,1,3))

AIC(cosmetic_arima_model)


cosmetic_forecast_arima<-forecast(cosmetic_arima_model,xreg=as.matrix(cosmetic_xreg2))
cosmetic_forecast_arima

cosmetic_forecast_cosmetic_xts<-xts(cosmetic_forecast_arima$mean,order.by = cosmetic_validation_dates)

cosmetic_forecast_cosmetic_xts

plot(dates,cosmetic$sold_count)

lines(cosmetic_validation_dates,cosmetic_forecast_cosmetic_xts,col="blue")

cosmetic_ARIMA_MAPE<-100*mean(abs((cosmetic_forecast_cosmetic_xts-as.numeric(cosmetic_valid_xts$sold_count))/as.numeric(cosmetic_valid_xts$sold_count)))
cosmetic_ARIMA_MAPE

#################Cosmetic Regression

cosmetic_lm_train<-cosmetic[307:367,]
cosmetic_lm_valid<-cosmetic[368:n,]

cosmetic_lm_model<-lm(sold_count~visit_count+basket_count+favored_count+price,data=cosmetic_lm_train)

pred_cosmetic_lm_model<-predict(cosmetic_lm_model,cosmetic_lm_valid)

pred_cosmetic_lm_model_xts<-xts(pred_cosmetic_lm_model,order.by=cosmetic_validation_dates)

plot(pred_cosmetic_lm_model_xts)

plot(dates,cosmetic$sold_count)
lines(cosmetic_validation_dates,pred_cosmetic_lm_model_xts,col="blue")

cosmetic_reg_MAPE<-100*mean(abs((pred_cosmetic_lm_model_xts-as.numeric(cosmetic_valid_xts$sold_count))/as.numeric(cosmetic_valid_xts$sold_count)))
cosmetic_reg_MAPE


######
cosmetic_new<-cosmetic_xts[ index(cosmetic_xts)>="2020-03-01"]


###Cosmetic ARIMA Submission Forecast
cosmetic_price_xreg<-as.numeric(cosmetic_new$price)
cosmetic_price_model<-auto.arima(cosmetic_price_xreg)
cosmetic_price_forecast<-forecast(cosmetic_price_model,h=2)

cosmetic_visit_xreg<-as.numeric(cosmetic_new$visit_count)
cosmetic_visit_model<-auto.arima(cosmetic_visit_xreg)
cosmetic_visit_forecast<-forecast(cosmetic_visit_model,h=2)

cosmetic_basket_xreg<-as.numeric(cosmetic_new$basket_count)
cosmetic_basket_model<-auto.arima(cosmetic_basket_xreg)
cosmetic_basket_forecast<-forecast(cosmetic_basket_model,h=2)
##################################################################

cosmetic_submission_xreg1<-cbind(cosmetic[307:n,"visit_count"],
                                 cosmetic[307:n,"price"])
cosmetic_submission_xreg2<-data.table("visit_count"=cosmetic_visit_forecast$mean,
                                      "price"=cosmetic_price_forecast$mean)

###ARIMA Cosmetic Submission Forecast

cosmetic_submission_xts<-as.numeric(cosmetic_new$sold_count)

cosmetic_submission_arima_model<-Arima(cosmetic_submission_xts,
                                       xreg=as.matrix(cosmetic_submission_xreg1),
                                       order=c(0,1,3))

cosmetic_submission_forecast_arima<-forecast(cosmetic_submission_arima_model,
                                             xreg=as.matrix(cosmetic_submission_xreg2))

###########
cosmetic_new2<-cosmetic[307:n,]

cosmetic_lm_submission<-lm(sold_count~visit_count+price,data=cosmetic_new2)

pred_cosmetic_lm_submodel<-predict(cosmetic_lm_submission,cosmetic_submission_xreg2)


cosmetic_submission_arima<-0.5*(cosmetic_submission_forecast_arima$mean[2]+pred_cosmetic_lm_submodel[2])
cosmetic_submission_arima

##############################################END 85004 Kozmetik Y¨uz Temizleyici La Roche Posay###############################################################################

#################################################6676673 Elektronik Telefon Bluetooth Kulaklýk Xiaomi######################################################################

#### ARIMA Earbud



#Create earbud data
earbud<-subset(data, data$product_content_id==6676673)

earbud

plot(dates,earbud$sold_count)

#Corvert to xts for forecasting
earbud_xts<-xts(earbud,order.by=dates)

#Split data to traind and test data
earbud_train_xts<-earbud_xts[index(earbud_xts)>"2019-06-19" & index(earbud_xts)<"2020-03-01"]

earbud_valid_xts<-earbud_xts[index(earbud_xts)>="2020-03-01"]


#To creata data table of extra regressor for model
earbud_xreg1<-cbind(earbud[52:306,"favored_count"],
                    earbud[52:306,"price"])

earbud_xreg2<-cbind(earbud[307:n,"favored_count"],
                    earbud[307:n,"price"])



earbud_arima_model<-Arima(as.numeric(earbud_train_xts$sold_count),
                          xreg=as.matrix(earbud_xreg1), 
                          order=c(0,1,3))

AIC(earbud_arima_model)

earbud_forecast_arima<-forecast(earbud_arima_model,xreg=as.matrix(earbud_xreg2))

earbud_forecast_arima

earbud_forecast_arima_xts<-xts(earbud_forecast_arima$mean,order.by=validation_dates)

plot(dates,earbud$sold_count)

lines(validation_dates,earbud_forecast_arima_xts,col="blue")

earbud_ARIMA_MAPE<-100*mean(abs((earbud_forecast_arima_xts-as.numeric(earbud_valid_xts$sold_count))/as.numeric(earbud_valid_xts$sold_count)))
earbud_ARIMA_MAPE

############################################################################
###Earbud Regression

earbud_lm_train<-earbud[52:306,]
earbud_lm_valid<-earbud[307:n,]

earbud_lm_model<-lm(sold_count~favored_count,data=earbud_lm_train)

earbud_lm_model

pred_earbud_lm_model<-predict(earbud_lm_model,earbud_lm_valid)

pred_earbud_lm_model

pred_earbud_lm_model_xts<-xts(pred_earbud_lm_model,order.by=validation_dates)

plot(pred_earbud_lm_model_xts)

plot(dates,earbud$sold_count)
lines(validation_dates,pred_earbud_lm_model_xts,col="blue")

earbud_reg_MAPE<-100*mean(abs((pred_earbud_lm_model_xts-as.numeric(earbud_valid_xts$sold_count))/as.numeric(earbud_valid_xts$sold_count)))
earbud_reg_MAPE

#earbud_regression_MAPE<-100*mean(abs((pred_earbud_lm_model_xts-earbud_valid_MAPE)/earbud_valid_MAPE))
#earbud_regression_MAPE

#########################################################################
###Earbud ARIMA Submission Forecast
earbud_price_xreg<-as.numeric(earbud_xts$price)
earbud_price_model<-auto.arima(earbud_price_xreg)
earbud_price_forecast<-forecast(earbud_price_model,h=2)

earbud_favored_xreg<-as.numeric(earbud_xts$favored_count)
earbud_favored_model<-auto.arima(earbud_favored_xreg)
earbud_favored_forecast<-forecast(earbud_favored_model,h=2)
##################################################################

earbud_submission_xreg1<-cbind(earbud[1:n,"favored_count"],
                               earbud[1:n,"price"])
earbud_submission_xreg2<-data.table("favored_count"=earbud_favored_forecast$mean,
                                    "price"=earbud_price_forecast$mean)

########## Arima submission earbud
earbud_submission_xts<-as.numeric(earbud_xts$sold_count)

earbud_submission_arima_model<-Arima(earbud_submission_xts,xreg=as.matrix(earbud_submission_xreg1),order=c(0,1,3))

earbud_submission_forecast_arima<-forecast(earbud_submission_arima_model,xreg=as.matrix(earbud_submission_xreg2))
#########Regression submission
earbud_lm_submission<-lm(sold_count~favored_count+price,data=earbud)

pred_earbud_lm_submodel<-predict(earbud_lm_submission,earbud_submission_xreg2)

##########
earbud_submission_arima<-0.5*(earbud_submission_forecast_arima$mean[2]+pred_earbud_lm_submodel[2])
earbud_submission_arima
########################################################




########################################################        END Earbud                    #############################################################################



##############################################7061886 Elektronik S¨up¨urge Fakir ######################################



sweeper<-subset(data, data$product_content_id==7061886)

sweeper

plot(dates,sweeper$sold_count)


sweeper_xts<-xts(sweeper,order.by=dates)


sweeper_train_xts<-sweeper_xts[index(sweeper_xts)<"2020-03-01" & index(sweeper_xts)>"2019-07-26"]


sweeper_valid_xts<-sweeper_xts[index(sweeper_xts)>="2020-03-01"]

#To creata data table of extra regressor for model
sweeper_xreg1<-cbind(sweeper[89:306,"favored_count"],
                     sweeper[89:306,"basket_count"],
                     sweeper[89:306,"price"])

sweeper_xreg2<-cbind(sweeper[307:n,"favored_count"],
                     sweeper[307:n,"basket_count"],
                     sweeper[307:n,"price"])



sweeper_arima_model<-Arima(as.numeric(sweeper_train_xts$sold_count),
                           xreg=as.matrix(sweeper_xreg1),
                           order=c(4,0,0))


AIC(sweeper_arima_model)



sweeper_forecast_arima<-forecast(sweeper_arima_model,xreg=as.matrix(sweeper_xreg2))

sweeper_forecast_arima


sweeper_forecast_arima_xts<-xts(sweeper_forecast_arima$mean,order.by=validation_dates)

sweeper_forecast_arima_xts


plot(dates,sweeper$sold_count)

lines(validation_dates,sweeper_forecast_arima_xts,col="blue")

sweeper_ARIMA_MAPE<-100*mean(abs((sweeper_forecast_arima_xts-as.numeric(sweeper_valid_xts$sold_count))/as.numeric(sweeper_valid_xts$sold_count)))
sweeper_ARIMA_MAPE

####Regression Sweeper

sweeper_lm_train<-sweeper[89:306,]
sweeper_lm_valid<-sweeper[307:n,]

sweeper_lm_model<-lm(sold_count~favored_count+price,data=earbud_lm_train)

pred_sweeper_lm_model<-predict(sweeper_lm_model,sweeper_lm_valid)

pred_sweeper_lm_model_xts<-xts(pred_sweeper_lm_model,order.by = validation_dates)

plot(pred_sweeper_lm_model_xts)

plot(dates,sweeper$sold_count)
lines(validation_dates,pred_sweeper_lm_model_xts,col="blue")

sweeper_reg_MAPE<-100*mean(abs((sweeper_forecast_arima_xts-as.numeric(sweeper_valid_xts$sold_count))/as.numeric(sweeper_valid_xts$sold_count)))
sweeper_reg_MAPE




###Earbud ARIMA Submission Forecast
sweeper_price_xreg<-as.numeric(sweeper_xts$price)
sweeper_price_model<-auto.arima(sweeper_price_xreg)
sweeper_price_forecast<-forecast(sweeper_price_model,h=2)

sweeper_favored_xreg<-as.numeric(sweeper_xts$favored_count)
sweeper_favored_model<-auto.arima(sweeper_favored_xreg)
sweeper_favored_forecast<-forecast(sweeper_favored_model,h=2)

sweeper_basket_xreg<-as.numeric(sweeper_xts$basket_count)
sweeper_basket_model<-auto.arima(sweeper_basket_xreg)
sweeper_basket_forecast<-forecast(sweeper_basket_model,h=2)

###########
sweeper_submission_xreg1<-cbind(sweeper[1:n,"favored_count"],
                                sweeper[1:n,"basket_count"],
                                sweeper[1:n,"price"])
sweeper_submission_xreg2<-data.table("favored_count"=sweeper_favored_forecast$mean,
                                     "basket_count"=sweeper_basket_forecast$mean,
                                     "price"=sweeper_price_forecast$mean)

###ARIMA Submission Forecast
sweeper_submission_xts<-as.numeric(sweeper_xts$sold_count)

sweeper_submission_arima_model<-Arima(sweeper_submission_xts,
                                      xreg=as.matrix(sweeper_submission_xreg1),
                                      order=c(4,0,0))

sweeper_submission_forecast_arima<-forecast(sweeper_submission_arima_model,
                                            xreg=as.matrix(sweeper_submission_xreg2))

#########Regression submission
sweeper_lm_submission<-lm(sold_count~favored_count+basket_count+price,data=sweeper)

pred_sweeper_lm_submodel<-predict(sweeper_lm_submission,sweeper_submission_xreg2)


sweeper_submission_arima<-0.5*(sweeper_submission_forecast_arima$mean[2]+pred_sweeper_lm_submodel[2])
sweeper_submission_arima
################################################################SWEEPER ENDS################################################################################################



################################################################31515569 Giyim Tayt TRENDYOLM'ILLA############################################################


############Tight ARIMA



tight<-subset(data, data$product_content_id==31515569)

tight

plot(dates,tight$sold_count)


tight_xts<-xts(tight,order.by=dates)

tight_train_xts<-tight_xts[index(tight_xts)<"2020-03-01"&index(tight_xts)>"2019-09-21"]
tight_valid_xts<-tight_xts[index(tight_xts)>="2020-03-01"]


tight_train_sold_xts<-as.numeric(tight_train_xts$sold_count)

tight_valid_MAPE<-as.numeric(tight_valid_xts$sold_count)

#To creata data table of extra regressor for model
tight_xreg1<-cbind(tight[146:306,"favored_count"],
                   tight[146:306,"basket_count"],
                   
                   tight[146:306,"price"])

tight_xreg2<-cbind(tight[307:n,"favored_count"],
                   tight[307:n,"basket_count"],
                   
                   tight[307:n,"price"])

tight_arima_model<-Arima(as.numeric(tight_train_xts$sold_count),
                         xreg=as.matrix(tight_xreg1),
                         order=c(0,2,4))

AIC(tight_arima_model)


tight_forecast_arima<-forecast(tight_arima_model,
                               xreg=as.matrix(tight_xreg2))

tight_forecast_arima


tight_forecast_arima_xts<-xts(tight_forecast_arima$mean,order.by=validation_dates)

tight_forecast_arima_xts

plot(dates,tight$sold_count)

lines(validation_dates,tight_forecast_arima_xts,col="blue")

tight_ARIMA_MAE<-mean(abs((tight_forecast_arima_xts-as.numeric(tight_valid_xts$sold_count))))
tight_ARIMA_MAE

####Regression Tight

tight_lm_train<-tight[146:306,]
tight_lm_valid<-tight[307:n,]


tight_lm_model<-lm(sold_count~favored_count+basket_count+price,data=tight_lm_train)
pred_tight_lm_model<-predict(tight_lm_model,tight_lm_valid)
pred_tight_lm_model_xts<-xts(pred_tight_lm_model,order.by = validation_dates)

plot(pred_tight_lm_model_xts)

plot(dates,tight$sold_count)
lines(validation_dates,pred_tight_lm_model_xts,col="blue")

tight_reg_MAE<-mean(abs((pred_tight_lm_model_xts-as.numeric(tight_valid_xts$sold_count))))
tight_reg_MAE


#########################################################################
###Earbud ARIMA Submission Forecast
tight_price_xreg<-as.numeric(tight_xts$price)
tight_price_model<-auto.arima(tight_price_xreg)
tight_price_forecast<-forecast(tight_price_model,h=2)

tight_favored_xreg<-as.numeric(tight_xts$favored_count)
tight_favored_model<-auto.arima(tight_favored_xreg)
tight_favored_forecast<-forecast(tight_favored_model,h=2)

tight_basket_xreg<-as.numeric(tight_xts$basket_count)
tight_basket_model<-auto.arima(tight_basket_xreg)
tight_basket_forecast<-forecast(tight_basket_model,h=2)
##################################################################

tight_submission_xreg1<-cbind(tight[1:n,"favored_count"],
                              tight[1:n,"basket_count"],
                              tight[1:n,"price"])
tight_submission_xreg2<-data.table("favored_count"=tight_favored_forecast$mean,
                                   "basket_count"=tight_basket_forecast$mean,
                                   "price"=tight_price_forecast$mean)

#ARIMA
tight_submission_xts<-as.numeric(tight_xts$sold_count)

tight_submission_arima_model<-Arima(tight_submission_xts,
                                    xreg=as.matrix(tight_submission_xreg1),
                                    order=c(0,2,4))

tight_submission_forecast_arima<-forecast(tight_submission_arima_model,
                                          xreg=as.matrix(tight_submission_xreg2))


#########Regression submission
tight_lm_submission<-lm(sold_count~favored_count+basket_count+price,data=tight)

pred_tight_lm_submodel<-predict(tight_lm_submission,tight_submission_xreg2)



tight_submission_arima<-0.5*(tight_submission_forecast_arima$mean[2]+pred_tight_lm_submodel[2])
tight_submission_arima
#770.1799 

##############################################TIGHT ENDS#################################################################################################################

##############################################5926527 Giyim Bikini Ust¨u TRENDYOLM ¨ 'ILLA#############################################################################


bikini<-subset(data, data$product_content_id==5926527)

bikini

plot(dates,bikini$sold_count)

bikini_xts<-xts(bikini,order.by=dates)
bikini_train_xts<-bikini_xts[index(bikini_xts)<"2020-03-01"]

bikini_valid_xts<-bikini_xts[index(bikini_xts)>="2020-03-01"]

#To creata data table of extra regressor for model
bikini_xreg1<-cbind(bikini[1:306,"favored_count"],
                    bikini[1:306,"price"])

bikini_xreg2<-cbind(bikini[307:n,"favored_count"],
                    bikini[307:n,"price"])

auto.arima(as.numeric(bikini_train_xts$sold_count))
bikini_arima_model<-Arima(as.numeric(bikini_train_xts$sold_count),
                          xreg=as.matrix(bikini_xreg1),
                          order=c(0,1,3))

AIC(bikini_arima_model)


bikini_forecast_arima<-forecast(bikini_arima_model,
                                xreg=as.matrix(bikini_xreg2))

bikini_forecast_arima


bikini_forecast_arima_xts<-xts(bikini_forecast_arima$mean,order.by=validation_dates)

bikini_forecast_arima_xts

plot(dates,bikini$sold_count)

lines(validation_dates,bikini_forecast_arima_xts,col="blue")
####BIKINI REGRESSION

bikini_lm_train<-bikini[1:306,]
bikini_lm_valid<-bikini[307:n,]


bikini_lm_model<-lm(sold_count~favored_count+price,data=bikini_lm_train)

pred_bikini_lm_model<-predict(bikini_lm_model,bikini_lm_valid)

pred_bikini_lm_model_xts<-xts(pred_bikini_lm_model,order.by = validation_dates)

plot(pred_bikini_lm_model_xts)

plot(dates,bikini$sold_count)
lines(validation_dates,pred_bikini_lm_model_xts,col="blue")

###Earbud ARIMA Submission Forecast
bikini_price_xreg<-as.numeric(bikini_xts$price)
bikini_price_model<-auto.arima(bikini_price_xreg)
bikini_price_forecast<-forecast(bikini_price_model,h=2)

bikini_favored_xreg<-as.numeric(bikini_xts$favored_count)
bikini_favored_model<-auto.arima(bikini_favored_xreg)
bikini_favored_forecast<-forecast(bikini_favored_model,h=2)
##################################################################

bikini_submission_xreg1<-cbind(bikini[1:n,"favored_count"],
                               bikini[1:n,"price"])
bikini_submission_xreg2<-data.table("favored_count"=bikini_favored_forecast$mean,
                                    "price"=bikini_price_forecast$mean)


###ARIMA Submission Forecast

bikini_submission_xts<-as.numeric(bikini_xts$sold_count)

bikini_submission_arima_model<-Arima(bikini_submission_xts,
                                     xreg=as.matrix(bikini_submission_xreg1),
                                     order=c(0,1,3))

bikini_submission_forecast_arima<-forecast(bikini_submission_arima_model,
                                           xreg=as.matrix(bikini_submission_xreg2) )
#########Regression submission
bikini_lm_submission<-lm(sold_count~favored_count+price,data=bikini)

pred_bikini_lm_submodel<-predict(bikini_lm_submission,bikini_submission_xreg2)


bikini_submission_arima<-0.5*(bikini_submission_forecast_arima$mean[2]+pred_bikini_lm_submodel[2])

bikini_submission_arima

#######################################################BIKINI ENDS############################################################
mont_submission_arima<-0

submission_list<-list(earbud_submission_arima,toothbrush_submission_arima,tight_submission_arima,cosmetic_submission_arima,
                      mont_submission_arima,sweeper_submission_arima,napkin_submission_arima,bikini_submission_arima)
submission_list


submission_productlist<-list(6676673,32939029,31515569,85004,3904356,7061886,4066298,5926527)


submission_df<-data.table("product_content_id"=submission_productlist, "forecast"=submission_list)

submission_df$forecast<-as.numeric(submission_df$forecast)

submission_df

check_format(submission_df)

send_submission(submission_df, token, url=subm_url, submit_now=T)



submission_check_ourpredictions_12Haziran<-submission_df
submission_check_ourpredictions_12Haziran

submission_check_real<-subset(data,data$event_date=="2020-06-08")
submission_check_real