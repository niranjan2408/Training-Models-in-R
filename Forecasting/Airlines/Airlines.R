library(forecast)
library(fpp)
library(smooth)
library(readxl)

Airlines<-read_excel(file.choose())
View(Airlines) 
# Seasonality 12 months

#Plotting graph of Months vs Passengers
windows()
plot(Airlines$Month,Airlines$Passengers,type="o")



# creating 12 dummy variables as Seasonality is of 12 months. Keeping length 96 as we have 96 observations
X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )
View(X)
# Assigning month names
colnames(X)<-month.abb 
View(X)

AirlinesData<-cbind(Airlines,X)
View(AirlinesData)

# creating required vatiavle columns
AirlinesData["t"]<- 1:96
AirlinesData["log_Passengers"]<-log(AirlinesData["Passengers"])
AirlinesData["t_square"]<-AirlinesData["t"]*AirlinesData["t"]


attach(AirlinesData)

# splitting data into train and test

train<-AirlinesData[1:84,]

test<-AirlinesData[85:96,]


########################### LINEAR MODEL #############################

linear_model<-lm(Passengers~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
rmse_linear<-sqrt(mean((test$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear # 53.19924



######################### Exponential #################################

expo_model<-lm(log_Passengers~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 46.05736

######################### Quadratic ####################################

Quad_model<-lm(Passengers~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 48.05189

######################### Additive Seasonality #########################

sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 132.8198

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Passengers-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 35.34896

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 26.36082

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 140.0632

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 10.51917

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Linear","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Linear,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
colnames(table_rmse)<-c("model","RMSE")
table_rmse$RMSE <- round(table_rmse$RMSE,0)
View(table_rmse)

# Multiplicative Seasonality Linear trend has least RMSE value so we can consider this s a final model

#Building Final Model with complete data set

Final_Model <- lm(log_Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = AirlinesData)

#######################################3############## Predicting new data on final model ###############################3#############

#Creating New datarame for required forcast (here I am taking forecast of next 12 months)
Month1 <- seq(from = as.Date("2003-01-01"), to = as.Date("2003-12-01"), by = 'month')
X1 <- data.frame(outer(rep(month.abb,length = 12), month.abb,"==") + 0 )
colnames(X1)<-month.abb
t1 <- 97:108

ForecastData <- data.frame(Month1,X1,t1)
ForecastData$Passengers <- NA   #creating Null variable to store predicted values
colnames(ForecastData)[1]<- paste("Month") #changing column name as per final model
colnames(ForecastData)[14]<- paste("t") #changing column name as per final model

ForecastData <- cbind(ForecastData[1],ForecastData[15],ForecastData[2:14]) #rearrangong the columns

pred_ForecastData <- predict(Final_Model, newdata = ForecastData) #predicting the ForecastData with Final_Model

Passengers_ForecastData <- round(exp(pred_ForecastData),0) #taking exponential as Y in final model is log Y

ForecastData$Passengers <- Passengers_ForecastData #storing predicted values in Passengers column

ForecastData_Passengers <- ForecastData[,-c(3:15)] # removing created dummy columns

View(ForecastData_Passengers)

################################################################################################################################

#checking if residuals of final model are carring any informtion

Final_Model_residuals <- residuals(Final_Model)
Final_Model_residuals[1:10]
windows()
acf(Final_Model_residuals,lag.max = 12) 
# Majority of lags are above +-2 standard error. That means errors contain some information
# By principal of parcimony we will consider lag - 1  as we have so many significant lags 
# Building Autoregressive model on residuals consider lag-1 

k <- arima(Final_Model_residuals, order=c(1,0,0))
str(k)

View(data.frame(ressid=resid,newresid=k$residuals))
windows()
acf(k$residuals,lag.max = 12)
#All lags are below +-2 standard error. That means errors doesnot contain any information

#forecasting next 12 residuals
Forecast_residuals<- predict(k,n.ahead = 12) 
str(Forecast_residuals)
Forecast_residuals$pred

#taking exponential as errors are in log
ForecastData_Passengers["Forecasted_Errors"] <- exp(Forecast_residuals$pred) 

#Adding forecasted errors to forecasted predictions
ForecastData_Passengers["Final_ForecastData_Passengers"] <- (ForecastData_Passengers$Passengers+ForecastData_Passengers$Forecasted_Errors)

#rounding off the numbers as number of Passengers is discrete count
ForecastData_Passengers$Forecasted_Errors <-  round(ForecastData_Passengers$Forecasted_Errors,0)

ForecastData_Passengers$Final_ForecastData_Passengers <- round(ForecastData_Passengers$Final_ForecastData_Passengers,0) 

View(ForecastData_Passengers)

#Saving the final forecasted data frame in to excel file
write.csv(ForecastData_Passengers,file="ForecastData_Passengers.csv",col.names = F,row.names = F)
