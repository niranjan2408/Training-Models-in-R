library(forecast)
library(fpp)
library(smooth)
library(readxl)

Cocacola<-read_excel(file.choose())
View(Cocacola) 
# Seasonality 4 Quarters

#Plotting graph of Months vs Passengers
windows()
plot(Cocacola$Sales,type="o")

#creating dummy variables
Q1 <-  ifelse(grepl("Q1",Cocacola$Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2",Cocacola$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",Cocacola$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",Cocacola$Quarter),'1','0')

#including dummy quartely variables to main data frame
CocacolaData<-cbind(Cocacola,Q1,Q2,Q3,Q4)

# creating required vatiavle columns
CocacolaData["t"]<- 1:42
CocacolaData["log_Sales"]<-log(CocacolaData["Sales"])
CocacolaData["t_square"]<-CocacolaData["t"]*CocacolaData["t"]

attach(CocacolaData)

# splitting data into train and test

train<-CocacolaData[1:36,]

test<-CocacolaData[37:42,]

########################### LINEAR MODEL #############################

linear_model<-lm(Sales~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear # 667.4257



######################### Exponential #################################

expo_model<-lm(log_Sales~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 526.7673

######################### Quadratic ####################################

Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 485.1407

######################### Additive Seasonality #########################

sea_add_model<-lm(Sales~Q1+Q2+Q3,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 1895.559

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Sales~t+Q1+Q2+Q3,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 555.3404

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_square+Q1+Q2+Q3,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 283.062

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_Sales~Q1+Q2+Q3,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 1980.534

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_Sales~t+Q1+Q2+Q3,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 323.2128

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Linear","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Linear,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
colnames(table_rmse)<-c("model","RMSE")
table_rmse$RMSE <- round(table_rmse$RMSE,0)
View(table_rmse)

# Additive Seasonality with Quadratic trend  has least RMSE value

#Building Final Model with complete data set

Final_Model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=CocacolaData)

###################################################### Actual vs Fitted/Predicted Graph ###########################

# Add a Actual sales line
plot(CocacolaData$t,CocacolaData$Sales, type="o", pch=19, col="red", xlab="Quarters", ylab="Sales",main = "Actual vs Predicted Graph")
# Add a Predicted/Fitted sales line
lines(CocacolaData$t,Final_Model$fitted.values,pch=19, col="blue", type="o", lty=2)
# Add a legend
legend("topleft", legend = c("Actual Graph", "Predicted Graph"), col = c("red", "blue"),pch = 19)

##################################################### Predicting new data on final model ###############################3#############

Quarters <- seq(from = as.Date("1996-09-30"), to = as.Date("1997-12-31"), by = 'quarter')
Quarters_dummies <- cbind(Q1[3:8],Q2[3:8],Q3[3:8],Q4[3:8])
colnames(Quarters_dummies) <- c("Q1","Q2","Q3","Q4")
t1 <- 43:48
t1_square <- t1^2

ForecastData <- data.frame(Quarters,Quarters_dummies,t1,t1_square)
ForecastData$Forecast_Sales <- NA   #creating Null variable to store predicted values
colnames(ForecastData)[6]<- paste("t") #changing column name as per final model
colnames(ForecastData)[7]<- paste("t_square") #changing column name as per final model


ForecastData <- cbind(ForecastData[1],ForecastData[8],ForecastData[2:7]) #rearrangong the columns

pred_ForecastData <- predict(Final_Model, newdata = ForecastData) #predicting the ForecastData with Final_Model

ForecastData$Forecast_Sales <- pred_ForecastData #storing predicted values in Passengers column

ForecastData_Sales <- ForecastData[,-c(3:8)] # removing created dummy columns

View(ForecastData_Sales)

################################################################################################################################

#checking if residuals of final model are carring any informtion

Final_Model_residuals <- residuals(Final_Model)
Final_Model_residuals[1:10]
windows()
acf(Final_Model_residuals,lag.max = 4) 
# First lag is above +-2 standard error. That means errors contain some information
# By principal of parcimony we will consider lag - 1  as we have so many significant lags 
# Building Autoregressive model on residuals consider lag-1 

k <- arima(Final_Model_residuals, order=c(1,0,0))
str(k)

View(data.frame(ressid=resid,newresid=k$residuals))
windows()
acf(k$residuals,lag.max = 4)

# Second lag is above +-2 standard error. That means errors contain some information
# Building Autoregressive model on residuals consider lag-2 
k <- arima(Final_Model_residuals, order=c(2,0,0))
str(k)

View(data.frame(ressid=resid,newresid=k$residuals))
windows()
acf(k$residuals,lag.max = 4)

#All lags are below +-2 standard error. That means errors doesnot contain any information

#forecasting next 12 residuals
Forecast_residuals<- predict(k,n.ahead = 6) 
str(Forecast_residuals)
Forecast_residuals$pred

# adding errors column into forecast sheet
ForecastData_Sales["Forecasted_Errors"] <- Forecast_residuals$pred

#Adding forecasted errors to forecasted sales figures
ForecastData_Sales["Final_Sales_Forecast_Data"] <- round((ForecastData_Sales$Forecast_Sales+ForecastData_Sales$Forecasted_Errors),0)

View(ForecastData_Sales)

#Saving the final forecasted data frame in to excel file
write.csv(ForecastData_Sales,file="Quaterly_Sales_Forecast.csv",col.names = F,row.names = F)
