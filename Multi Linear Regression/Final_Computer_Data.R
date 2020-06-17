#Predict Price of the computer
#A dataframe containing :
#price : price in US dollars of 486 PCs
#speed : clock speed in MHz
#hd : size of hard drive in MB
#ram : size of Ram in in MB
#screen : size of screen in inches
#cd : is a CD-ROM present ?
#multi : is a multimedia kit (speakers, sound card) included ?
#premium : is the manufacturer was a "premium" firm (IBM, COMPAQ) ?
#ads : number of 486 price listings for each month
#trend : time trend indicating month starting from January of 1993 to November of 1995.

comp_data<-read.csv(choose.files())
comp_data<-comp_data[,-1]
str(comp_data)
summary(comp_data)
sum(is.na(comp_data))

library(moments)
library(car)
par(mfrow=c(1,2))

hist(comp_data$price)
boxplot(comp_data$price)
skewness(comp_data$price)
kurtosis(comp_data$price)
boxplot(log(comp_data$price))
skewness(log(comp_data$price))
kurtosis(log(comp_data$price))

hist(comp_data$speed)
boxplot(comp_data$speed)
skewness(comp_data$speed)
kurtosis(comp_data$speed)
boxplot(log(comp_data$speed))
skewness(log(comp_data$speed))
kurtosis(log(comp_data$speed))

hist(comp_data$hd)
boxplot(comp_data$hd)
skewness(comp_data$hd)
kurtosis(comp_data$hd)
boxplot(log(comp_data$hd))
skewness(log(comp_data$hd))
kurtosis(log(comp_data$hd))

hist(comp_data$ram)
boxplot(comp_data$ram)
skewness(comp_data$ram)
kurtosis(comp_data$ram)
boxplot(log(comp_data$ram))
skewness(log(comp_data$ram))
kurtosis(log(comp_data$ram))

hist(comp_data$screen)
boxplot(comp_data$screen)
skewness(comp_data$screen)
kurtosis(comp_data$screen)
boxplot(log(comp_data$screen))
skewness(log(comp_data$screen))
kurtosis(log(comp_data$screen))


hist(comp_data$ads)
boxplot(comp_data$ads)
skewness(comp_data$ads)
kurtosis(comp_data$ads)
boxplot((comp_data$ads)^2)
skewness((comp_data$ads)^2)
kurtosis((comp_data$ads)^2)

hist(comp_data$trend)
boxplot(comp_data$trend)
skewness(comp_data$trend)
kurtosis(comp_data$trend)



pairs.panels(comp_data, col="red")
model1<-lm(comp_data$price~.,comp_data) # model1 with the given data
summary(model1)
RMSE_model1<-sqrt(mean(model1$residuals^2))
RMSE_model1
plot(model1)
vif(model1)
avPlots(model1)
influenceIndexPlot(model1)
influencePlot(model1)
hist(residuals(model1))
boxplot(model1$residuals, horizontal = T)
plot(comp_data$price,resid(model1),ylab="Residuals", xlab="Profit",main="comp_data")
abline(0, 0)
plot(model1$fitted.values,comp_data$price,pch = 5, col = c("red", "blue"))




new_data<-data.frame (cbind(
  log(comp_data$price),
  log(comp_data$speed),
  log(comp_data$hd),
  log(comp_data$ram),
  log(comp_data$screen),
  comp_data$cd,
  comp_data$multi,
  comp_data$premium,
  comp_data$ads^2,
  comp_data$trend
))

names(new_data)<-c("price","speed","hd","ram","screen","cd","multi","premium","ads","trend")

pairs.panels(new_data, col="red")
model2<-lm(new_data$price~.,new_data) # model2 is with transformed data
summary(model2)
RMSE_model2<-sqrt(mean(model2$residuals^2))
RMSE_model2
plot(model2)
vif(model2)
avPlots(model2)
influenceIndexPlot(model2)
influencePlot(model2)
hist(residuals(model2))
boxplot(model2$residuals, horizontal = T)
plot(new_data$price,resid(model2),ylab="Residuals", xlab="Profit",main="comp_data")
abline(0, 0)
plot(model2$fitted.values,new_data$price,pch = 5, col = c("red", "blue"))



removed_influencers_new_data<-new_data[-c(80,1689,1785,1441,4328),] #removing influencing entries


model3<-lm(removed_influencers_new_data$price~.,removed_influencers_new_data) # model3 with transformed data with removing influencing entries.
summary(model3)
RMSE_model3<-sqrt(mean(model3$residuals^2))
RMSE_model3
plot(model3)
vif(model3)
avPlots(model3)
influenceIndexPlot(model3)
influencePlot(model3)
hist(residuals(model3))
boxplot(model3$residuals, horizontal = T)
plot(removed_influencers_new_data$price,resid(model3),ylab="Residuals", xlab="Profit",main="comp_data")
abline(0, 0)
plot(model3$fitted.values,removed_influencers_new_data$price,pch = 5, col = c("red", "blue"))



#Model3 has all pedictors signicicant p values. 
#Model3 has highest R-surared value of 0.792 and Adjusted R-squared:  0.7917
#Model3 has lowest RMSE of 0.1175089
