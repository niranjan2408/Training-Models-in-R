#Consider only the below columns and prepare a prediction model for predicting Price.
#Corolla<-Corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
#Price  -- Offer Price in EUROs	
#Age_08_04 -- Age in months as in August 2004	
#KM -- Accumulated Kilometers on odometer
#HP -- Horse Power
#cc -- Cylinder Volume in cubic centimeters
#Doors -- Number of doors
#Gears -- Number of gear positions
#Quarterly_Tax -- Quarterly road tax in EUROs
#Weight -- Weight in Kilograms


Toyota_Corolla<-read.csv(choose.files())
corolla<-Toyota_Corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
sum(is.na(corolla))
str(corolla)
summary(corolla)

library(moments)
library(corpcor)
library(car)
library(psych)
par(mfrow=c(1,2))


hist(corolla$Price)
boxplot(corolla$Price)$out
skewness(corolla$Price)
kurtosis(corolla$Price)

hist(corolla$Age_08_04)
boxplot(corolla$Age_08_04)$out
skewness(corolla$Age_08_04)
kurtosis(corolla$Age_08_04)

hist(corolla$KM)
boxplot(corolla$KM)$out
skewness(corolla$KM)
kurtosis(corolla$KM)

hist(corolla$HP)
boxplot(corolla$HP)$out
skewness(corolla$HP)
kurtosis(corolla$HP)

hist(corolla$cc)
boxplot(corolla$cc)$out
skewness(corolla$cc)
kurtosis(corolla$cc)

hist(corolla$Doors)
boxplot(corolla$Doors)$out
skewness(corolla$Doors)
kurtosis(corolla$Doors)

hist(corolla$Gears)
boxplot(corolla$Gears)$out
skewness(corolla$Gears)
kurtosis(corolla$Gears)

hist(corolla$Quarterly_Tax)
boxplot(corolla$Quarterly_Tax)$out
skewness(corolla$Quarterly_Tax)
kurtosis(corolla$Quarterly_Tax)

hist(corolla$Weight)
boxplot(corolla$Weight)$out
skewness(corolla$Weight)
kurtosis(corolla$Weight)

pairs.panels(corolla, col="red")
cor2pcor(cor(corolla))


model1<-lm(Price~.,corolla)
summary(model1)
RMSE_model1<-sqrt(mean(model1$residuals^2))
RMSE_model1
plot(model1) #222,602,81,961 influencers
vif(model1)
avPlots(model1)
influenceIndexPlot(model1)
influencePlot(model1)
hist(residuals(model1))
boxplot(model1$residuals, horizontal = T)
plot(corolla$Price,resid(model1),ylab="Residuals", xlab="Profit",main="comp_data")
abline(0, 0)
plot(model1$fitted.values,corolla$Price,pch = 5, col = c("red", "blue"))


corolla_woInf <- corolla[-c(81,222,602,961),]
model2<-lm(corolla_woInf$Price~.,corolla_woInf)
summary(model2)
RMSE_model2<-sqrt(mean(model2$residuals^2))
RMSE_model2
plot(model2) #148,524,655,992,957 influencers
vif(model2)
avPlots(model2)
influenceIndexPlot(model2)
influencePlot(model2)
hist(residuals(model2))
boxplot(model2$residuals, horizontal = T)
plot(corolla_woInf$Price,resid(model2),ylab="Residuals", xlab="Profit",main="comp_data")
abline(0, 0)
plot(model2$fitted.values,corolla_woInf$Price,pch = 5, col = c("red", "blue"))

pairs.panels(corolla_woInf, col="red")


model3<-lm(Price~log(Age_08_04)+sqrt(KM)+HP+log(cc)+exp(Doors^2)+Gears+Quarterly_Tax+
                  log(Weight),data=corolla_woInf [-c(185,186,524),])
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
plot(corolla_woInf$Price,resid(model3),ylab="Residuals", xlab="Profit",main="comp_data")
abline(0, 0)
plot(model3$fitted.values,corolla_woInf$Price,pch = 5, col = c("red", "blue"))


# Model 2 is better with Multiple R-squared:  0.8894,	Adjusted R-squared:  0.8888 
# Lowest RMSE_model2 = 1204.713

corolla_woInf <- corolla[-c(81,222,602,961,185,186,524),]
model4<-lm(corolla_woInf$Price~.,corolla_woInf)
summary(model4)
RMSE_model4<-sqrt(mean(model4$residuals^2))
RMSE_model4
plot(model4) #148,524,655,992,957 influencers
vif(model4)
avPlots(model4)
influenceIndexPlot(model4)
influencePlot(model4)
hist(residuals(model4))
boxplot(model4$residuals, horizontal = T)
plot(corolla_woInf$Price,resid(model4),ylab="Residuals", xlab="Profit",main="comp_data")
abline(0, 0)
plot(model4$fitted.values,corolla_woInf$Price,pch = 5, col = c("red", "blue"))

# Model 4 is better as compared to Model 2 with Multiple R-squared:  0.8904,	Adjusted R-squared:  0.8898 
# Lowest RMSE_model4 = 1194.866

