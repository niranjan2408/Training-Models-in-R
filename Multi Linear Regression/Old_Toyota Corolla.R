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

hist(log(corolla$Price))
boxplot(log(corolla$Price))
skewness(log(corolla$Price))
kurtosis(log(corolla$Price))

Price_log<-log(corolla$Price)
corolla_Price_log<-cbind(corolla[,2:9],Price_log)
names(corolla_Price_log)[9]<-c("Price")
shapiro.test(corolla_Price_log$Price)

pairs.panels(corolla_Price_log, col="red")
cor2pcor(cor(corolla_Price_log))

model1_all<-lm(Price~.,corolla)
summary(model1_all)
model1_Price_log_all<-lm(Price~.,corolla_Price_log)
summary(model1_Price_log_all)

influenceIndexPlot(model1_Price_log_all,id.n=5)

corolla_Price_log_woInfluencers <- corolla_Price_log[-c(81,192,193,222,602),]
model_woInfluencers<-lm(corolla_Price_log_woInfluencers$Price~.,corolla_Price_log_woInfluencers)
summary(model_woInfluencers)

hist(residuals(model_woInfluencers))
boxplot(model_woInfluencers$residuals, horizontal = T)
plot(corolla_Price_log_woInfluencers$Price,resid(model_woInfluencers),ylab="Residuals", xlab="Price",main="Toyota_Corolla")
abline(0, 0)
plot(model_woInfluencers$fitted.values,corolla_Price_log_woInfluencers$Price,pch = 5, col = c("red", "blue"))

