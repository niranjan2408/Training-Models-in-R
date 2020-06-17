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
library(GGally)
library(car)

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

ggpairs(corolla)
cor2pcor(cor(corolla))

model1_all<-lm(Price~.,corolla)
summary(model1_all)
#vif(model1_all)
#avPlots(model1_all,id.n=2,id.cex=0.7)
#influenceIndexPlot(model1_all,id.n=5)

corolla_woDoors<-corolla[,-6]
model2_woDoors<-lm(Price~.,corolla_woDoors)
summary(model2_woDoors)
anova(model2_woDoors,model1_all)

corolla_woDoorsCc<-corolla[,-c(5,6)]
model3_woDoorsCc<-lm(Price~.,corolla_woDoorsCc)
summary(model3_woDoorsCc)
anova(model3_woDoorsCc,model2_woDoors)
vif(model3_woDoorsCc)
influenceIndexPlot(model3_woDoorsCc,id.n=5)

corolla_woDoorsCcInfluencers<-corolla_woDoorsCc[-c(222,602,961),]
model4_woDoorsCcInfluencers<-lm(Price~.,corolla_woDoorsCcInfluencers)
summary(model4_woDoorsCcInfluencers)

FINAL_COROLLA<-corolla_woDoorsCcInfluencers[,-6]
FINAL_MODEL<-lm(Price~.,FINAL_COROLLA)
summary(FINAL_MODEL)
anova(FINAL_MODEL,model4_woDoorsCcInfluencers)

vif(FINAL_MODEL)
plot(FINAL_MODEL)
hist(residuals(FINAL_MODEL))
boxplot(FINAL_MODEL$residuals, horizontal = T)
plot(FINAL_COROLLA$Price,resid(FINAL_MODEL),ylab="Residuals", xlab="Price",main="Toyota_Corolla")
abline(0, 0)
plot(FINAL_MODEL$fitted.values,FINAL_COROLLA$Price,pch = 5, col = c("red", "blue"))
