#Prepare a prediction model for profit of 50_startups data.
#Do transformations for getting better predictions of profit and make a table containing R^2 value for each prepared model.
#R&D Spend -- Research and devolop spend in the past few years
#Administration -- spend on administration in the past few years
#Marketing Spend -- spend on Marketing in the past few years
#State -- states from which data is collected
#Profit  -- profit of each state in the past few years

#Y= Profir
#X1 = R.D.Spend, X2 = Administration, X3 = Marketing.Spend, X4 = State

startups_data <- read.csv(choose.files())

summary(startups_data)
str(startups_data)
sum(is.na(startups_data))

attach(startups_data)
library(moments)
library(corpcor)
library(car)
library(psych)
par(mfrow=c(1,2))

hist(R.D.Spend)
boxplot(R.D.Spend)
skewness(R.D.Spend)
kurtosis(R.D.Spend)

hist(Administration)
boxplot(Administration)
skewness(Administration)
kurtosis(Administration)

hist(Marketing.Spend)
boxplot(Marketing.Spend)
skewness(Marketing.Spend)
kurtosis(Marketing.Spend)

hist(Profit)
boxplot(Profit)
skewness(Profit)
kurtosis(Profit)

startups_data_woStates<-startups_data[-4]
pairs.panels(startups_data_woStates, col="red")
cor(startups_data_woStates)
cor2pcor(cor(startups_data_woStates))

new_data<-data.frame(
                      cbind(
                              sqrt(R.D.Spend),
                              (Marketing.Spend)^2,
                              log(Profit)
                            )
                    )
names(new_data)<-c("R.D.Spend","Marketing.Spend","Profit")
new_data<-new_data[-c(46,47,49,50),]
pairs.panels(new_data, col="red")

model_new_data<-lm(new_data$Profit~.,new_data)
summary(model_new_data)
RMSE_model_new_data<-sqrt(mean(model_new_data$residuals^2))
RMSE_model_new_data
hist(residuals(model_new_data))
boxplot(model_new_data$residuals, horizontal = T)$out
vif(model_new_data)
avPlots(model_new_data)
qqPlot(model_new_data,if.n=5)
influencePlot(model_new_data)
shapiro.test(residuals(model_new_data))
confint(model_new_data)

model1<-lm(Profit~.,startups_data_woStates)
summary(model1)
hist(residuals(model1))
boxplot(model1$residuals, horizontal = T)$out
vif(model1)
avPlots(model1)
qqPlot(model1,if.n=5)
influencePlot(model1)
shapiro.test(residuals(model1))
confint(model1)
RMSE_Model1<-sqrt(mean(model1$residuals^2))
RMSE_Model1


startups_data_woStatesInfluencers<-startups_data_woStates[-c(47,49,50),]
model2<-lm(startups_data_woStatesInfluencers$Profit~.,startups_data_woStatesInfluencers)
summary(model2)
vif(model2)
avPlots(model2)
influenceIndexPlot(model2)
influencePlot(model2)
plot(model2)
qqPlot(model2)
hist(residuals(model2))
boxplot(model2$residuals, horizontal = T)
shapiro.test(residuals(model2))
RMSE_Model2<-sqrt(mean(model2$residuals^2))
RMSE_Model2

cor(startups_data_woStatesInfluencers$Profit,log(startups_data_woStatesInfluencers$Administration))

model3<-lm(startups_data_woStatesInfluencers$Profit~.,startups_data_woStatesInfluencers[,-2])
summary(model3)
vif(model3)
avPlots(model3)
influenceIndexPlot(model3)
influencePlot(model3)
plot(model3)
qqPlot(model3)
hist(residuals(model3))
boxplot(model3$residuals, horizontal = T)
shapiro.test(residuals(model3))
influencePlot(model3)
plot(startups_data_woStatesInfluencers$Profit,resid(model3),ylab="Residuals", xlab="Profit",main="Startups")
abline(0, 0)
plot(model3$fitted.values,startups_data_woStatesInfluencers$Profite,pch = 5, col = c("red", "blue"))
RMSE_model3<-sqrt(mean(model3$residuals^2))
RMSE_model3

cor(startups_data_woStatesInfluencers$Profit,(startups_data_woStatesInfluencers$Marketing.Spend)^2)
cor(startups_data_woStatesInfluencers$Profit,startups_data_woStatesInfluencers$Administration^-3)

start_data_woStateAdminInflu<-data.frame(cbind(startups_data_woStatesInfluencers$R.D.Spend,startups_data_woStatesInfluencers$Administration^-3,(startups_data_woStatesInfluencers$Marketing.Spend)^2,startups_data_woStatesInfluencers$Profit))
names(start_data_woStateAdminInflu)<-c("R.D.Spend", "Administration","Marketing.Spend","Profit")
model4<-lm(start_data_woStateAdminInflu$Profit~.,start_data_woStateAdminInflu)
summary(model4)
RMSE_model4<-sqrt(mean(model4$residuals^2))
RMSE_model4


#model_new_data is the best model among selected models
