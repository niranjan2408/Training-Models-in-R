calories_consumed<-read.csv(choose.files())
View(calories_consumed)
summary(calories_consumed)
attach(calories_consumed)
library(lattice)
sum(is.na(calories_consumed))
plot(Weight.gained..grams.,Calories.Consumed) #scatter plot, there is a positive strong relation
cor(Weight.gained..grams.,Calories.Consumed) #corelation coef r
library(moments)
skewness(Weight.gained..grams.)
skewness(Calories.Consumed)
kurtosis(Weight.gained..grams.)
kurtosis(Calories.Consumed)
dotplot(Weight.gained..grams.)
dotplot(Calories.Consumed)
boxplot(Weight.gained..grams.,horizontal = T)
boxplot(Calories.Consumed,horizontal = T)
reg<-lm(Weight.gained..grams.~Calories.Consumed)
reg$residuals
reg$coefficients
reg$fitted.values
mean(reg$residuals)
summary(reg)
library(ggplot2)
ggplot(data = calories_consumed, aes(x = Calories.Consumed, y = Weight.gained..grams.)) +   geom_point(color='blue') +  geom_line(color='red',data = calories_consumed, aes(x=Calories.Consumed, y=reg$fitted.values))
sqrt(mean(reg$residuals^2))

# SUMMARY
# Output - Weight.gained..grams, Input - Calories.Consumed
# corelation coef r = 0.946991
# R-squared:  0.8968
# RMSE = 103.3025