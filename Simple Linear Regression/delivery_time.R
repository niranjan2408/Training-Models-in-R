DT<-read.csv(choose.files())
View(DT)
summary(DT)
attach(DT)
library(lattice)
sum(is.na(DT))
 #scatter plot, there is a positive strong relation
library(moments)
skewness(Delivery.Time)
skewness(Sorting.Time)
kurtosis(Delivery.Time)
kurtosis(Sorting.Time)
dotplot(Delivery.Time)
dotplot(Sorting.Time)
boxplot(Delivery.Time,horizontal = T)
boxplot(Sorting.Time,horizontal = T)


plot(Delivery.Time,Sorting.Time)
reg<-lm(Delivery.Time~Sorting.Time)
summary(reg)
cor(Delivery.Time,Sorting.Time)
sqrt(mean(reg$residuals^2))
STime<-predict(reg)

plot(Delivery.Time,log(Sorting.Time))
reg_logX<-lm(Delivery.Time~log(Sorting.Time))
summary(reg_logX)
cor(Delivery.Time,log(Sorting.Time))
sqrt(mean(reg_logX$residuals^2))
STime_logx<-predict(reg_logX)

plot(log(Delivery.Time),log(Sorting.Time))
reg_logXy<-lm(log(Delivery.Time)~log(Sorting.Time))
summary(reg_logXy)
cor(log(Delivery.Time),log(Sorting.Time))
sqrt(mean(reg_logXy$residuals^2))
STime_logxy<-predict(reg_logXy)
STime_y<-exp(STime_logxy)

#SUMMARY
#Output - Delivery.Time, Input - Sorting.Time
# Y~X, r=0.8259973,r^2=0.6823, rmse=2.79165
# Y~logX, r=0.8339325,r^=0.6954, rmse=2.733171
# logY~logX, r=0.8787271,r^=0.7722, rmse=0.1482331

                          
