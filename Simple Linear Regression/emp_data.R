Emp_Data<-read.csv(choose.files())
View(Emp_Data)
summary(Emp_Data)
attach(Emp_Data)
library(lattice)
sum(is.na(Emp_Data))
library(moments)
skewness(Salary_hike)
skewness(Churn_out_rate)
kurtosis(Salary_hike)
kurtosis(Churn_out_rate)
dotplot(Salary_hike)
dotplot(Churn_out_rate)
boxplot(Salary_hike,horizontal = T)
boxplot(Churn_out_rate,horizontal = T)

plot(Salary_hike,Churn_out_rate) #scatter plot, there is a negative strong relation
reg<-lm(Salary_hike~Churn_out_rate)
summary(reg)
cor(Salary_hike,Churn_out_rate)
sqrt(mean(reg$residuals^2))
SALARY_HIKE<-predict(reg)

#SUMMARY
#Output - Salary Hike, Input - Churn_out_rate
# Y~X, r=-0.9117216,r^2=0.8312, rmse=35.89264

plot(Salary_hike,log(Churn_out_rate))
reg_logx<-lm(Salary_hike~log(Churn_out_rate))
summary(reg_logx)
cor(Salary_hike,log(Churn_out_rate))
sqrt(mean(reg_logx$residuals^2))
SALARY_HIKE_Logx<-predict(reg_logx)

#SUMMARY
#Output - Salary Hike, Input - Churn_out_rate
# Y~logX, r=-0.9346361,r^2=0.8735, rmse=31.06952

plot(Salary_hike,log(log(Churn_out_rate)))
reg_loglogx<-lm(Salary_hike~log(log(Churn_out_rate)))
summary(reg_loglogx)
cor(Salary_hike,log(log(Churn_out_rate)))
sqrt(mean(reg_loglogx$residuals^2))
SALARY_HIKE_LogLogx<-predict(reg_loglogx)

#SUMMARY
#Output - Salary Hike, Input - Churn_out_rate
# Y~loglogX, r=-0.9395025,r^2=0.8827, rmse=29.92815

plot(Salary_hike,log(log(log(Churn_out_rate))))
reg_logloglogx<-lm(Salary_hike~log(log(log(Churn_out_rate))))
summary(reg_logloglogx)
cor(Salary_hike,log(log(log(Churn_out_rate))))
sqrt(mean(reg_logloglogx$residuals^2))
SALARY_HIKE_LogLogLogx<-predict(reg_logloglogx)

#SUMMARY
#Output - Salary Hike, Input - Churn_out_rate
# Y~logloglogX, r=-0.9427371,r^2=0.8888, rmse=29.14134

plot(Salary_hike,log(log(log(log(Churn_out_rate)))))
reg_loglogloglogx<-lm(Salary_hike~log(log(log(log(Churn_out_rate)))))
summary(reg_loglogloglogx)
cor(Salary_hike,log(log(log(log(Churn_out_rate)))))
sqrt(mean(reg_loglogloglogx$residuals^2))
SALARY_HIKE_LogLogLogLogx<-predict(reg_loglogloglogx)

#SUMMARY
#Output - Salary Hike, Input - Churn_out_rate
# Y~loglogloglogX, r=-0.9509379,r^2=0.9043, rmse=27.03091
# All the parameters were improved as compared to previous model. However interceptor significance has become P(0.252).
# Therefore previous model // reg_logloglogx<-lm(Salary_hike~log(log(log(Churn_out_rate)))) // will be selected
