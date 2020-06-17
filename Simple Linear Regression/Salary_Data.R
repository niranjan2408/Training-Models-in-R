Salary_Data<-read.csv(choose.files())
View(Salary_Data)
summary(Salary_Data)
attach(Salary_Data)
library(lattice)
sum(is.na(Salary_Data))
library(moments)
skewness(YearsExperience)
skewness(Salary)
kurtosis(YearsExperience)
kurtosis(Salary)
dotplot(YearsExperience)
dotplot(Salary)
boxplot(YearsExperience,horizontal = T)
boxplot(Salary,horizontal = T)

plot(Salary,YearsExperience) #scatter plot, there is a positive strong relation
reg<-lm(Salary~YearsExperience)
summary(reg)
cor(Salary,YearsExperience)
sqrt(mean(reg$residuals^2))
SALARY<-predict(reg)

#SUMMARY
#Output - Salary, Input - YearsExperience
# Y~X, r=-0.9782416,r^2=0.957, rmse=5592.044