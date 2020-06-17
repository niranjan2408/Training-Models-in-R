#Prepare a classification model using Naive Bayes for salary data 

#Data Description:
  
#age -- age of a person
#workclass	-- A work class is a grouping of work 
#education	-- Education of an individuals	
#maritalstatus -- Marital status of an individulas	
#occupation	 -- occupation of an individuals
#relationship -- 	Husband, Not-in-family, Other-relative, Own-child, Unmarried,Wife
#race --  Race of an Individual
#sex --  Gender of an Individual
#capitalgain --  profit received from the sale of an investment	
#capitalloss	-- A decrease in the value of a capital asset
#hoursperweek -- number of hours work per week	
#native -- Native of an individual
#Salary -- salary of an individual


library("e1071")
library("psych")
library("ggplot2")
library("dplyr")

salary_raw_train <- read.csv(choose.files())
salary_raw_test <- read.csv(choose.files())

sum(is.na(salary_raw_train))
sum(is.na(salary_raw_test))


str(salary_raw_train)
head(salary_raw_train)
summary(salary_raw_train)
describe(salary_raw_train)

pairs.panels(salary_raw_train[,c(1,4,10,11,12)])
#there is no colinearity problem
pairs.panels(salary_raw_train)

boxplot(salary_raw_train$age)
boxplot(salary_raw_train$educationno)
boxplot(salary_raw_train$capitalgain)
boxplot(salary_raw_train$capitalloss)
boxplot(salary_raw_train$hoursperweek)


salary_raw_train %>% ggplot(aes(x=Salary,y=age,fill=Salary))+geom_boxplot()+ggtitle("Box Plot")
#there is significant overlap between " <=50K"," >50K"  w.r.t. age
salary_raw_train %>% ggplot(aes(x=Salary,y=educationno,fill=Salary))+geom_boxplot()+ggtitle("Box Plot")
#as educationno for " <=50K"," >50K" are not overlapping this could be good predictor for classification
salary_raw_train %>% ggplot(aes(x=Salary,y=capitalgain,fill=Salary))+geom_boxplot()+ggtitle("Box Plot")
#not of use
salary_raw_train %>% ggplot(aes(x=Salary,y=capitalloss,fill=Salary))+geom_boxplot()+ggtitle("Box Plot")
#not of use
salary_raw_train %>% ggplot(aes(x=Salary,y=hoursperweek,fill=Salary))+geom_boxplot()+ggtitle("Box Plot")
#as hoursperweek for " <=50K"," >50K" are not overlapping this could be good predictor for classification


# %>% is a pipe function. It is used to avoid mentioning data frame name before each variable in the following function
#Below graphs are used to analyse continueous variables
salary_raw_train %>% ggplot(aes(x=age,fill=Salary))+geom_density(alpha=0.8, color='black')+ggtitle("Box Plot")
salary_raw_train %>% ggplot(aes(x=educationno,fill=Salary))+geom_density(alpha=0.8, color='black')+ggtitle("Box Plot")
salary_raw_train %>% ggplot(aes(x=capitalgain,fill=Salary))+geom_density(alpha=0.8, color='black')+ggtitle("Box Plot")
salary_raw_train %>% ggplot(aes(x=capitalloss,fill=Salary))+geom_density(alpha=0.8, color='black')+ggtitle("Box Plot")
salary_raw_train %>% ggplot(aes(x=hoursperweek,fill=Salary))+geom_density(alpha=0.8, color='black')+ggtitle("Box Plot")

#Below graphs are used to analyse categorical variables
plot(salary_raw_train$education,salary_raw_train$Salary)
plot(salary_raw_train$maritalstatus,salary_raw_train$Salary)
plot(salary_raw_train$occupation,salary_raw_train$Salary)
plot(salary_raw_train$relationship,salary_raw_train$Salary)
plot(salary_raw_train$race,salary_raw_train$Salary)
plot(salary_raw_train$sex,salary_raw_train$Salary)
plot(salary_raw_train$native,salary_raw_train$Salary)


# Building naiveBayes model to predict salary class
Model1 <- naiveBayes(Salary ~ ., data = salary_raw_train)
Model1

# Predicting salary class for train data and savinf it in Model1_pred_train
Model1_pred_train <- predict(Model1,salary_raw_train)
#Checking the accuracy of the model
mean(Model1_pred_train==salary_raw_train$Salary) # model is giving 82.25% accuracy
# Building confusion matrix to evaluate alpha and beta errors
table(Model1_pred_train,salary_raw_train$Salary)


# Running model1 on test data and saving results in Model1_pred_test
Model1_pred_test <- predict(Model1,salary_raw_test)
#Checking the accuracy of the model on test data
mean(Model1_pred_test==salary_raw_test$Salary) # model is giving 81.93% accuracy
# Building confusion matrix to evaluate alpha and beta errors
table(Model1_pred_test,salary_raw_test$Salary)


