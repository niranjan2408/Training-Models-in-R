#1) Prepare a classification model using SVM for salary data 

#Data Description:
  
#age -- age of a person
#workclass	-- A work class is a grouping of work 
#education	-- Education of an individuals	
#maritalstatus -- Marital status of an individulas	
#occupation	 -- occupation of an individuals
#relationship -- 	
#race --  Race of an Individual
#sex --  Gender of an Individual
#capitalgain --  profit received from the sale of an investment	
#capitalloss	-- A decrease in the value of a capital asset
#hoursperweek -- number of hours work per week	
#native -- Native of an individual
#Salary -- salary of an individual

library(kernlab) # for ksvm function
library(psych) #for describe function
library(caret) # for confusion matrix
library(ggplot2) #for ggplot

SD_train_org <- read.csv(choose.files())
SD_test_org <- read.csv(choose.files())

sum(is.na(SD_train_org))
sum(is.na(SD_test_org))

str(SD_train_org)
summary(SD_train_org)
describe(SD_train_org)

hist(SD_train_org$capitalgain)
hist(SD_train_org$capitalloss)
boxplot(SD_train_org$capitalgain)

#pairs.panels(SD_train_org[c(10,11,14)])
table(SD_train_org$Salary)

train_data <- SD_train_org[,-c(3,10,11)] # removing capitalgain and capitalloss as it is not contributing to analysis
test_data <- SD_test_org[,-c(3,10,11)]

ggplot(data=train_data, aes(x=Salary, y=age, fill=Salary))+geom_boxplot()+ggtitle("Box Plot")
ggplot(data=train_data, aes(x = age,fill= Salary))+geom_density(alpha = 0.9, color = 'Violet')

ggplot(data=train_data, aes(x=Salary, y=educationno, fill=Salary))+geom_boxplot()+ggtitle("Box Plot")
ggplot(data=train_data,aes(x = educationno,fill= Salary))+geom_density(alpha = 0.9, color = 'Violet')

ggplot(data=train_data, aes(x=Salary, y=hoursperweek, fill=Salary))+geom_boxplot()+ggtitle("Box Plot")
ggplot(data=train_data,aes(x = hoursperweek,fill= Salary))+geom_density(alpha = 0.9, color = 'Violet')

plot(train_data$workclass,train_data$Salary)
plot(train_data$maritalstatus,train_data$Salary)
plot(train_data$occupation,train_data$Salary)
plot(train_data$relationship,train_data$Salary)
plot(train_data$race,train_data$Salary)
plot(train_data$sex,train_data$Salary)
plot(train_data$native,train_data$Salary)


library(doParallel) 
registerDoParallel(cores=4) 

model1 <- ksvm(Salary ~., data=train_data, kernel= "vanilladot")
pred_vanilla_train <- predict(model1,train_data[,-11])
mean(train_data$Salary==pred_vanilla_train) #tain = 0.8267962
pred_vanilla_test <- predict(model1,test_data[,-11])
mean(test_data$Salary==pred_vanilla_test) #test = 0.8270252

model2 <- ksvm(Salary ~., data=train_data, kernel= "rbfdot")
pred_rbf_train <- predict(model2,train_data[,-11])
mean(train_data$Salary==pred_rbf_train) #tain = 0.841484
pred_rbf_test <- predict(model2,test_data[,-11])
mean(test_data$Salary==pred_rbf_test) #test = 0.8354582

model3 <- ksvm(Salary ~., data=train_data, kernel= "polydot")
pred_poly_train <- predict(model3,train_data[,-11])
mean(train_data$Salary==pred_poly_train) #tain = 0.8267962
pred_poly_test <- predict(model3,test_data[,-11])
mean(test_data$Salary==pred_poly_test) #test = 0.8270252

model4 <- ksvm(Salary ~., data=train_data, kernel= "tanhdot")
pred_tanh_train <- predict(model4,train_data[,-11])
mean(train_data$Salary==pred_tanh_train) #tain = 0.6522662
pred_tanh_test <- predict(model4,test_data[,-11])
mean(test_data$Salary==pred_tanh_test) #test = 0.650996

model5 <- ksvm(Salary ~., data=train_data, kernel= "laplacedot")
pred_laplace_train <- predict(model5,train_data[,-11])
mean(train_data$Salary==pred_laplace_train) #tain = 0.848082
pred_laplace_test <- predict(model5,test_data[,-11])
mean(test_data$Salary==pred_laplace_test) #test = 0.8346614

model6 <- ksvm(Salary ~., data=train_data, kernel= "besseldot")
pred_bessel_train <- predict(model6,train_data[,-11])
mean(train_data$Salary==pred_bessel_train) #tain = 0.7886343
pred_bessel_test <- predict(model6,test_data[,-11])
mean(test_data$Salary==pred_bessel_test) #test = 0.7817397

#model7 <- ksvm(Salary ~., data=train_data, kernel= "anovadot")
#pred_anova_train <- predict(model7,train_data[,-11])
#mean(train_data$Salary==pred_anova_train) #tain = 
#pred_anova_test <- predict(model7,test_data[,-11])
#mean(test_data$Salary==pred_anova_test) #test = 

#model8 <- ksvm(Salary ~., data=train_data, kernel= "splinedot")
#pred_spline_train <- predict(model8,train_data[,-11])
#mean(train_data$Salary==pred_spline_train) #tain = 
#pred_spline_test <- predict(model8,test_data[,-11])
#mean(test_data$Salary==pred_spline_test) #test = 