#Classify whether application accepted or not using Logistic regression
#card
#Factor. Was the application for a credit card accepted?
#reports - Number of major derogatory reports.
#age - Age in years plus twelfths of a year.
#income - Yearly income (in USD 10,000).
#share - Ratio of monthly credit card expenditure to yearly income.
#expenditure - Average monthly credit card expenditure.
#owner - Factor. Does the individual own their home?
#selfemp - Factor. Is the individual self-employed?
#dependents - Number of dependents.
#months - Months living at current address.
#majorcards - Number of major credit cards held.
#active - Number of active credit accounts.

library(caTools)
library(corpcor)
library(car)
library(psych)
library(ROCR)
library(dplyr)
library(doParallel)
library(pROC)
registerDoParallel(core=4)
par(mfrow=c(1,2))

OrgData_CrdCard <-read.csv(choose.files())
OrgData_CrdCard <-OrgData_CrdCard[-1]
str(OrgData_CrdCard)
summary(OrgData_CrdCard)
sum(is.na(OrgData_CrdCard))

boxplot(OrgData_CrdCard$age)
boxplot(OrgData_CrdCard$income)
boxplot(OrgData_CrdCard$expenditure)
boxplot(OrgData_CrdCard$months)

split<- sample.split(OrgData_CrdCard$active,SplitRatio = 0.7)
train_data<-subset(OrgData_CrdCard,split==TRUE)
test_data<-subset(OrgData_CrdCard,split==FALSE)

pairs.panels(test_data, col="red")


model<-glm(train_data$card~.,train_data,family = "binomial")
summary(model)

#Null deviance:  967.431 is higher than Residual deviance: 86.443 

#Created a data frame with removing non influencing factors and general knowledge


train_datal<-train_data[,c(1,2,4,5,9,12)]
#checked colinearity of selected variables
pairs.panels(train_datal, col="red")
#cor2pcor(cor(train_datal))

model1<-glm(train_datal$card~.,train_datal,family = "binomial")
summary(model1)


#model1 summary - all variables are significant. Null deviation is higher than Residual deviation.
# model1 is good to use

#created variable prob and saved model3 predicted probability values for card
prob1 <- predict(model1,train_datal,type="response")

#created confusion matrix of predicted card probability values with cut off 0.5 and original card data
confusion1<-table(prob1>0.5,train_datal$card)
confusion1
#false negative (alpha) = 21

Accuracy1<-sum(diag(confusion1)/sum(confusion1))
Accuracy1
#Accuracy of model3 is 98.1%


#created variable (pred_values) with if predicted p value is greater than 0.5 put "yes" else "no"
pred_values1 <- ifelse(prob1>=0.5,"yes","no")

#Added column of (pred_values) to the original data set
train_datal[,"pred_values1"] <- pred_values1
View(train_datal[,c(1,7)]) #Just to compare actual data and predicted data

##prop.table(OrgData_CrdCard$card) //cant use as data in these columns is categorical not binary numerical
##prop.table(OrgData_CrdCard$pred_values)


#Receiver Operating Characteristic (ROC) Curve Analysis
ROCR_Prediction1<-prediction(prob1,train_datal$card)
ROCR_Performance1<-performance(ROCR_Prediction1,'tpr','fpr')

plot(ROCR_Performance1,colorize=T,text.adj=c(-0.2,1.7),print.cutoffs.at=seq(0.1,by=0.1))

str(ROCR_Performance1)
ROCR_cutoff1 <- data.frame(cut_off = ROCR_Performance1@alpha.values[[1]],fpr=ROCR_Performance1@x.values,tpr=ROCR_Performance1@y.values)
colnames(ROCR_cutoff1) <- c("cut_off","FPR","TPR")
ROCR_cutoff1<-round(ROCR_cutoff1,3)
View(ROCR_cutoff1)


confusion2<-table(prob1>0.2,train_datal$card)
confusion2
Accuracy<-sum(diag(confusion2)/sum(confusion2))
Accuracy

updated_pred_values <- ifelse(prob1>=0.2,"yes","no")
train_datal[,"updated_pred_values"] <- updated_pred_values
