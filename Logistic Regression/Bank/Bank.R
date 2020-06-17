#Input variables:

# bank client data:

#  1 - age (numeric)
#2 - job : type of job (categorical: "admin.","unknown","unemployed","management","housemaid","entrepreneur","student","blue-collar","self-employed","retired","technician","services") 
#3 - marital : marital status (categorical: "married","divorced","single"; note: "divorced" means divorced or widowed)
#4 - education (categorical: "unknown","secondary","primary","tertiary")
#5 - default: has credit in default? (binary: "yes","no")
#6 - balance: average yearly balance, in euros (numeric) 
#7 - housing: has housing loan? (binary: "yes","no")
#8 - loan: has personal loan? (binary: "yes","no")

#     related with the last contact of the current campaign:

#9 - contact: contact communication type (categorical: "unknown","telephone","cellular") 
#10 - day: last contact day of the month (numeric)
#11 - month: last contact month of year (categorical: "jan", "feb", "mar", ..., "nov", "dec")
#12 - duration: last contact duration, in seconds (numeric)

# other attributes:

#13 - campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
#14 - pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric, -1 means client was not previously contacted)
#15 - previous: number of contacts performed before this campaign and for this client (numeric)
#16 - poutcome: outcome of the previous marketing campaign (categorical: "unknown","other","failure","success")

#Output variable (desired target):
#  17 - y - has the client subscribed a term deposit? (binary: "yes","no")


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

org_bank<-read.csv(choose.files())
str(org_bank)
summary(org_bank)
sum(is.na(org_bank))

hist(org_bank$age)
boxplot(org_bank$age)

hist(org_bank$balance)
boxplot(org_bank$balance)

hist(org_bank$day)
boxplot(org_bank$day)

hist(org_bank$duration)
boxplot(org_bank$duration)

hist(org_bank$campaign)
boxplot(org_bank$campaign)

hist(org_bank$pdays)
boxplot(org_bank$pdays)

hist(org_bank$previous)
boxplot(org_bank$previous)

#Splitting the data into train and test with 0.7 split ratio

split<- sample.split(org_bank$y,SplitRatio = 0.7)
train_data<-subset(org_bank,split==TRUE)
test_data<-subset(org_bank,split==FALSE)

#pairs.panels(train_data, col="red")

#checking the colinearity between continueous variables by creating new data without categorical variables
td_wo_cat<-data.frame(cbind(train_data$age,train_data$balance,train_data$day,train_data$duration,train_data$campaign,train_data$pdays,train_data$previous))
names(td_wo_cat)<-c("age","balance","day","duration","campaign","pdays","previous")
pairs.panels(td_wo_cat, col="red")

#model1 with all variables
model<-glm(train_data$y~.,train_data,family = "binomial")
summary(model)

#insignificant variables age, pdays, previous 
#removing insignificant variables from the train data and creating new data set train_data_1

train_data_1<-train_data[,-c(1,5,15,14)]

model1<-glm(train_data_1$y~.,train_data_1,family = "binomial")
summary(model1)

#model1 summary - all variables are significant. Null deviation is higher than Residual deviation.
# model1 is good to use

prob1 <- predict(model1,train_data_1,type="response")

confusion1<-table(prob1>0.5,train_data_1$y)
confusion1
#false negative (alpha) = 2390

Accuracy1<-sum(diag(confusion1)/sum(confusion1))
Accuracy1
#Accuracy of model3 is 90.3%

#created variable (pred_values) with if predicted p value is greater than 0.5 put "yes" else "no"
pred_values1 <- ifelse(prob1>=0.5,"yes","no")

#Added column of (pred_values) to the original train data set 1 
train_data_1[,"pred_values1"] <- pred_values1
View(train_data_1[,c(14,15)]) #Just to compare actual data and predicted data

#Receiver Operating Characteristic (ROC) Curve Analysis
ROCR_Prediction1<-prediction(prob1,train_data_1$y)
ROCR_Performance1<-performance(ROCR_Prediction1,'tpr','fpr')
plot(ROCR_Performance1,colorize=T,text.adj=c(-0.2,1.7),print.cutoffs.at=seq(0.01,by=0.01))

str(ROCR_Performance1)
ROCR_cutoff1 <- data.frame(cut_off = ROCR_Performance1@alpha.values[[1]],fpr=ROCR_Performance1@x.values,tpr=ROCR_Performance1@y.values)
colnames(ROCR_cutoff1) <- c("cut_off","FPR","TPR")
ROCR_cutoff1<-round(ROCR_cutoff1,3)
View(ROCR_cutoff1)


confusion2<-table(prob1>0.39,train_data_1$y)
confusion2
Accuracy2<-sum(diag(confusion2)/sum(confusion2))
Accuracy2

#as per below calculation proportion of majority class is 88.3%
table(train_data_1$y)
27945/31647

updated_pred_values <- ifelse(prob1>=0.39,"yes","no")
train_data_1[,"updated_pred_values"] <- updated_pred_values

auc<-performance(ROCR_Prediction1,measure="auc")
str(auc)
auc<-auc@y.values[[1]]
auc

## Test data

test_data_1<-test_data[,-c(1,5,15,14)]

test_prob1 <- predict(model1,test_data_1,type="response")

confusion1_test<-table(test_prob1>0.39,test_data_1$y)
confusion1_test

Accuracy1_test<-sum(diag(confusion1_test)/sum(confusion1_test))
Accuracy1_test
