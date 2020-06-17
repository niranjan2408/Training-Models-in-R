#Use decision trees to prepare a model on fraud data 
# treating those who have taxable_income <= 30000 as "Risky" and others are "Good"
# Data Description :
#  Undergrad : person is under graduated or not
#  Marital.Status : marital status of a person
#  Taxable.Income : Taxable income is the amount of how much tax an individual owes to the government 
#  City Polulation :
#  Work Experience : Work experience of an individual person
#  Urban : Whether that person belongs to urban area or not

#install.packages("tree")
#install.packages("C50")
#install.packages("caret")


library(C50)
library(psych)

FC <- read.csv(choose.files())
str(FC)
describe(FC)
summary(FC)

#converitng Taxable.Income variable into "Risky","Good" categories
FC$Taxable.Income <- ifelse(FC$Taxable.Income<= 30000,"Risky","Good")
#changing FC$Taxable.Income format to factor with 2 levels
FC$Taxable.Income <- as.factor(FC$Taxable.Income)


#using sequencial sampling technique
FC_train <- FC[1:450,]
FC_test <- FC[451:600,]

#checking proportion of"Risky","Good" in train and test samples
prop.table(table(FC_train$Taxable.Income))
prop.table(table(FC_test$Taxable.Income))

#building model
DT_FC_train <- C5.0(FC_train,FC_train$Taxable.Income)
plot(DT_FC_train)      



#checking accuracy with train data
pred_train <- predict(DT_FC_train,FC_train)
table(pred_train,FC_train$Taxable.Income)
mean(pred_train==FC_train$Taxable.Income) #accuracy 1

#checking accuracy with test data
pred_test <- predict(DT_FC_train,FC_test)
table(pred_test,FC_test$Taxable.Income)
mean(pred_test==FC_test$Taxable.Income) # accuracy 1




#using random splitting technique

library("caTools")
split<- sample.split(FC$Taxable.Income,SplitRatio = 0.75) # spliting data in to train and test data
train_data<-data.frame(subset(FC,split==TRUE))
test_data<-data.frame(subset(FC,split==FALSE))

prop.table(table(train_data$Taxable.Income))
prop.table(table(test_data$Taxable.Income))
str(train_data)

DT_train_data <- C5.0(train_data,train_data$Taxable.Income)
plot(DT_train_data)      

pred_train_data <- predict(DT_train_data,train_data)
table(pred_train_data,train_data$Taxable.Income)
mean(pred_train_data==train_data$Taxable.Income) # accuracy 1

pred_test <- predict(DT_train_data,test_data)
table(pred_test,test_data$Taxable.Income)
mean(pred_test==test_data$Taxable.Income) # accuracy 1

