# Use Random Forest to prepare a model on fraud data 
# treating those who have taxable_income <= 30000 as "Risky" and others are "Good"

library(psych) #describe(RF)

RF <- read.csv(choose.files())
str(RF)
describe(RF)
summary(RF)

#converitng Taxable.Income variable into "Risky","Good" categories
Risky_Good <- ifelse(RF$Taxable.Income<= 30000,"Risky","Good")
#changing FC$Taxable.Income format to factor with 2 levels
Risky_Good <- as.factor(Risky_Good)


#including new variable column to normalized data set and saving it as RF2
RF1 <- data.frame(RF,Risky_Good)
summary(RF1)

#using sequencial sampling technique
RF1_train <- RF1[1:450,]
RF1_test <- RF1[451:600,]

#checking proportion of"Risky","Good" in train and test samples
prop.table(table(RF1_train$Risky_Good))
prop.table(table(RF1_test$Risky_Good))

#install.packages("randomForest",dependencies = TRUE)
library(randomForest)

# Building a random forest model on training data 
model1 <- randomForest(RF1_train$Risky_Good~.,data=RF1_train, na.action=na.roughfix,importance=TRUE)
# Training accuracy 
mean(RF1_train$Risky_Good==predict(model1,RF1_train)) # 100% accuracy 

# Prediction of train data
pred_train <- predict(model1,RF1_train)


library(caret)

# Confusion Matrix
confusionMatrix(RF1_train$Risky_Good, pred_train)


# Predicting test data 
pred_test <- predict(model1,RF1_test)
mean(pred_test==RF1_test$Risky_Good) # Accuracy = 100% 

# Confusion Matrix
confusionMatrix(RF1_test$Risky_Good, pred_test)


# Visualization 
plot(model1,lwd=2)
legend("topright", colnames(model1$err.rate),col=1:4,cex=0.8,fill=1:4)
