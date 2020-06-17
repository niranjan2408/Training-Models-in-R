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

#creating dummy variables
library(dummies)
RF1 <- dummy.data.frame(RF, sep = ".")

#normalizing the data
normalize <- function(x){return((x-min(x))/(max(x)-min(x)))}

RF1_norm <- as.data.frame(lapply(RF1,FUN = normalize))


#including new variable column to normalized data set and saving it as RF2
RF2 <- data.frame(RF1_norm,Risky_Good)
summary(RF2)

#using sequencial sampling technique
RF2_train <- RF2[1:450,]
RF2_test <- RF2[451:600,]

#checking proportion of"Risky","Good" in train and test samples
prop.table(table(RF2_train$Risky_Good))
prop.table(table(RF2_test$Risky_Good))

#install.packages("randomForest",dependencies = TRUE)
library(randomForest)

# Building a random forest model on training data 
model1 <- randomForest(RF2_train$Risky_Good~.,data=RF2_train, na.action=na.roughfix,importance=TRUE)
# Training accuracy 
mean(RF2_train$Risky_Good==predict(model1,RF2_train)) # 100% accuracy 

# Prediction of train data
pred_train <- predict(model1,RF2_train)


library(caret)

# Confusion Matrix
confusionMatrix(RF2_train$Risky_Good, pred_train)


# Predicting test data 
pred_test <- predict(model1,RF2_test)
mean(pred_test==RF2_test$Risky_Good) # Accuracy = 100% 

# Confusion Matrix
confusionMatrix(RF2_test$Risky_Good, pred_test)


# Visualization 
plot(model1,lwd=2)
legend("topright", colnames(model1$err.rate),col=1:4,cex=0.8,fill=1:4)
