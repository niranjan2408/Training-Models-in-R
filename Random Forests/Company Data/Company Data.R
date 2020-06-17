library(psych)
library(caTools)
library(dummies)
library(randomForest)
library(caret)

CD <- read.csv(choose.files())
str(CD)
summary(CD) #as Q3 of Sales is (3rd Qu.:) 9.320 we can consider above sales above 10 thousands as high sale
hist(CD$Sales)
describe(CD)
sum(is.na(CD))

highsales = ifelse(CD$Sales>10, "Yes", "No")

CD1 <- data.frame(CD[2:11], highsales)
str(CD1)
table(CD1$highsales)
prop.table(table(CD1$highsales))

train_data <- CD1 [1:300,]
test_data <- CD1 [301:400,]

prop.table(table(train_data$highsales))
prop.table(table(test_data$highsales))


model1 <- randomForest(train_data$highsales~.,data=train_data[-16], na.action=na.roughfix,importance=TRUE)
pred_train <- predict(model1,train_data)
table(pred_train,train_data$highsales)
mean(train_data$highsales==predict(model1,train_data)) # 100% accuracy 


pred_test <- predict(model1,test_data)
table(pred_test,test_data$highsales)
mean(test_data$highsales==predict(model1,test_data)) # 84% accuracy 

# Visualization 
plot(model1,lwd=2)
legend("topright", colnames(model1$err.rate),col=1:4,cex=0.8,fill=1:4)

varImpPlot(model1)

# Mean Decrease Accuracy graph shows that how worst the model performs without each variable followed by Price.
# ShelveLoc is the most important variable for prediction
# Whereas population is the least important.

# MeanDecrease gini graph shows how much by average the gini (Gini is defined as "inequity" ) decreases if one of those nodes were removed. 
#Price is very important and Urban is least important variables

varImpPlot(model1 ,Sort = T, n.var = 5, main = "Top 5 -Variable Importance")
#Top 5 impacting the predictions

importance(model1)
# Quantitative values clearly shows that Price and ShelveLoc are the most important factors highsales
