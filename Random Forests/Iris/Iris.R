library(randomForest)
library(caret)
library(psych)

data(iris)
View(iris)
str(iris)
summary(iris) 
describe(iris)
sum(is.na(iris))

# Splitting data into training and testing. As the species are in order 
# splitting the data based on species 

iris_setosa<-iris[iris$Species=="setosa",]
iris_versicolor <- iris[iris$Species=="versicolor",]
iris_virginica <- iris[iris$Species=="virginica",]

iris_train <- rbind(iris_setosa[1:35,],iris_versicolor[1:35,],iris_virginica[1:35,])
iris_test <- rbind(iris_setosa[36:50,],iris_versicolor[36:50,],iris_virginica[36:50,])

# Building a random forest model on training data 
RF <- randomForest(Species~.,data=iris_train, na.action=na.roughfix,importance=TRUE)
# Training accuracy 
mean(iris_train$Species==predict(RF,iris_train)) # 100% accuracy 

# Prediction of train data
pred_train <- predict(RF,iris_train)

# Confusion Matrix
confusionMatrix(iris_train$Species, pred_train)

# Predicting test data 
pred_test <- predict(RF,newdata=iris_test)
mean(pred_test==iris_test$Species) # Accuracy = 100% 

# Confusion Matrix 
confusionMatrix(iris_test$Species, pred_test)

# Visualization 
plot(RF,lwd=2)
legend("topright", colnames(RF$err.rate),col=1:4,cex=0.8,fill=1:4)

varImpPlot(RF)
#Petal.Length and Petal.Width are the most important variables
#Sepal.Length and Sepal.Width are least important variables

importance(RF)
#Petal.Length and Petal.Width are the most important variables
#Sepal.Length and Sepal.Width are least important variables