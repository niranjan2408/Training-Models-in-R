#Build a decision tree for the 'iris' data with function 'ctree()' in package "party".

library(C50)
library(tree)
library(party)


data("iris")
View(iris)

iris_setosa<-iris[iris$Species=="setosa",] 
iris_versicolor <- iris[iris$Species=="versicolor",] 
iris_virginica <- iris[iris$Species=="virginica",] 
iris_train <- rbind(iris_setosa[1:35,],iris_versicolor[1:35,],iris_virginica[1:35,])
iris_test <- rbind(iris_setosa[36:50,],iris_versicolor[36:50,],iris_virginica[36:50,])

# Building model on training data 
irisc5.0_train <- C5.0(iris_train[,-5],iris_train$Species)
plot(irisc5.0_train) 

# Training accuracy
pred_train <- predict(irisc5.0_train,iris_train)

mean(iris_train$Species==pred_train) # 97.33% Accuracy

# Testing accuracy
predc5.0_test <- predict(irisc5.0_train,newdata=iris_test) # predicting on test data
mean(predc5.0_test==iris_test$Species) # 94.66% accuracy 

# ctree from package "party".

train_ctree <-  ctree(
                      iris_train$Species ~ 
                                            iris_train$Sepal.Length + 
                                            iris_train$Sepal.Width + 
                                            iris_train$Petal.Length + 
                                            iris_train$Petal.Width
                  )
summary(train_ctree)
plot(train_ctree)

test_ctree <-  ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iris_test)
                      
summary(test_ctree)
plot(test_ctree)
