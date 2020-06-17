#PREDICT THE BURNED AREA OF FOREST FIRES WITH NEURAL NETWORKS

forestfires <- read.csv(choose.files())
View(forestfires)
str(forestfires)
summary(forestfires)
library(psych)
describe(forestfires)


forestfires1 <- forestfires[,-c(1,2)]
str(forestfires1)

normalize<-function(x){return ( (x-min(x))/(max(x)-min(x)))}

#normalizing the entire data
forestfires1_norm<-as.data.frame(lapply(forestfires1[1:28],FUN=normalize))

forestfires1_norm <- cbind(forestfires1_norm,forestfires1[29])

summary(forestfires1_norm)

table(forestfires$size_category)

train_data <- forestfires1_norm[1:387,]
test_data <- forestfires1_norm[388:517,]


# spliting data in to train and test data
#library(caTools)

#split<- sample.split(forestfires1_norm$size_category,SplitRatio = 0.75) 
#train_data<-data.frame(subset(forestfires1_norm,split==TRUE))
#test_data<-data.frame(subset(forestfires1_norm,split==FALSE))

prop.table(table(train_data$size_category))
prop.table(table(test_data$size_category))

#install.packages("nnet", dependencies = TRUE)
library(nnet)  

# Building model
forestfires_model1 <- nnet(size_category~.,data=train_data,size=2)

#train accuracy
forestfires_model1_train_results <- predict(forestfires_model1, train_data[-29], type = "class")
table1_train <- table(forestfires_model1_train_results,train_data$size_category) 
accuracy_train_1 <- (sum(diag(table1_train))/sum(table1_train))
accuracy_train_1 # accuracy = 0.8273196

#test accuracy
forestfires_model1_test_results <- predict(forestfires_model1, test_data[-29], type = "class")
table1_test <- table(forestfires_model1_test_results,test_data$size_category) 
accuracy_test_1 <- (sum(diag(table1_test))/sum(table1_test))
accuracy_test_1 # accuracy = 0.744186


# Building mode2
forestfires_model2 <- nnet(size_category~.,data=train_data,size=2,rang =1)

#train accuracy
forestfires_model2_train_results <- predict(forestfires_model2, train_data[-29], type = "class")
table2_train <- table(forestfires_model2_train_results,train_data$size_category) 
accuracy_train_2 <- (sum(diag(table2_train))/sum(table2_train))
accuracy_train_2 # accuracy = 0.9613402

#test accuracy
forestfires_model2_test_results <- predict(forestfires_model2, test_data[-29], type = "class")
table2_test <- table(forestfires_model2_test_results,test_data$size_category) 
accuracy_test_2 <- (sum(diag(table2_test))/sum(table2_test))
accuracy_test_2 # accuracy = 0.8837209





# Building mode3
forestfires_model3 <- nnet(size_category~.,data=train_data,size=2,rang =1,maxit = 200)

#train accuracy
forestfires_model3_train_results <- predict(forestfires_model3, train_data[-29], type = "class")
table3_train <- table(forestfires_model3_train_results,train_data$size_category) 
accuracy_train_3 <- (sum(diag(table3_train))/sum(table3_train))
accuracy_train_3 # accuracy = 0.9329897

#test accuracy
forestfires_model3_test_results <- predict(forestfires_model3, test_data[-29], type = "class")
table3_test <- table(forestfires_model3_test_results,test_data$size_category) 
accuracy_test_3 <- (sum(diag(table3_test))/sum(table3_test))
accuracy_test_3 # accuracy = 0.8837209




# Building mode4
forestfires_model4 <- nnet(size_category~.,data=train_data,size=2,rang =1,decay = 5e-4, maxit = 200)

#train accuracy
forestfires_model4_train_results <- predict(forestfires_model4, train_data[-29], type = "class")
table4_train <- table(forestfires_model4_train_results,train_data$size_category) 
accuracy_train_4 <- (sum(diag(table4_train))/sum(table4_train))
accuracy_train_4 # accuracy = 1

#test accuracy
forestfires_model4_test_results <- predict(forestfires_model4, test_data[-29], type = "class")
table4_test <- table(forestfires_model4_test_results,test_data$size_category) 
accuracy_test_4 <- (sum(diag(table4_test))/sum(table4_test))
accuracy_test_4 # accuracy = 0.9457364



accuracy_train_1 #1
accuracy_test_1 #0.8307692

accuracy_train_2 #0.997416
accuracy_test_2 #0.9

accuracy_train_3 #0.997416
accuracy_test_3 #0.8846154

accuracy_train_4 #1
accuracy_test_4 #0.9384615

#model 1 and 4 are overfitting models
#model 2 and 3 are good models to use

model_train_predictions <-as.data.frame(cbind(train_data[29],
                              forestfires_model1_train_results,
                              forestfires_model2_train_results,
                              forestfires_model3_train_results,
                              forestfires_model4_train_results))
summary(model_train_predictions)


model_test_predictions <-as.data.frame(cbind(test_data[29],
                                              forestfires_model1_test_results,
                                              forestfires_model2_test_results,
                                              forestfires_model3_test_results,
                                              forestfires_model4_test_results))
summary(model_test_predictions)
