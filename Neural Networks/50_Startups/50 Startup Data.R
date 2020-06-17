# Build a Neural Network model for 50_startups data to predict profit 


SD <- read.csv(file.choose())
str(SD)
summary(SD)
library(psych)
describe(SD)

boxplot(SD$R.D.Spend)
boxplot(SD$Administration)
boxplot(SD$Marketing.Spend)
boxplot(SD$Profit)

normalize<-function(x){return ( (x-min(x))/(max(x)-min(x)))}

#normalizing the entire data
SD_norm<-as.data.frame(lapply(SD[,c(1,2,3,5)],FUN=normalize))

summary(SD_norm)

#creating train and test data sets
library(caTools)
split<- sample.split(SD_norm$Profit,SplitRatio = 0.7) # spliting data in to train and test data
SD_norm_train<-data.frame(subset(SD_norm,split==TRUE))
SD_norm_test<-data.frame(subset(SD_norm,split==FALSE))


library(neuralnet)  

# Building model

SD_model_l <- neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend, data=SD_norm_train)

str(SD_model_l)

plot(SD_model_l)

#train predictions
model_results_train <- compute(SD_model_l,SD_norm_train)

str(model_results_train)

cor(model_results_train$net.result,SD_norm_train$Profit) # accuracy = 0.9734785

plot(model_results_train$net.result,SD_norm_train$Profit)

#test predictions
model_results_test <- compute(SD_model_l,SD_norm_test)

cor(model_results_test$net.result,SD_norm_test$Profit) # accuracy = 0.976215

plot(model_results_test$net.result,SD_norm_test$Profit)


# Building mode2 with 3 nodes

SD_model_2 <- neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend, data=SD_norm_train,hidden = 3)

plot(SD_model_2)


# train predictions
SD_model_2_train_results<-compute(SD_model_2,SD_norm_train)

cor(SD_model_2_train_results$net.result,SD_norm_train$Profit) #accuracy = 0.9756652

plot(SD_model_2_train_results$net.result,SD_norm_train$Profit)


# test predictions
SD_model_2_test_results<-compute(SD_model_2,SD_norm_test)

cor(SD_model_2_test_results$net.result,SD_norm_test$Profit) #accuracy = 0.978726

plot(SD_model_2_test_results$net.result,SD_norm_test$Profit)

#train and testing acuracy difference is minimal and bothe the accuracies are above 95%, we can use this concrete_mode2 as a final model
