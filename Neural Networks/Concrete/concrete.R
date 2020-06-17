# Prepare a model for strength of concrete data using Neural Networks


concrete <- read.csv(file.choose())
str(concrete)
summary(concrete)
library(psych)
describe(concrete)


normalize<-function(x){return ( (x-min(x))/(max(x)-min(x)))}

#normalizing the entire data
concrete_norm<-as.data.frame(lapply(concrete,FUN=normalize))

summary(concrete_norm)

#creating train and test data sets
concrete_train<-concrete_norm[1:775,]
concrete_test<-concrete_norm[776:1030,]


library(neuralnet)  

# Building model
concrete_model <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data = concrete_train)

str(concrete_model)

plot(concrete_model)

model_results <- compute(concrete_model,concrete_test)

str(model_results)

cor(model_results$net.result,concrete_test$strength) # accuracy = 0.806467

plot(model_results$net.result,concrete_test$strength)


# Building mode2 with 3 nodes

concrete_mode2 <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data = concrete_train,hidden = 3)

plot(concrete_mode2)


#testing train predictions
concrete_mode2_train_results<-compute(concrete_mode2,concrete_train)

cor(concrete_mode2_train_results$net.result,concrete_train$strength) #accuracy = 0.9251554

plot(concrete_mode2_train_results$net.result,concrete_train$strength)


#testing test predictions
concrete_mode2_results<-compute(concrete_mode2,concrete_test)

cor(concrete_mode2_results$net.result,concrete_test$strength) #accuracy = 0.9151997

plot(concrete_mode2_results$net.result,concrete_test$strength)

#train and testing acuracy difference is minimal and bothe the accuracies are above 90%, we can use this concrete_mode2 as a final model
#further accuracy can be increased by increasing hidden = nodes combination