#install.packages("kernlab",dependencies = T)

library(kernlab) # for ksvm function
library(psych) #for describe function
library(caret) # for confusion matrix

forestfires <- read.csv(choose.files())
View(forestfires)
str(forestfires)
summary(forestfires)
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

model1 <- ksvm(size_category ~., data=train_data, kernel= "vanilladot")
pred_vanilla_train <- predict(model1,train_data[,-29])
mean(train_data$size_category==pred_vanilla_train) #tain = 0.7622739
pred_vanilla_test <- predict(model1,test_data[,-29])
mean(test_data$size_category==pred_vanilla_test) #test = 0.7076923


model2 <- ksvm(size_category ~., data=train_data, kernel= "rbfdot")
pred_rbf_train <- predict(model2,train_data[,-29])
mean(train_data$size_category==pred_rbf_train) #tain = 0.7648579
pred_rbf_test <- predict(model2,test_data[,-29])
mean(test_data$size_category==pred_rbf_test) #test = 0.7

model3 <- ksvm(size_category ~., data=train_data, kernel= "polydot")
pred_poly_train <- predict(model3,train_data[,-29])
mean(train_data$size_category==pred_poly_train) #tain = 0.7622739
pred_poly_test <- predict(model3,test_data[,-29])
mean(test_data$size_category==pred_poly_test) #test = 0.7076923

model4 <- ksvm(size_category ~., data=train_data, kernel= "tanhdot")
pred_tanh_train <- predict(model4,train_data[,-29])
mean(train_data$size_category==pred_tanh_train) #tain = 0.7390181
pred_tanh_test <- predict(model4,test_data[,-29])
mean(test_data$size_category==pred_tanh_test) #test = 0.7

model5 <- ksvm(size_category ~., data=train_data, kernel= "laplacedot")
pred_laplace_train <- predict(model5,train_data[,-29])
mean(train_data$size_category==pred_laplace_train) #tain = 0.7622739
pred_laplace_test <- predict(model5,test_data[,-29])
mean(test_data$size_category==pred_laplace_test) #test = 0.7

model6 <- ksvm(size_category ~., data=train_data, kernel= "besseldot")
pred_bessel_train <- predict(model6,train_data[,-29])
mean(train_data$size_category==pred_bessel_train) #tain = 0.7596899
pred_bessel_test <- predict(model6,test_data[,-29])
mean(test_data$size_category==pred_bessel_test) #test = 0.7

model7 <- ksvm(size_category ~., data=train_data, kernel= "anovadot")
pred_anova_train <- predict(model7,train_data[,-29])
mean(train_data$size_category==pred_anova_train) #tain = 0.7416021
pred_anova_test <- predict(model7,test_data[,-29])
mean(test_data$size_category==pred_anova_test) #test = 0.7

model8 <- ksvm(size_category ~., data=train_data, kernel= "splinedot")
pred_spline_train <- predict(model8,train_data[,-29])
mean(train_data$size_category==pred_spline_train) #tain = 0.7881137
pred_spline_test <- predict(model8,test_data[,-29])
mean(test_data$size_category==pred_spline_test) #test = 0.5692308


