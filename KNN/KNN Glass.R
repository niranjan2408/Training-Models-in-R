#Prepare a model for glass classification using KNN

#Data Description:
  
#  RI : refractive index

#Na: Sodium (unit measurement: weight percent in corresponding oxide, as are attributes 4-10)
#Mg: Magnesium
#AI: Aluminum
#Si: Silicon
#K:Potassium
#Ca: Calcium
#Ba: Barium
#Fe: Iron

#Type: Type of glass: (class attribute)
#1 -- building_windows_float_processed
#2 --building_windows_non_float_processed
#3 --vehicle_windows_float_processed
#4 --vehicle_windows_non_float_processed (none in this database)
#5 --containers
#6 --tableware
#7 --headlamps

Org_glass<-read.csv(choose.files()) 
str(Org_glass)
summary(Org_glass)
sum(is.na(Org_glass))
table(Org_glass$Type)
round(prop.table(table(Org_glass$Type))*100,digit=2)

norm <- function(x) { 
                      return((x-min(x))/(max(x)-min(x)))
                    }

norm_x_values <- as.data.frame(lapply(Org_glass[1:9], norm)) #normalizing all X values

View(norm_x_values)
summary(norm_x_values)
summary(norm_x_values)

norm_glass <-cbind(Org_glass[10],norm_x_values) #combining Y values and normalized X values

library("caTools")
split<- sample.split(norm_glass$RI,SplitRatio = 0.7) # spliting data in to train and test data
train_data<-subset(norm_glass,split==TRUE)
test_data<-subset(norm_glass,split==FALSE)

round(prop.table(table(train_data$Type))*100,digit=2) # checked proportion for train and test data
round(prop.table(table(test_data$Type))*100,digit=2)

library("class")
library("caret")

pred_type<-knn(train_data[,2:10],test_data[,2:10],train_data$Type,k=13) #built a KNN model with k=13

table(pred_type) # compared test predicted Y values and original Y values
table(test_data$Type)
table(pred_type,test_data$Type)

library(gmodels)
CrossTable(test_data$Type,pred_type)


train_acc <- NULL # created for loop for i=1 to 10 with interval of 1 and stored train accuracies in train_acc
for (i in seq(8,15,1))
{
  train_glass_pred <- knn(train_data,train_data,train_data$Type,k=i)
  train_acc <- c(train_acc,mean(train_glass_pred==train_data$Type))
  }

test_acc <- NULL # created for loop for i=1 to 10 with interval of 1 and stored test accuracies in test_acc
for (i in seq(8,15,1))
{
  test_glass_pred <- knn(train_data, test_data, train_data$Type, k=i)
  test_acc <- c(test_acc,mean(test_glass_pred==test_data$Type))
}

par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(8,15,1),train_acc,type="l",main="Train_accuracy",col="blue") #train accuract plot
plot(seq(8,15,1),test_acc,type="l",main="Test_accuracy",col="red") #test accuracy plot

acc_neigh_df <- data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=seq(8,15,1))) #stored train and test accuracies in acc_neigh_df

library(ggplot2) #plot graph of tain acc and test acc to get optimum k value for given data set
ggplot(acc_neigh_df,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))

#as per the graph optimum K value is 11 as testing accuract starts to fall after 11
