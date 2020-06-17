#Implement a KNN model to classify the animals in to categories


org_zoo<-read.csv(choose.files())
str(org_zoo)
summary(org_zoo)
sum(is.na(org_zoo))
table(org_zoo$type)
round(prop.table(table(org_zoo$type))*100,digit=2)
zoo_data<-org_zoo[-1] # first column is of no sifnificance

norm <- function(x) { 
                      return((x-min(x))/(max(x)-min(x)))
                    }

norm_x_values <- as.data.frame(lapply(zoo_data[1:16], norm)) #normalizing all X values

View(norm_x_values)
summary(norm_x_values)

norm_zoo <-cbind(zoo_data[17],norm_x_values) #combining Y values and normalized X values

library("caTools")
split<- sample.split(norm_zoo$hair,SplitRatio = 0.7) # spliting data in to train and test data
train_data<-subset(norm_zoo,split==TRUE)
test_data<-subset(norm_zoo,split==FALSE)

round(prop.table(table(train_data$type))*100,digit=2) # checked proportion for train and test data
round(prop.table(table(test_data$type))*100,digit=2)

library("class")

pred_type<-knn(train_data[,2:17],test_data[,2:17],train_data$type,k=5) #built a KNN model with k=5

table(pred_type) # compared test predicted Y values and original Y values
table(test_data$type)
table(pred_type,test_data$type)

library(gmodels)
CrossTable(test_data$type,pred_type)


train_acc <- NULL # created for loop for i=1 to 10 with interval of 1 and stored train accuracies in train_acc
for (i in seq(1,10,1))
{
  train_pred <- knn(train_data,train_data,train_data$type,k=i)
  train_acc <- c(train_acc,mean(train_pred==train_data$type))
  }

test_acc <- NULL # created for loop for i=1 to 10 with interval of 1 and stored test accuracies in test_acc
for (i in seq(1,10,1))
{
  test_pred <- knn(train_data, test_data, train_data$type, k=i)
  test_acc <- c(test_acc,mean(test_pred==test_data$type))
}

par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(1,10,1),train_acc,type="l",main="Train_accuracy",col="blue") #train accuract plot
plot(seq(1,10,1),test_acc,type="l",main="Test_accuracy",col="red") #test accuracy plot

acc_neigh_df <- data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=seq(1,10,1))) #stored train and test accuracies in acc_neigh_df

library(ggplot2) #plot graph of tain acc and test acc to get optimum k value for given data set
ggplot(acc_neigh_df,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))

#as per the graph optimum K value is 3 as testing accuract starts to fall after 3