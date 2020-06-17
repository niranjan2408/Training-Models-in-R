#About the data: 
#  Let's consider a Company dataset with around 10 variables and 400 records. 
#  The attributes are as follows: 
#  ??? Sales -- Unit sales (in thousands) at each location
#  ??? Competitor Price -- Price charged by competitor at each location
#  ??? Income -- Community income level (in thousands of dollars)
#  ??? Advertising -- Local advertising budget for company at each location (in thousands of dollars)
#  ??? Population -- Population size in region (in thousands)
#  ??? Price -- Price company charges for car seats at each site
#  ??? Shelf Location at stores -- A factor with levels Bad, Good and Medium 
#                                indicating the quality of the shelving location for the car seats at each site
#  ??? Age -- Average age of the local population
#  ??? Education -- Education level at each location
#  ??? Urban -- A factor with levels No and Yes to indicate whether the store is in an urban or rural location
#  ??? US -- A factor with levels No and Yes to indicate whether the store is in the US or not
 
#  Problem Statement:
#  A cloth manufacturing company is interested to know about the segment or attributes causes high sale. 
#  Approach - A decision tree can be built with target variable Sale (we will first convert it in categorical variable) 
#             & all other variable will be independent in the analysis.  

library(C50)
library(tree)
library(caTools)
library(dplyr)

CD <- read.csv(choose.files())
str(CD)
summary(CD) #as Q3 of Sales is (3rd Qu.:) 9.320 we can consider above sales above 10 thousands as high sale
hist(CD$Sales)
describe(CD)
sum(is.na(CD))

CD$Sales = ifelse(CD$Sales>10, "Yes", "No")
CD$Sales <- as.factor(CD$Sales)

str(CD)
table(CD$Sales)
prop.table(table(CD$Sales))

train_data <- CD [1:300,]
test_data <- CD [301:400,]

prop.table(table(train_data$Sales))
prop.table(table(test_data$Sales))

#split<- sample.split(CD$Sales,SplitRatio = 0.75) # spliting data in to train and test data
#train_data1<-data.frame(subset(CD,split==TRUE))
#test_data1<-data.frame(subset(CD,split==FALSE))

#prop.table(table(train_data1$Sales))
#prop.table(table(test_data1$Sales))

DT_train_data <- C5.0(train_data[,-1],train_data$Sales)
plot(DT_train_data) 

pred_train_data <- predict(DT_train_data,train_data)
table(pred_train_data,train_data$Sales)
mean(pred_train_data==train_data$Sales)


pred_test_data <- predict(DT_train_data,test_data)
table(pred_test_data,test_data$Sales)
mean(pred_test_data==test_data$Sales)

#install.packages("party",dependencies = TRUE)
library(party)
#?ctree
op_tree <-  ctree(Sales ~ CompPrice + Income + Advertising + Population + Price + ShelveLoc
                + Age + Education + Urban + US, data = train_data)
summary(op_tree)
plot(op_tree)

# On looking into the Above tree plot, it is clear that 

#if the shelf Location at stores is good and price is <=119
# then there is a probability of 80% chance that the customer will buy and sales will go high

# Also if the shelf Location at stores is good and price is >119 but compititor prices are > 146 
# then there is a probability of ~65% chance that the customer will buy and sales will go high

# If the shelf Location at stores is medium,bad and price is <=87  
# then there is a probability of 50% chance that the customer will buy and sales will go high