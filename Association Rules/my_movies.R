#Prepare rules for the all the data sets 
#1) Try different values of support and confidence. Observe the change in number of rules for different support,confidence values
#2) Change the minimum length in apriori algorithm
#3) Visulize the obtained rules using different plots 

library(arules)
library(arulesViz)

my_movies<-read.csv(choose.files()) #read csv file

my_movies_binary<-my_movies[,6:15] #selecting only required columns

my_movies1<-as.matrix(my_movies_binary) #convert data frame into matrix

my_movies2<-as(my_movies1,"transactions") #converting into formal class transactions so that we can use it in apriori

summary(my_movies2) #density of 0.3




rules <- apriori(my_movies2,parameter=list(support=0.2, confidence = 0.5,minlen=2))

inspect(sort(rules,by="lift"))

plot(rules, method = "scatterplot")

plot(rules, method = "grouped")

plot(rules, method = "graph")

inspect(head(sort(rules, by="lift")))



rules1 <- apriori(my_movies2,parameter=list(support=0.25, confidence = 0.4,minlen=2))

inspect(sort(rules1,by="lift"))

plot(rules1, method = "scatterplot")

plot(rules1, method = "grouped")

plot(rules1, method = "graph")

inspect(head(sort(rules1, by="lift")))





rules2 <- apriori(my_movies2,parameter=list(support=0.1111, confidence = 0.75,minlen=2))

inspect(sort(rules2,by="lift"))

plot(rules2, method = "scatterplot")

plot(rules2, method = "grouped")

plot(rules2, method = "graph")

inspect(head(sort(rules2, by="lift")))




rules3 <- apriori(my_movies2,parameter=list(support=0.005, confidence = 0.9,minlen=4))

inspect(sort(rules3,by="lift"))

plot(rules3, method = "scatterplot")

plot(rules3, method = "grouped")

plot(rules3, method = "graph")

inspect(head(sort(rules3, by="lift")))


##second method

my_movies_names<-my_movies[,1:5] #selecting only required columns

my_movies_names1<-as(my_movies_names,"transactions")

inspect(my_movies_names1)

summary(my_movies_names1) #density of 0.2777778




rules4<-apriori(my_movies_names1,parameter = list(support=0.25,confidence=0.8,minlen=4))

inspect(head(sort(rules4,by="lift")))

inspect(sort(rules4,by="lift"))

plot(rules4, method = "scatterplot")

plot(rules4, method = "grouped")

plot(rules4, method = "graph")



rules5<-apriori(my_movies_names1,parameter = list(support=0.25,confidence=0.9,minlen=3))

inspect(sort(rules5,by="lift"))

plot(rules5, method = "scatterplot")

plot(rules5, method = "grouped")

plot(rules5, method = "graph")
