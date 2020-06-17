#Prepare rules for the all the data sets 
#1) Try different values of support and confidence. Observe the change in number of rules for different support,confidence values
#2) Change the minimum length in apriori algorithm
#3) Visulize the obtained rules using different plots 

library(arules)

library(arulesViz)



book<-read.csv(choose.files()) #read csv file

book1<-as.matrix(book) #convert data frame into matrix

book2<-as(book1,"transactions") #converting into formal class transactions so that we can use it in apriori

summary(book2) #density of 0.2202273



rules <- apriori(book2,parameter=list(support=0.02, confidence = 0.7,minlen=6))

inspect(sort(rules,by="lift"))

plot(rules, method = "scatterplot")

plot(rules, method = "grouped")

plot(rules, method = "graph")

inspect(head(sort(rules, by="lift")))



rules1 <- apriori(book2,parameter=list(support=0.0222, confidence = 0.8,minlen=6))

inspect(sort(rules1,by="lift"))

plot(rules1, method = "scatterplot")

plot(rules1, method = "grouped")

plot(rules1, method = "graph")

inspect(head(sort(rules1, by="lift")))




rules2 <- apriori(book2,parameter=list(support=0.1, confidence = 0.7,minlen=3))

inspect(sort(rules2,by="lift"))

plot(rules2, method = "scatterplot")

plot(rules2, method = "grouped")

plot(rules2, method = "graph")

inspect(head(sort(rules2, by="lift")))




rules3 <- apriori(book2,parameter=list(support=0.05, confidence = 0.8,minlen=5))

inspect(sort(rules3,by="lift"))

plot(rules3, method = "scatterplot")

plot(rules3, method = "grouped")

plot(rules3, method = "graph")

inspect(head(sort(rules3, by="lift")))
