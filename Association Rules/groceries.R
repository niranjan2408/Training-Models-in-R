#Prepare rules for the all the data sets 
#1) Try different values of support and confidence. Observe the change in number of rules for different support,confidence values
#2) Change the minimum length in apriori algorithm
#3) Visulize the obtained rules using different plots 

library(arules)
library(arulesViz)


groceries<-read.transactions(choose.files(),format = "basket")

inspect(groceries[1:10])
  
summary(groceries) #density of 0.0005240481

itemFrequencyPlot(groceries,topN=10)




rules<-apriori(groceries,parameter = list(support=0.0004,confidence=0.8,minlen=5))

inspect(sort(rules,by="lift"))

plot(rules, method = "scatterplot")

plot(rules, method = "grouped")

plot(rules, method = "graph")

inspect(head(sort(rules, by="lift")))



rules1<-apriori(groceries,parameter = list(support=0.00045,confidence=0.9,minlen=5))

inspect(sort(rules1,by="lift"))

plot(rules1, method = "scatterplot")

plot(rules1, method = "grouped")

plot(rules1, method = "graph")

inspect(head(sort(rules1, by="lift")))


rules2<-apriori(groceries,parameter = list(support=0.00049555,confidence=0.7,minlen=5))

inspect(sort(rules2,by="lift"))

plot(rules2, method = "scatterplot")

plot(rules2, method = "grouped")

plot(rules2, method = "graph")

inspect(head(sort(rules2, by="lift")))


rules3<-apriori(groceries,parameter = list(support=0.0003,confidence=0.8,minlen=6))

inspect(sort(rules3,by="lift"))

plot(rules3, method = "scatterplot")

plot(rules3, method = "grouped")

plot(rules3, method = "graph")

inspect(head(sort(rules3, by="lift")))