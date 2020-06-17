library(readxl)
library(openxlsx)
Airlines<-read_xlsx(choose.files())
str(Airlines)
summary(Airlines)
sum(is.na(Airlines))
Airlines1<-Airlines[-1]

#Kmeans Clustering

normalized_data<-scale(Airlines1)

kmeans<-kmeans(normalized_data,8)

Airlines_passenger_class_Kmeans<-data.frame(Airlines,kmeans$cluster)

Airlines_passenger_class_Kmeans_Final<-Airlines_passenger_class_Kmeans[,c(ncol(Airlines_passenger_class_Kmeans),1:(ncol(Airlines_passenger_class_Kmeans)-1))]

write.xlsx(Airlines_passenger_class_Kmeans_Final,file = "Airlines_passenger_class_Kmeans_Final.xlsx")


#k selection

library(kselection)
library(doParallel)
library(openxlsx)

registerDoParallel(core=4)
k1<-kselection(normalized_data, parallel = TRUE,k_threshold = 0.95,max_centers = 13 )
k1
# 9 clusters are significant

#k~ sqrt(n/2)
k2=sqrt(3999/2)
k2
# 44 number of clusters are signigicant as per sqrt(n/2). BUt sqrt(n/2) is not useful for larger number of observations(n)

#elbow curve
twss<-NULL
for(i in 1:14)
  twss[i]=sum(kmeans(normalized_data, centers=i)$tot.withinss)
plot(1:14,twss, type="b",xlab="No of Clusters", ylab="within groups sum of squares")
title(sub="kmeans clustering means plot")

#As per the graph there is significant change after 5 or 7 number of clusters
