#Perform Clustering for the crime data and identify the number of clusters formed and draw inferences.
#Data Description:
#Murder -- Muder rates in different places of United States
#Assualt- Assualt rate in different places of United States
#UrbanPop - urban population in different places of United States
#Rape - Rape rate in different places of United States

CD<-read.csv(choose.files())
str(CD)
summary(CD)
CD1<-CD[,-1]
sum(is.na(CD1))
normalized_data<-scale(CD1)
kmeans<-kmeans(normalized_data,5)
summary(kmeans)
CD_Kmeans<-data.frame(CD,kmeans$cluster)
CD_Class_Crime_Final_Kmeans<-CD_Kmeans[,c(ncol(CD_Kmeans),1:(ncol(CD_Kmeans)-1))]
library(openxlsx)
write.xlsx(CD_Class_Crime_Final_Kmeans,file = "CD_Class_Crime_Final_Kmeans.xlsx")

#k selection

library(kselection)
library(doParallel)
library(openxlsx)

registerDoParallel(core=4)
k1<-kselection(normalized_data, parallel = TRUE,k_threshold = 0.95,max_centers = 13 )
k1
# 2 clusters are significant

#k~ sqrt(n/2)
k2=sqrt(50/2)
k2
# 5 number of clusters are signigicant

#elbow curve
twss<-NULL
for(i in 1:14)
    twss[i]=sum(kmeans(normalized_data, centers=i)$tot.withinss)
plot(1:14,twss, type="b",xlab="No of Clusters", ylab="within groups sum of squares")
title(sub="kmeans clustering means plot")

#As per the graph there is significant change after 4 and 5 number of clusters. So we can go with eigther 4 or 5 clusters