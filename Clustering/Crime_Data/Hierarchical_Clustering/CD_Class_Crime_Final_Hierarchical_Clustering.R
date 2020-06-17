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
euclidean_dist<-dist(normalized_data,method = "euclidean")
hierarchical_clustering <- hclust(euclidean_dist,method="complete")
plot(hierarchical_clustering,hang=-1)
Class_Crime<-cutree(hierarchical_clustering,k=4)
rect.hclust(hierarchical_clustering,k=4)
Class_Crime<-as.matrix(Class_Crime)
CD_Class_Crime<-data.frame(CD,Class_Crime)
CD_Class_Crime_Final<-CD_Class_Crime[,c(ncol(CD_Class_Crime),1:(ncol(CD_Class_Crime)-1))]
head(CD_Class_Crime_Final)
write.xlsx(CD_Class_Crime_Final,file = "CD_Class_Crime_Final_Hierarchical_Clustering.xlsx")
Summary<-aggregate(CD_Class_Crime_Final[,c(1,3:6)],by=list(CD_Class_Crime$Class_Crime),FUN=mean)
Summary
