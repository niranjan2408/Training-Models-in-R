#Perform Principal component analysis and perform clustering using first 3 principal component scores 
#(both heirarchial and k mean clustering(scree plot or elbow curve) 
#and obtain optimum number of clusters and check whether we have obtained same number of clusters 
#with the original data (class column we have ignored at the begining who shows it has 3 clusters)df

wine<-read.csv(choose.files())
wine<-wine[,-1]
str(wine)
summary(wine)
sum(is.na(wine))

library(psych)
pairs.panels(wine, col="red")

pcaObj<- princomp(wine,cor=TRUE,scores =TRUE,covmat = NULL )

summary(pcaObj)
str(pcaObj)
loadings(pcaObj)
pcaObj$loadings

wine_PCA<-cbind(wine,pcaObj$scores[,1:3])
wine_PCA_clust_data<-wine_PCA[,14:16]
norm_wine_PCA_clust_data<-scale(wine_PCA_clust_data)
dist<-dist(norm_wine_PCA_clust_data,method="euclidean")
PCA_hclust<-hclust(dist, method="complete")
plot(PCA_hclust, hang=-1)
rect.hclust(PCA_hclust, k=6, border = "red")
PCA_groups<-cutree(PCA_hclust,6)
membership<-as.matrix(PCA_groups)
wine_PCA_clust_hierarchical<-cbind(membership,wine)
wine_PCA_clust_hierarchical_summary<-aggregate(wine_PCA_clust_hierarchical,by=list(membership),FUN=mean)
wine_PCA_clust_hierarchical_summary


library(openxlsx)
write.xlsx(wine_PCA_clust_hierarchical_summary,file = "wine_PCA_clust_hierarchical_summary.xlsx")
write.xlsx(wine_PCA_clust_hierarchical,file = "wine_PCA_clust_hierarchical.xlsx")



#clusters with the original data
#hierarchical_clustering
normalized_data_wine<-scale(wine)
euclidean_dist<-dist(normalized_data_wine,method = "euclidean")
hierarchical_clustering <- hclust(euclidean_dist,method="complete")
plot(hierarchical_clustering,hang=-1)
Class_wine<-cutree(hierarchical_clustering,k=4)
rect.hclust(hierarchical_clustering,k=4)
Class_wine<-as.matrix(Class_wine)
wine_Class_wine<-data.frame(wine,Class_wine)
wine_Class_wine_Final<-wine_Class_wine[,c(ncol(wine_Class_wine),1:(ncol(wine_Class_wine)-1))]
head(wine_Class_wine_Final)
write.xlsx(wine_Class_wine_Final,file = "wine_Class_wine_Final_Hierarchical_Clustering.xlsx")
Summary<-aggregate(wine_Class_wine_Final,by=list(wine_Class_wine_Final$Class_wine),FUN=mean)
Summary


#k selection

library(kselection)
library(doParallel)
registerDoParallel(core=4)
k1<-kselection(normalized_data_wine, parallel = TRUE,k_threshold = 0.95,max_centers = 11 )
k1
# 2 clusters are significant


#k~ sqrt(n/2)
k2=sqrt(178/2)
k2
# 9 number of clusters are signigicant. But we can not consider this as n is high



#elbow curve
twss<-NULL
for(i in 1:14)
  twss[i]=sum(kmeans(normalized_data_wine, centers=i)$tot.withinss)
plot(1:14,twss, type="b",xlab="No of Clusters", ylab="within groups sum of squares")
title(sub="kmeans clustering means plot")
#As per the graph there is significant change after 3 and 4 number of clusters. So we can go with 3 or 4 clusters



#kmeans_clustering
kmeans<-kmeans(normalized_data_wine,3)
summary(kmeans)
wine_Kmeans<-data.frame(wine,kmeans$cluster)
wine_Class_wine_Final_Kmeans<-wine_Kmeans[,c(ncol(wine_Kmeans),1:(ncol(wine_Kmeans)-1))]
write.xlsx(wine_Class_wine_Final_Kmeans,file = "wine_class_wine_Final_Kmeans_Clustering.xlsx")


#Summary of the assignment

#with Principal component analysis and performing clustering using first 3 principal component scores; 
#we can cluster the given data into 6 distinct clusters

#clusters with the original data
# 1) hierarchical_clustering - we can cluster the given data into 4 distinct clusters
# 2) kmeans_clustering - we can cluster the given data into 3 distinct clusters