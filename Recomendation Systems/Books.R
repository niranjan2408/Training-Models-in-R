library("recommenderlab")
library(caTools)

#movie rating data
books_data <- read.csv(choose.files())

books_data<-books_data[1:1000,-2] #selecting first 1000 observations and removing id column

sum(is.na(books_data)) # checked na values

str(books_data) #metadata about the variable

hist(books_data$Book.Rating) #rating distribution

books_data_matrix <- as(books_data, 'realRatingMatrix') #the datatype should be realRatingMatrix inorder to build recommendation engine

#Popularity based 

books_data_model1 <- Recommender(books_data_matrix, method="POPULAR")

#Predictions for five users 
recommended_books_1 <- predict(books_data_model1, books_data_matrix[10:15], n=5)
as(recommended_books_1, "list")

## Popularity model recommends the same movies for all users 


#User Based Collaborative Filtering

recommended_books_2 <- Recommender(books_data_matrix, method="UBCF")

#Predictions for five users 
recommended_books_2 <- predict(recommended_books_2, books_data_matrix[10:15], n=5)
as(recommended_books_2, "list")

