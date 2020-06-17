#Build a naive Bayes model on the data set for classifying the ham and spam


#selecting the raw data file and analyzinng it

sms_raw <- read.csv(choose.files()) 
str(sms_raw)
table(sms_raw$type)
sms_raw$text <- as.character(sms_raw$text)

# creating the corpus of text column data

library("tm")
sms_corpus <- VCorpus(VectorSource(sms_raw$text)) 


#Cleaning the corpus

corpus_clean <- tm_map(sms_corpus, tolower)
corpus_clean <- tm_map(sms_corpus, removeNumbers)
corpus_clean <- tm_map(sms_corpus, removeWords,stopwords())
corpus_clean <- tm_map(sms_corpus, removePunctuation)
corpus_clean <- tm_map(sms_corpus, stripWhitespace)
corpus_clean <- tm_map(sms_corpus, PlainTextDocument)

#creating document term matrix

sms_dtm <- DocumentTermMatrix(corpus_clean)

#creating training and tresting data sets

sms_raw_train <- sms_raw[1:4169,]
sms_raw_test <- sms_raw[4170:5559,,]

sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4170:5559,]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test <- corpus_clean[4170:5559]

#checking proportion in train and test
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

#setting up frequency for selecting most occuring words (here setting as "4")

sms_dist <- findFreqTerms(sms_dtm_train,4)

sms_dtm_train_1 <- DocumentTermMatrix(sms_corpus_train,list(dictonary=sms_dist))
sms_dtm_test_1 <- DocumentTermMatrix(sms_corpus_test,list(dictonary=sms_dist))

conver_count <- function(x) {
  x <- ifelse(x>0,1,0)
  x <- factor(x,levels=c(0,1),labels=c("No","Yes"))
}


sms_dtm_train_1 <- apply(sms_dtm_train_1,MARGIN = 2,conver_count)
sms_dtm_test_1 <- apply(sms_dtm_test_1,MARGIN = 2,conver_count)

library("e1071")

#building model on naiveBayes
sms_classifier1 <- naiveBayes(sms_dtm_train_1,sms_raw_train$type)

#evaluating madel on test data
sms_test_predict1 <- predict(sms_classifier1,sms_dtm_test_1)

table1 <- table(sms_raw_test$type, sms_test_predict1)
table1

accuracy1 <- sum(diag(table1))/sum(table1)
accuracy1

mean(sms_test_predict1==sms_raw_test$type)

library(gmodels)
CrossTable(sms_raw_test$type,sms_test_predict1,prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,dnn=c('predict','actual'))


#building model including laplace
sms_classifier2 <- naiveBayes(sms_dtm_train_1,sms_raw_train$type,laplace = 6)

#evaluating madel2 on test data
sms_test_predict2 <- predict(sms_classifier2,sms_dtm_test_1)

CrossTable(sms_raw_test$type,sms_test_predict2,prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,dnn=c('predict','actual'))

table2 <- table(sms_raw_test$type,sms_test_predict2)
table2

accuracy2 <- sum(diag(table2))/sum(table2)
accuracy2

mean(sms_test_predict2==sms_raw_test$type)

