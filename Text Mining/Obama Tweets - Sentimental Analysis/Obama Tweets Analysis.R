library("twitteR")
library("ROAuth")
library("base64enc")
library("httpuv")
library("rtweet")
library("syuzhet")
library("tm")
library("wordcloud")


cred <- OAuthFactory$new(consumerKey='BagGgBbanzbdpPNNp8Uy6TQBP', 
                         consumerSecret='pFxap1Jzc1fClDQ9psLNU3RKSQ5FvS2PhJz8E2R7ix0cawPKfa', 
                         requestURL='https://api.twitter.com/oauth/request_token',                
                         accessURL='https://api.twitter.com/oauth/access_token',                 
                         authURL='https://api.twitter.com/oauth/authorize')

save(cred, file="twitter authentication.Rdata")
load("twitter authentication.Rdata")

setup_twitter_oauth("BagGgBbanzbdpPNNp8Uy6TQBP", 
                    "pFxap1Jzc1fClDQ9psLNU3RKSQ5FvS2PhJz8E2R7ix0cawPKfa", 
                    "1076425245521731584-Ev31ZLB7Cf0idVMqDI8BxiVG2SgRnu",  
                    "ZVUw0Z0mFrX7d6sjQxuB08l48JHhmnjmlAm86G2OPG7BS")  


#################  Dounloading tweets of Barac Obama  #################3

Tweets <- userTimeline('BarackObama', n = 1000,includeRts = T) #Recent 1000 tweets
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
write.csv(TweetsDF,"BarackObama_tweets.csv")



################# Sentiment Analysis for tweets:#################

# Read File 
tweet_data <- read.csv(file.choose(), header = TRUE)
str(tweet_data)
tweets <- as.character(tweet_data$text) #converting text column from factor to character
class(tweets)

Sentiment_scores <- get_nrc_sentiment(tweets) # generating sentiment scores and generating bar plot
head(Sentiment_scores)
windows()
barplot(colSums(Sentiment_scores),las = 2.5,col = rainbow(10),ylab = 'Count',main= 'Sentiment scores for BarackObamas last 1000 Tweets')



###################   Semantic_Analysis for tweets:   ##################

# Building Corpus 
tweet_text <- gsub("[\r\n]", "", tweet_data$text) #gsub("[\r\n]", "", used to remove \n (new line) from the text
corpus <- Corpus(VectorSource(tweet_text))
inspect(corpus[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*','',x) # for removing anu url in the data
corpus <- tm_map(corpus, content_transformer(removeURL))
corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,removeNumbers)

stopwdrds = readLines(file.choose())
cleanset <- tm_map(corpus, removeWords, c(stopwords("english"),stopwdrds,"obama"))
cleanset <- tm_map(cleanset,stripWhitespace)

inspect(cleanset[1:5])

# creating term document matrix (TDM)
tdm <- TermDocumentMatrix(cleanset)
tdm_matrix <- as.matrix(tdm)
tdm_matrix[1:10,1:20]

#creating term frequency-inverse document frequency (TFIDF)
tfidf <- TermDocumentMatrix(cleanset,control = list(weighting = function(p) weightTfIdf(p,normalize = T)))
tfidf_matrix <- as.matrix(tfidf)
tfidf_matrix[1:10,1:20]

# creating list of empty documents after cleaning a0 for TDM and a1 for TFIDF
a0 <- NULL
a1 <- NULL
# getting the indexes of documents having count of words = 0
for (i1 in 1:ncol(tdm))
{ if (sum(tdm[, i1]) == 0) {a0 = c(a0, i1)} }
for (i1 in 1:ncol(tfidf))
{ if (sum(tfidf[, i1]) == 0) {a1 = c(a1, i1)} }

#a0 and a1 both are null so need to write program for removing empty documents


# Document term matrix  (DTM) for tdm and tfidf
dtm <- t(tdm)
dtm1 <- t(tfidf)


#####################  #defining functions for plots ####################
makewordc = function(x){	
                          freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
                          freq.df = data.frame(word=names(freq), freq=freq)
                          windows()
                          wordcloud(freq.df$word[1:120], freq.df$freq[1:120],scale = c(4,.5),random.order = F, colors=1:10)
                        } 

#defining functions for bar plot
words_bar_plot <- function(x){
                              freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
                              freq.df = data.frame(word=names(freq), freq=freq)
                              head(freq.df, 20)
                              library(ggplot2)
                              windows()
                              ggplot(head(freq.df,50), aes(reorder(word,freq), freq)) +
                              geom_bar(stat = "identity", col = rainbow(50)) + coord_flip() +
                              xlab("Words") + ylab("Frequency") +
                              ggtitle("Most frequent words")
                              }


############################ Word cloud and bar plot  ################

# Word cloud and bar plot - TF - Unigram
makewordc(tdm)
title(sub = "UNIGRAM - Wordcloud using TF")
words_bar_plot(tdm)


# Word cloud and bar plot - TFIDF - Unigram
makewordc(tfidf)
title(sub = "UNIGRAM - Wordcloud using TFIDF")
words_bar_plot(tfidf)

# Further we can remover words after analyzing the above plots which are not adding value to the analysis 
#by adding such words into remove words command on line no 64
