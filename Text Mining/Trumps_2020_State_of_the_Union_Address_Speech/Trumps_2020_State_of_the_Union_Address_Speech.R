library(rvest)
library(XML)
library(magrittr)
library(rapportools)
library(NLP)
library(rJava)
library(tm)
library(SnowballC)
library(wordcloud)
library(RWeka)	
library(qdap)		
library(textir)
library(data.table)
library(stringr)
library(slam)
library(ggplot2)
library(quanteda)
library(Matrix)

# Making wordcloud function 
makewordc = function(x){	
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  windows()
  wordcloud(freq.df$word[1:120], freq.df$freq[1:120],scale = c(4,.5),random.order = F, colors=1:10)
} 

# Making positive wordcloud function 
makeposwordc = function(x){
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  # matching positive words
  pos.matches = match(names(freq), c(pos.words,"approvals"))
  pos.matches = !is.na(pos.matches)
  freq_pos <- freq[pos.matches]
  names <- names(freq_pos)
  windows()
  wordcloud(names,freq_pos,scale=c(4,.5),colors = brewer.pal(8,"Dark2"))
}

# Making negatice wordcloud function
makenegwordc = function(x){	
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  # matching negatice words
  neg.matches = match(names(freq), neg.words)
  neg.matches = !is.na(neg.matches)
  freq_neg <- freq[neg.matches]
  names <- names(freq_neg)
  windows()
  wordcloud(names,freq_neg,scale=c(4,.5),colors = brewer.pal(8,"Dark2"))
}



words_bar_plot <- function(x){
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  head(freq.df, 20)
  library(ggplot2)
  windows()
  ggplot(head(freq.df,50), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("Words") + ylab("Frequency") +
    ggtitle("Most frequent words")
  
}

pos_words_bar_plot <- function(x){
  pos.matches = match(colnames(x), pos.words)
  pos.matches = !is.na(pos.matches)
  pos_words_freq = as.data.frame(apply(x, 2, sum)[pos.matches])
  colnames(pos_words_freq)<-"freq"
  pos_words_freq["word"] <- rownames(pos_words_freq)
  # Sorting the words in deceasing order of their frequency
  pos_words_freq <- pos_words_freq[order(pos_words_freq$freq,decreasing=T),]
  windows()
  ggplot(head(pos_words_freq,30), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("Positive words") + ylab("Frequency") +
    ggtitle("Most frequent positive words")
}
neg_words_bar_plot <- function(x){
  neg.matches = match(colnames(x), neg.words)
  neg.matches = !is.na(neg.matches)
  neg_words_freq = as.data.frame(apply(x, 2, sum)[neg.matches])
  colnames(neg_words_freq)<-"freq"
  neg_words_freq["word"] <- rownames(neg_words_freq)
  # Sorting the words in deceasing order of their frequency
  neg_words_freq <- neg_words_freq[order(neg_words_freq$freq,decreasing=T),]
  windows()
  ggplot(head(neg_words_freq,30), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("words") + ylab("Frequency") +
    ggtitle("Most frequent negative words")
}


# --- func to make cluster dendograms --- #
clusdend = function(a){	# writing func clusdend() 	
  mydata.df = as.data.frame(inspect(a));	
  mydata1.df = mydata.df[, order(-colSums(mydata.df))];
  min1 = min(ncol(mydata.df), 40) 	# minimum dimn of dist matrix
  test = matrix(0,min1,min1)
  test1 = test
  for(i1 in 1:(min1-1)){ 
    for(i2 in i1:min1){
      test = sum(mydata1.df[ ,i1]-mydata1.df[ ,i2])^2
      test1[i1,i2] = test; test1[i2, i1] = test1[i1, i2] 	}
  }
  # making dissimilarity matrix out of the freq one
  test2 = test1
  rownames(test2) = colnames(mydata1.df)[1:min1]
  # now plot collocation dendogram
  d <- dist(test2, method = "euclidean") # distance matrix
  fit <- hclust(d, method="ward")
  windows()
  plot(fit) # display dendogram
} # clusdend() func ends



########### Downloading the Donald Trupm's 2020 State of the Union Address Speect from The New York Times ################


NY_Times_url <- "https://www.nytimes.com/2020/02/05/us/politics/state-of-union-transcript.html"
Trumps_2020_State_of_the_Union_Address_Speech <- NULL
NY_murl <- read_html(as.character(paste(NY_Times_url)))
NY_rev <- NY_murl %>%  html_nodes(".css-53u6y8") %>%  html_text()
Trumps_2020_State_of_the_Union_Address_Speech <- c(Trumps_2020_State_of_the_Union_Address_Speech,NY_rev)
length(Trumps_2020_State_of_the_Union_Address_Speech)
write.table(Trumps_2020_State_of_the_Union_Address_Speech,"Trumps_2020_State_of_the_Union_Address_Speech.txt",row.names = F)


################# Sentiment Analysis for tweets:#################

# Read File 
speech_data <- read.csv(file.choose(), header = TRUE)
str(speech_data)
speech_data_char <- as.character(speech_data$x) #converting text column from factor to character
class(speech_data_char)
speech_data_char <- stemDocument(speech_data_char)

library("syuzhet")
Sentiment_scores <- get_nrc_sentiment(speech_data_char) # generating sentiment scores and generating bar plot
head(Sentiment_scores)
windows()
barplot(colSums(Sentiment_scores),las = 2.5,col = rainbow(10),ylab = 'Count',main= 'Sentiment scores for Trumps_2020_State_of_the_Union_Address_Speech')


###################   Semantic_Analysis for tweets:   ##################

# Building Corpus 

corpus <- Corpus(VectorSource(speech_data$x))
inspect(corpus[1:3])

corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,removeNumbers)

stopwdrds = readLines(file.choose())
cleanset <- tm_map(corpus, removeWords, c(stopwords("english"),stopwdrds,"good","back","days","rate","great","time","year","made","million","ago","percent","years","tonight","trump","president","applause","america","americans","american"))
cleanset <- tm_map(cleanset,stripWhitespace)

inspect(cleanset[1:3])

# creating term document matrix (TDM)
tdm <- TermDocumentMatrix(cleanset)
tdm_matrix <- as.matrix(tdm)
tdm_matrix[1:10,1:19]

#creating term frequency-inverse document frequency (TFIDF)
tfidf <- TermDocumentMatrix(cleanset,control = list(weighting = function(p) weightTfIdf(p,normalize = T)))
tfidf_matrix <- as.matrix(tfidf)
tfidf_matrix[1:10,1:19]

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
