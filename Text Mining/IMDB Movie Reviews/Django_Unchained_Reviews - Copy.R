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



aurl <- "https://www.imdb.com/title/tt1853728/reviews?ref_=tt_urv"
Django_Unchained_Reviews <- NULL
for (i in 1:1){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%  html_nodes(".show-more__control") %>%  html_text()
  Django_Unchained_Reviews <- c(Django_Unchained_Reviews,rev)
}

length(Django_Unchained_Reviews)


write.table(Django_Unchained_Reviews,"Django_Unchained_Reviews.txt",row.names = F)

# lOADING +VE AND -VE words  
pos.words=scan(file.choose(), what="character", comment.char=";")	# read-in positive-words.txt
neg.words=scan(file.choose(), what="character", comment.char=";") 	# read-in negative-words.txt
stopwdrds = readLines(file.choose())


Django_Unchained_Reviews= readLines(file.choose())

Django_Unchained_Reviews <- stemDocument(Django_Unchained_Reviews)

Django_Unchained_Reviews_corpus <- Corpus(VectorSource(Django_Unchained_Reviews))

inspect(Django_Unchained_Reviews_corpus[c(1:5)])

corpus_clean = tm_map(Django_Unchained_Reviews_corpus, tolower)
corpus_clean = tm_map(corpus_clean, removeNumbers)
corpus_clean = tm_map(corpus_clean, removeWords,c(stopwords("english"),stopwdrds))
corpus_clean = tm_map(corpus_clean, removePunctuation)
corpus_clean = tm_map(corpus_clean, stripWhitespace)
inspect(corpus_clean[c(1:5)])


#term document metrix
tdm0 <- TermDocumentMatrix(corpus_clean)
inspect(tdm0)
# Term document matrix with inverse frequency 
tdm1 <- TermDocumentMatrix(corpus_clean,control = list(weighting = function(p) weightTfIdf(p,normalize = F),stopwords=T))#,stemming=T))
inspect(tdm1)

a0 <- NULL
a1 <- NULL
# getting the indexes of documents having count of words = 0
for (i1 in 1:ncol(tdm0))
{ if (sum(tdm0[, i1]) == 0) {a0 = c(a0, i1)} }
for (i1 in 1:ncol(tdm1))
{ if (sum(tdm1[, i1]) == 0) {a1 = c(a1, i1)} }

# Removing empty docs 
tdm0 <- tdm0[,-a0]
tdm1 <- tdm1[,-a1]

# Document term matrix 
dtm0 <- t(tdm0)
inspect(dtm0)

dtm1 <- t(tdm1)
inspect(dtm1)

# Word cloud - TF - Uni gram
makewordc(tdm0)
title(sub = "UNIGRAM - Wordcloud using TF")
# Frequency Bar plot - TF - Uni gram
words_bar_plot(tdm0)


# Word cloud - TFIDF - Unigram
makewordc(tdm1)
title(sub = "UNIGRAM - Wordcloud using TFIDF")
# Frequency Barplot - TFIDF - Unigram
words_bar_plot(tdm1)


# Positive word cloud - TF - Unigram
makeposwordc(tdm0)
title(sub = "UNIGRAM - POSITIVE Wordcloud using TF")
# Frequency Barplot - Positive words - Unigram
pos_words_bar_plot(dtm0)


# Positive word cloud - Unigram - TFIDF
makeposwordc(tdm1)
title(sub = "UNIGRAM - POSITIVE Wordcloud using TFIDF")
# Frequency Barplot - Positive words - TFIDF - Unigram
pos_words_bar_plot(dtm1)


# Negative word cloud - TF - unigam
makenegwordc(tdm0) # doubts doubt 
title(sub = "UNIGRAM - NEGATIVE Wordcloud using TF")
# Frequency Barplot -negative words - Unigram - TF
neg_words_bar_plot(dtm0)


# Negative word cloud - Unigram - TFIDF
makenegwordc(tdm1) # doubts doubt 
title(sub = "UNIGRAM - NEGATIVE Wordcloud using TFIDF")
# Frequency Barplot - Negative words - TFIDF
neg_words_bar_plot(dtm1)






################# Sentiment Analysis for reviews:#################
library("syuzhet")

Sentiment_scores <- get_nrc_sentiment(Django_Unchained_Reviews) # generating sentiment scores and generating bar plot
head(Sentiment_scores)
windows()
barplot(colSums(Sentiment_scores),las = 2.5,col = rainbow(8),ylab = 'Count',main= 'Django_Unchained_Reviews Sentiment Analysis')
