library(tm)
library(tidyverse)
library(RWeka)

setwd("~/R/Data-Science-Capstone/Capstone/SwiftKey Dataset/en_US") #set working directory
options(warn=-1) #Global stop for warnings


#load the text data
blogs <- readLines("en_US.blogs.txt", skipNul = TRUE, encoding = "UTF-8")
news <- readLines("en_US.news.txt", skipNul = TRUE, encoding = "UTF-8")
twitter <- readLines("en_US.twitter.txt", skipNul = TRUE, encoding = "UTF-8")

#make corpus
Corpusblogs <- Corpus(VectorSource(blogs))
Corpusnews <- Corpus(VectorSource(news))
Corpustwitter <- Corpus(VectorSource(twitter))

#take a sample of the corpora
sampling <- c(sample(Corpusblogs, length(Corpusblogs)*0.02), 
             sample(Corpusnews, length(Corpusnews)*0.02), 
             sample(Corpustwitter, length(Corpustwitter)*0.02))

#clean the sample
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, PlainTextDocument)

#make a data frame of the clean and stemmed sample data corpus
corpusresult<-data.frame(text=unlist(sapply(corpus,'[',"content")),stringsAsFactors = FALSE)

print(head(corpusresult))

#number of unique words in the sample
show(dim.data.frame(corpusresult)[1])


#How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%? 

x <- sort(table(corpusresult),decreasing=T)
print(head(x))







'




#Unigrams
unigram <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
tdm_unigram = TermDocumentMatrix(corpus, control = list(tokenize = unigram))

unigramcorpus<-findFreqTerms(tdm_unigram,lowfreq=80)
unicorpus_sums <- rowSums(as.matrix(tdm_unigram[unigramcorpus,]))
tdm_unigram <- data.frame(Word = names(unicorpus_sums), frequency = unicorpus_sums)
unicorpus_sort <- tdm_unigram[order(-tdm_unigram$frequency),]

g<- ggplot(unicorpus_sort[1:12,],aes(x=reorder(Word,-frequency),y=frequency))+
        geom_bar(stat="identity",fill = I("grey50"))+
        labs(title="Unigrams",x="Most Words",y="Frequency")+
        theme(axis.text.x=element_text(angle=60, vjust = .5))
show(g)


#Bigrams
Bigram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm_bigram = TermDocumentMatrix(corpus, control = list(tokenize = Bigram))

bigramcorpus<-findFreqTerms(tdm_bigram,lowfreq=80)
bicorpus_sums <- rowSums(as.matrix(tdm_bigram[bigramcorpus,]))
tdm_bigram <- data.frame(Word = names(bicorpus_sums), frequency = bicorpus_sums)
bicorpus_sort <- tdm_bigram[order(-tdm_bigram$frequency),]

h <- ggplot(bicorpus_sort[1:12,],aes(x=reorder(Word,-frequency),y=frequency))+
        geom_bar(stat="identity",fill = I("grey50"))+
        labs(title="Bigrams",x="Most Words",y="Frequency")+
        theme(axis.text.x=element_text(angle=60, vjust = .5))

show(h)


#Trigrams
trigram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdm_trigram = TermDocumentMatrix(corpus, control = list(tokenize = trigram))

trigramcorpus<-findFreqTerms(tdm_trigram,lowfreq=10)
tricorpus_sums <- rowSums(as.matrix(tdm_trigram[trigramcorpus,]))
tdm_trigram <- data.frame(Word = names(tricorpus_sums), frequency = tricorpus_sums)
tricorpus_sort <- tdm_trigram[order(-tdm_trigram$frequency),]

j <- ggplot(tricorpus_sort[1:10,],aes(x=reorder(Word,-frequency),y=frequency))+
        geom_bar(stat="identity",fill = I("grey50"))+
        labs(title="Trigrams",x="Most Words",y="Frequency")+
        theme(axis.text.x=element_text(angle=60, vjust = .5))

show(j)


'

