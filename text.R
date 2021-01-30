#sentiment analysis
rm(list=ls())
path<-"F:/DMBI R/Mini Project"
setwd(path)
getwd()
library(wordcloud)
library(plyr)

library(tidyverse)

library(wordcloud)

library(tm)

library(SnowballC)

library(lubridate)

library(ggcorrplot)

library(DMwR)
library(caret)
library(rpart)
library(rpart.plot)

library(pROC)

library(randomForest)

library(ipred)

library(caretEnsemble)
# Text manipulation
df1 <- read.csv("USvideos.csv")

library(data.table)
library(tidytext)
library(stringr)
library(tm)
library(sentimentr)
library(wordcloud)
library(RSentiment)
biga <- unnest_tokens(df1,bigram, title, token = "ngrams", n = 2)
biga <- as.data.table(biga)

ggplot(biga[,.N,by=bigram][order(-N)][1:19],aes(reorder(bigram,-N),N,fill=bigram))+geom_bar(stat="identity")+geom_label(aes(label=N))+guides(fill="none")+theme(axis.text.x = element_text(angle = 45,hjust = 1))+  labs(title="Top Description bigrams")+xlab(NULL)+ylab(NULL)

biga <- unnest_tokens(df1,bigram, tags, token = "ngrams", n = 3)
biga <- as.data.table(biga)

ggplot(biga[,.N,by=bigram][order(-N)][1:19],aes(reorder(bigram,-N),N,fill=bigram))+geom_bar(stat="identity")+geom_label(aes(label=N))+guides(fill="none")+theme(axis.text.x = element_text(angle = 45,hjust = 1))+  labs(title="Top Description bigrams")+xlab(NULL)+ylab(NULL)

biga <- unnest_tokens(df1,bigram, description, token = "ngrams", n = 3)
biga <- as.data.table(biga)

ggplot(biga[,.N,by=bigram][order(-N)][1:19],aes(reorder(bigram,-N),N,fill=bigram))+geom_bar(stat="identity")+geom_label(aes(label=N))+guides(fill="none")+theme(axis.text.x = element_text(angle = 45,hjust = 1))+  labs(title="Top Description bigrams")+xlab(NULL)+ylab(NULL)


library(RColorBrewer)

yu<-data.frame(df1)
yu.Corpus<-Corpus(VectorSource(yu$description))
#yu.Corpus<-Corpus(DataframeSource(yu$description))
yu.Clean<-tm_map(yu.Corpus, PlainTextDocument)
yu.Clean<-tm_map(yu.Corpus,tolower)
yu.Clean<-tm_map(yu.Clean,removeNumbers)
yu.Clean<-tm_map(yu.Clean,removeWords,stopwords("english"))
yu.Clean<-tm_map(yu.Clean,removePunctuation)
yu.Clean<-tm_map(yu.Clean,stripWhitespace)
yu.Clean<-tm_map(yu.Clean,stemDocument)
wordcloud(yu.Clean,max.words = 200,random.color = TRUE,random.order=FALSE)
