#Data Frame With Top 5 Words
library(tm)
library(lsa)
library(tidyverse)
library(tidytext)
library(stringr)
library(SnowballC)
library(RTextTools)
library(topicmodels)
library(quanteda)


corpus_tidy <- read.csv("data_with_dict_2.csv")

corpus_tidy_text <- as.data.frame(corpus_tidy[, 4])
corpus <- Corpus(DataframeSource(corpus_tidy_text))

#Creating DTM
words <- c("applause", "trump", "cheers", "pennsylvania", "hampshire", 
           "arizona")

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("english"))) 
  corpus <- tm_map(corpus, removeWords, c(words))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers) 
  return(corpus)
}

corpus_clean <- clean_corpus(corpus)

corpus_dtm <- DocumentTermMatrix(corpus_clean) 
dtm_df <- tidy(corpus_dtm)
colnames(dtm_df)[1] <- "title"

dtm_df <- left_join(dtm_df, corpus_tidy[,-2], by = "title")
