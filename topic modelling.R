#LDA Topic Modelling
library(lsa)

words <- c("applause", "trump", "going", "will", "like", "every", "can", "dont", 
           "get", "said", "new", "hillary", "want", "thats", "theyre", "also", 
           "just", "people", "know", "clinton", "must", "right", "now", "say", 
           "plan", "one", "many", "great", "thank", "make", "think", "never", 
           "got", "back", "way", "look", "win", "time", "see", "well", "tell", 
           "good", "take", "state", "even", "ever", "much", "lot", "hes", "really", 
           "youre", "put", "big", "mean", "thing", "come", "year")

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("english"))) 
  corpus <- tm_map(corpus, removeWords, c(words))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stemDocument)
  return(corpus)
}

corpus_clean <- clean_corpus(corpus)
corpus_dtm <- DocumentTermMatrix(corpus_clean) 

set.seed(11)
lda <- LDA(corpus_dtm, 2)
terms(lda, 10)

trump_topics = as.data.frame(posterior(lda, corpus_dtm)$topics)
trump_topics$title <- rownames(trump_topics)
trump_topic <- left_join(trump_topics, states, by = "title")
trump_topic <- trump_topic[,c(1,2,4)]
colnames(trump_topic) <- c("Domestic", "Foreign", "State")

ggplot(trump_topic, aes(Domestic, Foreign)) +   
  geom_jitter( shape = 21, size = 4, fill = "red", stroke = 1.3, alpha = .6,
               width = .1, height = .1) +
  geom_text_repel(data = subset(trump_topic, State == "Ohio"), aes(label = State)) + 
  theme_fivethirtyeight()
  


df.ahca.topics = as.data.frame(ahca.topics)
df.nyt.topics = cbind(email=as.character(rownames(df.ahca.topics)),
                      df.ahca.topics, stringsAsFactors=F)

df.ahca.topics$source <- rownames(df.ahca.topics)

sample(which(df.ahca.topics$"1" > .6))
sample(which(df.ahca.topics$"2" > .6))


topic <- c("Bill", "Insurance", "Bill", "Insurance", "Bill", "Insurance", "Insurance", "Insurance", "Bill", "Bill")

df.ahca.topics$topic <- topic