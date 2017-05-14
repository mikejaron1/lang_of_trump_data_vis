setwd("/Users/samuelgass/final-project-team-america/data")
library(tm)
library(tidyverse)
library(tidytext)
library(stringr)
library(SnowballC)
library(RTextTools)
library(topicmodels)
library(quanteda)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(reshape)

#Loading Meta Data
meta <- read.csv("data/meta_data_with_lat_lon.csv")
meta$date <- as.Date(meta$date, "%d-%B-%y")
meta$title <- as.character(meta$title)
meta$location <- as.character(meta$location)

meta [62,3] <- "Washington, District of Columbia"
meta [31,3] <- "Washington, District of Columbia"
meta [70,3] <- "Washington, District of Columbia"
meta [68,3] <- "Gettysburg, Pennsylvania"
meta [30,3] <- "Detroit, Michigan"
meta [29,3] <- "New York, New York"
meta [3,3] <- "New York, New York"
meta [10,3] <- "New York, New York"
meta [7,3] <- "New York, New York"
meta [55,3] <- "New York, New York"


meta <- meta[order(as.Date(meta$date)),]
meta <- meta[!duplicated(meta$title),]



#Creating Corpus
corpus <- VCorpus(DirSource(directory = "text"))
corpus_tidy <- as.data.frame(tidy(corpus))
colnames(corpus_tidy)[5] <- "title"
corpus_tidy <- corpus_tidy[,c(5,8)]
sort_data <- meta[,1:2]
corpus_tidy$text <- as.character(corpus_tidy$text)
corpus_tidy$title <- as.character(corpus_tidy$title)
sort_data$title <- as.character(sort_data$title)

corpus_tidy <- as.data.frame(sapply(corpus_tidy, function(x) gsub(".txt", "", x)))
corpus_tidy$title <- as.character(corpus_tidy$title)

corpus_tidy <- left_join(corpus_tidy, sort_data, by = c("title"))
corpus_tidy <- corpus_tidy[order(as.Date(corpus_tidy$date)),]
corpus_tidy_text <- as.data.frame(corpus_tidy[,2])

corpus <- Corpus(DataframeSource(corpus_tidy_text))

#Attaching Meta Data
f
meta(corpus, "id", type = "local") <- meta$title
meta(corpus, "date", type = "local") <- meta$date
meta(corpus, "location", type = "local") <- meta$location

#Full Data Frame w/meta

corpus_tidy <- right_join(corpus_tidy, meta, by = c("title", "date"))


#Add State Data

state_data <- read.csv("State_Data_healthindx_added.csv")
corpus_tidy$location <- as.character(corpus_tidy$location)

corpus_tidy <- separate(data = corpus_tidy, col = location, into = c("City", "State"), sep = ",")
colnames(state_data)[1] <- "State"
state_data$State <- as.character(lapply(state_data$State, function(x) paste("",x, sep=" ")))

corpus_tidy <- left_join(corpus_tidy, state_data, by = c("State"))

corpus_tidy <- mutate(corpus_tidy, trump_over_romney = share_voters_voted_trump - 
                        share_voters_voted_romney)

corpus_tidy <- mutate(corpus_tidy, share_white = 1 - share_non_white)

#Creating DTM
words <- c("applause", "trump", "cheers", "pennsylvania", "hampshire", 
           "arizona")

chunk <- 500
n <- length(myStopwords)
r <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
d <- split(myStopwords,r)

for (i in 1:length(d)) {
  myCorpus <- tm_map(myCorpus, removeWords, c(paste(d[[i]])))
}


clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("english"))) 
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers) 
  return(corpus)
}

corpus_clean <- clean_corpus(corpus)

corpus_dtm <- DocumentTermMatrix(corpus_clean) 
dtm_df <- tidy(corpus_dtm)
colnames(dtm_df)[1] <- "title"

dtm_df <- left_join(dtm_df, corpus_tidy[,-2], by = "title")

#Plotting Some Common Words

t <- dtm_df %>% top_n(n = 20, wt = count) %>% 
  ggplot(aes(term, count)) + geom_bar(stat = "identity") 

dtm_df %>% filter(term == "immigration") %>% group_by(State)  %>% 
  ggplot(aes(avg_hatecrimes_per_100k_fbi, count)) + geom_point() + geom_smooth(method = "lm")

dtm_df %>% filter(median_household_income < 40000) %>% top_n(n = 20, wt = count) %>% 
  ggplot(aes(term, count)) + geom_bar(stat = "identity") + coord_flip()

dtm_df %>% filter(median_household_income > 55000) %>% top_n(n = 20, wt = count) %>% 
  ggplot(aes(term, count)) + geom_bar(stat = "identity") + coord_flip()

#Sentiment

pos <- read.table("positive-words.txt")
neg <- read.table("negative-words.txt")

sentiment <- function(words){
  require(quanteda)
  tok <- quanteda::tokenize(words)
  pos.count <- sum(tok[[1]]%in%pos[,1])
  cat("\n positive words:",tok[[1]][which(tok[[1]]%in%pos[,1])],"\n")
  neg.count <- sum(tok[[1]]%in%neg[,1])
  cat("\n negative words:",tok[[1]][which(tok[[1]]%in%neg[,1])],"\n")
  out <- (pos.count - neg.count)/(pos.count+neg.count)
  return(out)
}

corpus_tidy$text <- as.character(corpus_tidy$text)
corpus_tidy$sentiment <- NA
for (i in 1:nrow(corpus_tidy)){
  corpus_tidy[i,24] <- sentiment(corpus_tidy[i,2])
}

#Plotting Sentiment Examples

corpus_tidy$date <- as.Date(corpus_tidy$date)


ggplot(corpus_tidy[-1,], aes(x = date, y = sentiment)) + 
  geom_point(aes(size = Health.Index), shape = 21, 
             stroke = 1.3, alpha = .6, fill = "red") + 
  geom_smooth(se = FALSE) + 
  theme_fivethirtyeight() +
  scale_color_tableau()

#Mapping

xyz <- corpus_tidy[,c(8,7)]
trump_loc <- SpatialPointsDataFrame(coords = xyz, data = corpus_tidy[,-2], proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) 

sub_stop <- readOGR("/Users/samuelgass/final-project-team-america/data/us shape",
                    "states")

t <- tidy(sub_stop)
t <- t[!t$id == 50,]
x <- t[,1:2]

<<<<<<< Updated upstream
US <- get_map("United States", zoom=11, source="stamen", maptype="toner-background")
t <- ggmap(US)
=======
z <- SpatialPolygonsDataFrame(coords = x, data = t, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) 


tm_shape(sub_stop) + tm_fill() + tm_shape(trump_loc) + tm_dots(col = "red", size = .1)


US <- get_map("United States", zoom=4, source="stamen", maptype="toner-background")

ggmap(US) + geom_point(aes(x = lon, y = lat, col = sentiment), data = corpus_tidy)

>>>>>>> Stashed changes

library(reshape2)
dtm_d <- dtm_df[, 1:3]
t <- reshape(dtm_d, 
             timevar = "term",
             idvar = c("title"),
             direction = "wide")
colnames(t) <- c("t", "w")
