#Cosine Similarity 
library(reshape2) 
<<<<<<< Updated upstream
library(lsa)
corpus_tidy$city_date <- paste(corpus_tidy$City, substr(corpus_tidy$date, 6, 11))
states <- corpus_tidy[,c(1, 24)]
=======
states <- corpus_tidy[,c(1,5)]
>>>>>>> Stashed changes

cosine <- DocumentTermMatrix(corpus_clean)

c <- tidy(cosine)
colnames(c)[1] <- "title"
c <- left_join(c, states, by = "title")
c <- c[,-1]

cos_dtm = c %>% cast_dtm(city_date, term, count)
cos_dtm <- t(as.matrix(cos_dtm))
dtms <- removeSparseTerms(cos_dtm, 0.05) # This makes a matrix that is 5% empty space, maximum. '
dtms <- as.matrix(cos_dtm)
cos <- cosine(dtms)

#Find Top 5 Most similar

i <- 1
similar <- data.frame()
similar[1:5, 1] <- NA

for (i in 1:60){
  col<- as.data.frame(cos[i, ])
  col$state <- rownames(col)
  state <- rownames(col)[i]
  colnames(col)[1] <- "Similarity"
  col <- arrange(col, desc(Similarity))
  col <- as.data.frame(col[2:6, 2])
  colnames(col)[1] <- state
  similar <- cbind(similar, col)
}
similar <- similar[, -1]

similar <- t(similar)
similar <- as.data.frame(similar)
similar$city_date <- rownames(similar)
colnames(similar) <- c("1 Similar", "2 Similar", "3 Similar", "4 Similar", "5 Similar", "city_date")

sim <- select(corpus_tidy, city_date, lat, lon)

sim <- left_join(sim, similar, by = "city_date")





