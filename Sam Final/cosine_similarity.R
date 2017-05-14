#Cosine Similarity 
library(reshape2) 
library(lsa)
states <- corpus_tidy[,c(1,2)]
cosine <- DocumentTermMatrix(corpus_clean)

c <- tidy(cosine)
colnames(c)[1] <- "title"
c <- left_join(c, states, by = "title")
c <- c[,-1]

cos_dtm = c %>% cast_dtm(title, term, count)
cos_dtm <- t(as.matrix(cosine))
cos <- cosine(cos_dtm)

#Find Top 5 Most similar

i <- as.data.frame(cos[1,])
similar <- data.frame()
similar[1:5, 1] <- NA

for (i in 1:73){
  col<- as.data.frame(cos[i, ])
  col$state <- rownames(col)
  state <- rownames(col)[1]
  colnames(col)[1] <- "Similarity"
  col <- arrange(col, desc(Similarity))
  col <- as.data.frame(col[2:6, 2])
  colnames(col) <- state
  similar <- cbind(similar, col)
}







cos_melt <- melt(c)
cos_melt$X1 <- as.character(state.abb(cos_melt$X1, state.name))

cos2 <- as.data.frame(cos)
ohio <- as.data.frame(cos2[,5])
colnames(ohio) <- "cosine"
rownames(ohio) <- rownames(cos2)
ohio$State <- rownames(ohio) 
ohio <- ohio[-5,]

ggplot(ohio, aes(reorder(State, cosine), cosine)) + geom_bar(stat = "identity") + coord_flip()

t <- lm()

