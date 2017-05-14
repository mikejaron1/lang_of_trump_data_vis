#Clustering
library(dendextend)
library(ggdendro)
library()

states <- corpus_tidy[,c(1,5)]

cosine <- DocumentTermMatrix(corpus_clean)

c <- tidy(cosine)
colnames(c)[1] <- "title"
c <- left_join(c, states, by = "title")
c <- c[,-1]

cos_dtm = c %>% cast_dtm(State, term, count)

dtm2 <- removeSparseTerms(cosine, sparse = .9)
dtm_m <- as.matrix(dtm2)
dtm_df <- as.data.frame(dtm_m)
texts_dist <- dist(dtm_df)
hc <- hclust(texts_dist)

hcd <- as.dendrogram(hc)

ddata <- dendro_data(hcd, type = "triangle")
ggplot(segment(ddata)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  coord_flip() + 
  scale_y_reverse(expand = c(0.2, 0)) +
  theme_dendro() +
  geom_text(data = ddata$labels, 
            aes(x = x, y = y, label = label), size = 3, hjust = -.1) 