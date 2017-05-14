library(dplyr)

data_with_dict_2 <- read.csv('data/data_with_dict_2.csv')
# example of X for it to work
X = "'all'"

makebar <- function(X) {
  cond <- paste("lat", "==",  X)
  print(cond)
  d <- data_with_dict_2 %>% filter_(cond) %>% select(economy_prop,  
                                             Immigration_trade_prop, ISIS_prop,
                                             healthcare_prop, Washington_prop)
  colnames(d) <- c("Economic Policy", "Immigration/Trade", "ISIS", "Healthcare Reform",
                 "Washington")
  d <- t(d)
  d <- as.data.frame(d)
  d <- rownames_to_column(d)
  colnames(d) <- c("Topic", "Proportion")
  d$Proportion <- as.numeric(as.character(d$Proportion))
  d$Proportion <- round(d$Proportion, digits = 4)
  ggplot(d, aes(x = reorder(Topic, Proportion), y = Proportion)) + 
    geom_bar(fill = "dodgerblue2", stat = "identity", width = 0.5) +
    xlab("") + ylab("") +
    coord_flip() + theme_minimal()
}

makebar(X)
