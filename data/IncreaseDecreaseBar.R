dat <- read.csv("data/data_with_dict_2.csv")

d1 <- dat %>% select(trump_over_romney, healthcare_words, ISIS_words, 
                     Immigration_trade_words, Washington_words, economy_words, word_count)

d1$Trump_Vote <- ifelse(d1$trump_over_romney > 0, "Increase", "Decrease")
d1$Trump_Vote <- as.factor(d1$Trump_Vote)

d1_inc <- d1 %>% filter(Trump_Vote == "Increase") %>%
  summarise(Healthcare = sum(healthcare_words),
            Immigration = sum(Immigration_trade_words),
            Washington = sum(Washington_words),
            ISIS = sum(ISIS_words),
            Economy = sum(economy_words),
            Total = sum(word_count))

d1_dec <- d1 %>% filter(Trump_Vote == "Decrease") %>%
  summarise(Healthcare = sum(healthcare_words),
            Immigration = sum(Immigration_trade_words),
            Washington = sum(Washington_words),
            ISIS = sum(ISIS_words),
            Economy = sum(economy_words),
            Total = sum(word_count))

d1 <- rbind(d1_inc, d1_dec)
d1$Healthcare <- d1$Healthcare / d1$Total
d1$Immigration <- d1$Immigration / d1$Total
d1$Washington <- d1$Washington / d1$Total
d1$ISIS <- d1$ISIS / d1$Total
d1$Economy <- d1$Economy / d1$Total

d1 <- d1 %>% select(-Total) 

d1 <- t(d1)
colnames(d1) <- c("Increase", "Decrease")
topic <- rownames(d1)
inc <- rep("Increase", 5)
dec <- rep("Decrease", 5)
d1 <- as.data.frame(d1)

d1_inc <- data.frame(topic, d1$Increase, inc)
d1_dec <- data.frame(topic, d1$Decrease, dec)

colnames(d1_inc) <- c("Topic", "Weight", "Change")
colnames(d1_dec) <- c("Topic", "Weight", "Change")

d2 <- rbind(d1_inc, d1_dec)

g <- ggplot(d2, aes(x = reorder(Topic, Weight), y = Weight))
g <- g + geom_bar(aes(fill = Change), stat="identity", position = "dodge") +
  scale_fill_manual(values = alpha(c("slateblue3", "grey70"))) +
  theme_minimal() +
  xlab("") +
  ylab("Proportion of Speech") +
  coord_flip()

ggsave("~/Documents/Columbia/Data Visualization/final-project-team-america/Team_America/TopicByVoteChange.png", g)

