data_with_dict_2 <- read.csv("data_with_dict_2.csv")
data <- data_with_dict_2[, c(26:27, 33:38, 24)]
vars <- colnames(data)[1:8]

air <- data.frame()
for(k in vars){
  var <- paste("Coef", k, sep = "_")
  form <- as.formula(paste("trump_over_romney", "~", k , collapse=""))
  model <- lm(form, data = data)
  coef <- as.data.frame(coef(summary(model)))
  coef <- coef[-1, ]
  air <- rbind(coef, air)
}

air$topic <- rownames(air)
air$significant <- ifelse(air$`Pr(>|t|)` <= .05, "Yes", "No")

air[1, 5] <- "Washington"
air[2, 5] <- "Immigration and Trade"
air[3, 5] <- "Healthcare"
air[4, 5] <- "Economy"
air[5, 5] <- "ISIS"
air[8, 5] <- "Sentiment"

air <- air[-c(5:6), ]
colnames(air)[2] <- "std_err"



ggplot(air, aes(reorder(topic, Estimate), Estimate, fill = significant)) + geom_bar(stat = "identity") + 
  theme_minimal() + scale_fill_tableau() + 
  labs(x = "Topic", y = "Effect on Increased Voteshare Over Romney", caption = "(Effect Represents Result of OLS Regression)", 
       fill = "Statistically Significant? (P < .05)") +
  theme(legend.position = "bottom", 
        axis.title = element_text(size = 20)) +
  coord_flip()



