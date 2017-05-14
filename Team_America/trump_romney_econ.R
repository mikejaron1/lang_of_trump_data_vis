#Read In Data
data_with_dict_2 <- read.csv("data/data_with_dict_2.csv")

#Making the Plot

pal <- c("dodgerblue2", "orangered1", "darkorchid3")

vote_econ <- plot_ly(data = data_with_dict_2, x = ~econ_above_avg) %>%
  add_markers(y = ~trump_over_romney, color = ~trump_won,
       text = ~paste(State, date, "<br>", "Trump Won This State by", round(trump_over_romney * 100, digits = 0), "% More Than Romney in 2012", 
                     "<br>", "Trump Spoke About the Economy", round(econ_above_avg, 0), "% More than his Average"), colors = pal, 
       marker = list(size = 15,
                     line = list(color = 'black',
                                 width = 3))) %>%
  add_lines(y = ~fitted(lm(trump_over_romney ~ econ_above_avg)),
            line = list(color = 'red'), 
            showlegend = FALSE) %>%
  layout(xaxis = list(title = 'Percent Increase in Talking About the Economy (Above Average)'),
         yaxis = list(title = 'Trump Voteshare Over Romney'),
         legend = list(x = 0.80, y = 0.90), 
         yaxis = list(zeroline = FALSE, 
                      showline = FALSE),
         xaxis = list(zeroline = FALSE, 
                      showline = FALSE))


#Coefficient Plot
data <- data_with_dict_2[, c(38:42, 28)]
vars <- colnames(data)[1:5]

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
air$significant <- ifelse(air$`Pr(>|t|)` <= .05, "Statistically Significant (P < .05)",
                          "Not Statistically Significant")

air[1, 5] <- "Washington"
air[2, 5] <- "Immigration/Trade"
air[3, 5] <- "Healthcare"
air[4, 5] <- "Economy"
air[5, 5] <- "ISIS"

colnames(air)[2] <- "std_err"


econ_coef <- plot_ly(data = air, x = ~topic, y = ~Estimate, color = ~significant,
        text = ~paste("Trump Voteshare Over Romney ~ Frequency Talking About", topic,
                      "<br>", "P Value:", `Pr(>|t|)`, "<br>", "Std Error:", std_err), 
        colors = c("dodgerblue2", "orangered3")) %>% 
  layout(xaxis = list(title = 'Topic'),
         yaxis = list(title = 'Effect on TrumpVoteshare Increase over Romney (2012)'), 
         legend = list(x = .7, y = 1, bgcolor = 'rgba(255, 255, 255, 0)', bordercolor = 'rgba(255, 255, 255, 0)'))

