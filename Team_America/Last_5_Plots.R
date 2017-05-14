# library(ggplot2)
# library(plotly)
# library(ggthemes)

data_with_dict_2 <- read.csv("data/data_with_dict_2.csv")
dict = data_with_dict_2

trump_lost <- c("New York", "District of Columbia", "Washington")
trump_won <- c("Pennsylvania", "Michigan", "Wisconsin", "Ohio", "Florida")


dict$trump_won <- ifelse(dict$State %in% trump_lost, "Trump Lost", 
                                     ifelse(dict$State %in% trump_won, "Trump Won Romney Lost", 
                                            "Trump and Romney Won"))

#Adding in New Variables
dict$mean_econ <- mean(dict$economy_prop)
dict <- mutate(dict, econ_above_avg = ((economy_prop - mean_econ) / mean_econ) * 100)

dict$mean_isis <- mean(dict$ISIS_prop)
dict <- mutate(dict, isis_above_avg = ((ISIS_prop - mean_isis) / mean_isis) * 100)


dict <- dict[-55, ]


# plots for economy -- two sign results

#non white
pal <- c("dodgerblue2", "orangered1", "darkorchid3")

Economy_non_white <- plot_ly(data = dict, x = ~share_non_white) %>%
  add_markers(y = ~economy_prop, color = ~"",
              text = ~paste(State, date, "<br>", "Trump Won This State by", 
                            round(trump_over_romney * 100, digits = 0), "% More Than Romney in 2012", 
                            "<br>", "Trump Spoke About the Economy", round(econ_above_avg, 0), "% More than his Average", 
                            "<br>", "The Non-White Population of this State is", round(share_non_white * 100, 0), "%"), 
              colors = "dodgerblue2", 
              marker = list(size = 15, 
                            line = list(color = 'black',
                                        width = 3))) %>%
  add_lines(y = ~fitted(lm(economy_prop ~ share_non_white)),
            line = list(color = 'orangered3'),
            showlegend = FALSE) %>%
  layout(xaxis = list(title = 'Share of Population in State that is Non-White'),
         yaxis = list(title = 'How Much Trump Spoke About Economy'),
         yaxis = list(zeroline = FALSE, 
                      showline = FALSE),
         xaxis = list(zeroline = FALSE, 
                      showline = FALSE), 
         showlegend = FALSE)



# Plot for ISIS

# Average annual hate crimes per 100,000 population, FBI, 2010-2015

ISIS_hatecrimes <- plot_ly(data = dict, x = ~avg_hatecrimes_per_100k_fbi) %>%
  add_markers(y = ~isis_above_avg, color = ~"",
              text = ~paste(State, date, "<br>", "Trump Won This State by", 
                            round(trump_over_romney * 100, digits = 0), "% More Than Romney in 2012", 
                            "<br>", "Trump Spoke About ISIS", round(isis_above_avg, 0), "% More than his Average", 
                            "<br>", "Hate Crimes Per 100k is", round(avg_hatecrimes_per_100k_fbi, 0)), 
              colors = "orangered3", 
              marker = list(size = 15, 
                            line = list(color = 'black',
                                        width = 3))) %>%
  add_lines(y = ~fitted(lm(isis_above_avg ~ avg_hatecrimes_per_100k_fbi)),
            line = list(color = 'orangered3'),
            showlegend = FALSE) %>%
  layout(xaxis = list(title = 'Hate Crimes Per 100K People'),
         yaxis = list(title = '% Increase of ISIS Topic'),
         yaxis = list(zeroline = FALSE, 
                      showline = FALSE),
         xaxis = list(zeroline = FALSE, 
                      showline = FALSE), 
         showlegend = FALSE)



#hate_crimes_per_100k_splc -- further proves point -- maybe plot the two side by side or leave this one out?

ISIS_hate_crimes_per_100k_splc <- plot_ly(data = dict, x = ~hate_crimes_per_100k_splc) %>%
  add_markers(y = ~isis_above_avg, color = ~"",
              text = ~paste(State, date, "<br>", "Trump Won This State by", 
                            round(trump_over_romney * 100, digits = 0), "% More Than Romney in 2012", 
                            "<br>", "Trump Spoke About the Economy", round(econ_above_avg, 0), "% More than his Average", 
                            "<br>", "The Non-White Population of this State is", round(share_non_white * 100, 0), "%"), 
              colors = "darkorchid3", 
              marker = list(size = 15, 
                            line = list(color = 'black',
                                        width = 3))) %>%
  add_lines(y = ~fitted(lm(isis_above_avg ~ hate_crimes_per_100k_splc)),
            line = list(color = 'orangered3'),
            showlegend = FALSE) %>%
  layout(xaxis = list(title = 'Hate Crimes Per 100K People'),
         yaxis = list(title = '% Increase of ISIS Topic'),
         yaxis = list(zeroline = FALSE, 
                      showline = FALSE),
         xaxis = list(zeroline = FALSE, 
                      showline = FALSE), 
         showlegend = FALSE)

# share voters for trump and share voters for romeny also significant but neg in relation to ISIS topic


# plot for sentiment

#trump over romeny -- only sign variable
Sentiment_trump_OVER_romney <- ggplot(dict, aes(y=sentiment, x=trump_over_romney)) +
  geom_point(shape = 21, fill = "dodgerblue2", size = 3) +
  geom_smooth(method = lm, se = FALSE, color = "orangered3")+
  geom_text(x = -0.01, y = 0.45, label = "Coef = 2.058e+00  P-value = 0.051", color = "dodgerblue2") +
  ggtitle("") +
  theme_minimal() +
  labs(x="Trump over Romney",y="Sentiment")
Sentiment_trump_OVER_romney <- ggplotly(Sentiment_trump_OVER_romney)
Sentiment_trump_OVER_romney # 5th plot

Sentiment_trump_OVER_romney <- plot_ly(data = dict, x = ~trump_over_romney) %>%
  add_markers(y = ~sentiment, color = ~"",
              text = ~paste(State, date, "<br>", "Trump Won This State by", 
                            round(trump_over_romney * 100, digits = 0), "% More Than Romney in 2012", 
                            "<br>", "Trump Spoke About the Economy", round(econ_above_avg, 0), "% More than his Average", 
                            "<br>", "The Non-White Population of this State is", round(share_non_white * 100, 0), "%"), 
              colors = "darkorchid3", 
              marker = list(size = 15, 
                            line = list(color = 'black',
                                        width = 3))) %>%
  add_lines(y = ~fitted(lm(sentiment ~ trump_over_romney)),
            line = list(color = 'orangered3'),
            showlegend = FALSE) %>%
  layout(xaxis = list(title = 'Trump Voteshare Over Romney'),
         yaxis = list(title = 'Sentiment of Speech'),
         yaxis = list(zeroline = FALSE, 
                      showline = FALSE),
         xaxis = list(zeroline = FALSE, 
                      showline = FALSE))




# plots for healthcare -- Dont need but made them anyways incase we wanna use them to further
# prove that speaking of health care inproves the voters in general

healthcare_share_voters_voted_trump <- ggplot(dict, aes(y=healthcare_prop, x=share_voters_voted_trump)) +
  geom_point(shape = 21, fill = "dodgerblue2", size = 3) +
  geom_smooth(method = lm, se = FALSE, color = "orangered3") +
  geom_text(x = 0.2, y = 0.0068, label = "OLS Coef = 4.429571e-03  P-value = 0.036", color = "dodgerblue2") +
  ggtitle("") +
  labs(x="Share of 2016 U.S. presidential voters who voted for Donald Trump",y="Proportion of Healthcare as Topic")
healthcare_share_voters_voted_trump <- ggplotly(healthcare_share_voters_voted_trump)
healthcare_share_voters_voted_trump # if we want extra

healthcare_share_voters_voted_romney <- ggplot(dict, aes(y=healthcare_prop, x=share_voters_voted_romney)) +
  geom_point(shape = 21, fill = "dodgerblue2", size = 3) +
  geom_smooth(method = lm, se = FALSE, color = "orangered3") +
  geom_text(x = 0.2, y = 0.0068, label = "OLS Coef = 4.914754e-03  P-value = 0.03", color = "dodgerblue2") +
  ggtitle("") +
  labs(x="Share of U.S. presidential voters who voted for Romney",y="Proportion of Healthcare as Topic")
healthcare_share_voters_voted_romney <- ggplotly(healthcare_share_voters_voted_romney)
healthcare_share_voters_voted_romney # if we want extra

healthcare_share_voters_voted_romney <- plot_ly(data = dict, x = ~share_voters_voted_trump) %>%
  add_markers(y = ~healthcare_prop, color = ~"",
              text = ~paste(State, date, "<br>", "Trump Won This State by", 
                            round(trump_over_romney * 100, digits = 0), "% More Than Romney in 2012", 
                            "<br>", "Trump Spoke About the Economy", round(econ_above_avg, 0), "% More than his Average", 
                            "<br>", "The Non-White Population of this State is", round(share_non_white * 100, 0), "%"), 
              colors = "dodgerblue2", 
              marker = list(size = 15, 
                            line = list(color = 'black',
                                        width = 3))) %>%
  add_lines(y = ~fitted(lm(healthcare_prop ~ share_voters_voted_trump)),
            line = list(color = 'orangered3'),
            showlegend = FALSE) %>%
  layout(xaxis = list(title = 'Trump Voteshare Over Romney'),
         yaxis = list(title = 'How Much Donald Trump Spoke About Healthcare'),
         yaxis = list(zeroline = FALSE, 
                      showline = FALSE),
         xaxis = list(zeroline = FALSE, 
                      showline = FALSE))



# no plots for washington topic -- no significance

# no plots for immigration -- no significance
