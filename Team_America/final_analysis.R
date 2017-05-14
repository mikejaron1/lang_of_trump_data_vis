#Read In Data
data_with_dict_2 <- read.csv("data_with_dict_2.csv")
data <- data_with_dict_2[, c(26:27, 33:38, 24)]
vars <- colnames(data)[1:8]

#Make New Variables
mean_econ <- mean(data_with_dict_2$economy_prop)

data_with_dict_2 <- mutate(data_with_dict_2, econ_above_avg = ((data_with_dict_2$economy_prop - mean_econ) / mean_econ) * 100)

trump_lost <- c("New York", "District of Columbia", "Washington")
trump_won <- c("Pennsylvania", "Michigan", "Wisconsin", "Ohio", "Florida")


data_with_dict_2$trump_won <- ifelse(data_with_dict_2$State %in% trump_lost, "Trump Lost", 
                                     ifelse(data_with_dict_2$State %in% trump_won, "Trump Won Romney Lost", 
                                     "Trump and Romney Won"))

data_with_dict_2 <- data_with_dict_2[-55, ]

#Making the Plot


pal <- c("dodgerblue2", "orangered1", "darkorchid3")

plot_ly(data = data_with_dict_2, x = ~econ_above_avg) %>%
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
