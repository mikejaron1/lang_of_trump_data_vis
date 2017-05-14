polling <- read.csv("data/polling.csv")
data_with_dict_2 <- read.csv("data/data_with_dict_2.csv")

#Make Enddate date
polling$enddate <- as.Date(polling$enddate, "%m/%d/%Y")

#Make Week Row
polling$week <-  format(polling$enddate, format = "%2016-%W")


#Average Polls by week and state

pol <- polling %>% group_by(state, week) %>% summarise(trump_avg = mean(adjpoll_trump), 
                                                       clinton_avg = mean(adjpoll_clinton))



US <- read.csv("data/US.csv")
US$date_week <- as.Date(US$date_week)

polls_avg <- plot_ly(data = US, x = ~ date_week, y = ~ trump_avg, 
                 name = 'Trump Polling Average', type = 'scatter', mode = 'lines',
                 line = list(color = 'rgb(205,55,0)', width = 4)) %>%
  add_trace(y = ~ clinton_avg, name = 'Clinton Polling Average', mode = 'lines', 
            line = list(color = 'rgb(28,134,238)', width = 4)) %>%
  layout(xaxis = list(title = 'Date'),
         yaxis = list(title = 'Polling Average'),
         yaxis = list(zeroline = FALSE, 
                      showline = FALSE),
         xaxis = list(zeroline = FALSE, 
                      showline = FALSE), 
         legend = list(x = 0.80, y = 0.20))
polls_avg

graph <- data_with_dict_2[!is.na(data_with_dict_2$trump_avg),]

poll_topics <- plot_ly(data = graph, x = ~trump_avg) %>%
  add_lines(y = ~fitted(loess(economy_prop ~ trump_avg)),
            line = list(color = 'rgb(154,50,205)', width = 3),
            name = "Economy", showlegend = TRUE) %>%
  add_lines(y = ~fitted(loess(ISIS_prop ~ trump_avg)),
            line = list(color = 'rgb(28,134,238)', width = 3),
            showlegend = TRUE, name = "ISIS / Terrorism") %>%
  add_lines(y = ~fitted(loess(Immigration_trade_prop ~ trump_avg)),
            line = list(color = 'rgb(205,55,0)', width = 3),
            showlegend = TRUE, name = "Immigration") %>%
  layout(xaxis = list(title = 'Trump Polling Averages'),
         yaxis = list(title = 'Proportion of Speech'),
         yaxis = list(zeroline = FALSE, 
                      showline = FALSE),
         xaxis = list(zeroline = FALSE, 
                      showline = FALSE), 
         legend = list(x = 0.80, y = 0.90))
# poll_topic

