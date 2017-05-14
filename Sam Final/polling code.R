polling <- read.csv("data/polling.csv")
data_with_dict_2 <- read.csv("data/data_with_dict_2.csv")

#Make Enddate date
polling$enddate <- as.Date(polling$enddate, "%m/%d/%Y")

#Make Week Row
polling$week <-  format(polling$enddate, format = "%2016-%W")


#Average Polls by week and state

pol <- polling %>% group_by(state, week) %>% summarise(trump_avg = mean(adjpoll_trump), 
                                                       clinton_avg = mean(adjpoll_clinton))

#Make Week Variable for Data
data_with_dict_2$date <- as.Date(data_with_dict_2$date)
data_with_dict_2$week <- format(data_with_dict_2$date, format = "2016-%W")

#Basic Data Frame for Merging
find <- select(data_with_dict_2, week, State, City)
find$weeks <- as.numeric(as.character(substr(find$week,6,7)))

pol$weeks <- as.numeric(as.character(substr(pol$week,6,7)))

#Lets Try this shit

i <- find[1,]
air <- data.frame()

for (i in 1:73){
  #state
  i <- find[i, ]
  stat <- paste(i$State)
  dat <- filter(pol, state == stat)
  
  #Find Closest Weeks
  x <- i$weeks - dat$weeks
  week_before <- i$weeks - min(x[x > 0])
  week_after  <- i$weeks - max(x[x < 0])
  
  #Filter by week
  before <- filter(dat, weeks == week_before)
  after <- filter(dat, weeks == week_after)
  
  change <- as.data.frame(after$trump_avg - before$trump_avg)
  change[1,2] <- i$State
  change[1,3] <- i$week
  change[1,4] <- i$weeks
  change[1, 5] <- i$City
  
  air <- rbind(air, change)
}

#Merge Data Sets

pol <- read.csv("data/poll_change.csv")


#polls over time
library(lubridate)
library(stringr)
US <- filter(polling, state == "U.S.")
US <- US %>% group_by(state, week) %>% summarise(trump_avg = mean(adjpoll_trump), 
                                                clinton_avg = mean(adjpoll_clinton))

US$weeks <- as.character(substr(US$week, 6, 7))
US$year <- as.numeric(as.character(substr(US$week, 1, 4)))
US$date_week <- as.Date(paste("1", US$weeks, US$year, sep = "-"), format = "%w-%W-%Y")
US <- US[1:47, ]

US <- US[-1, ]

polls <- plot_ly(data = US, x = ~ date_week, y = ~ trump_avg, 
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

#Merge US and change dfs
colnames(air) <- c("State_Change", "State", "week", "weeks", "City")
pollster <- left_join(air, US, by = "week")
colnames(pollster)[1:4] <- c("change", "State", "week", "week_num")

data_with_dict_2 <- left_join(data_with_dict_2, pollster, by = c("week", "State", "City"))
data_with_dict_2$weeks.y <- as.numeric(data_with_dict_2$weeks)

#Add US Change variable

us_ch <- data.frame()
for (i in 1:73){
  #state
  k <- i
  i <- data_with_dict_2[i, ]
  x <- i$week_num - data_with_dict_2[, 46]
  x <- na.omit(x)
  #Find Closest Weeks
  week_before <- i$week_num - min(x[x > 0])
  week_after  <- i$week_num - max(x[x < 0])
  
  #Filter by week
  before <- filter(data_with_dict_2, week_num == week_before)
  after <- filter(data_with_dict_2, week_num == week_after)
  
  us_change <- as.data.frame(after[1, 48] - before[1, 48])
  
  us_ch <- rbind(us_ch, us_change)
}

data_with_dict_2 <- cbind(data_with_dict_2, us_ch)
colnames(data_with_dict_2)[53] <- "Change_US"

graph <- data_with_dict_2[!is.na(data_with_dict_2$trump_avg),]

add_lines(y = ~fitted(loess(mpg ~ disp)),
          line = list(color = '#07A4B5'),
          name = "Loess Smoother", showlegend = TRUE)


poll_topic <- plot_ly(data = graph, x = ~trump_avg) %>%
  add_lines(y = ~fitted(loess(economy_prop ~ trump_avg)),
            line = list(color = 'rgb(205,55,0)', width = 3),
            name = "Economy", showlegend = TRUE) %>%
  add_lines(y = ~fitted(loess(ISIS_prop ~ trump_avg)),
            line = list(color = 'rgb(28,134,238)', width = 3),
            showlegend = TRUE, name = "ISIS / Terrorism") %>%
  layout(xaxis = list(title = 'Trump Polling Averages'),
         yaxis = list(title = 'Proportion of Speech'),
         yaxis = list(zeroline = FALSE, 
                      showline = FALSE),
         xaxis = list(zeroline = FALSE, 
                      showline = FALSE))
