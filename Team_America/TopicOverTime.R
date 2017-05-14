
dat <- read.csv("data/data_with_dict_2.csv")

d <- dat %>% select(State, date, ISIS_prop, economy_prop, healthcare_prop, 
                    Immigration_trade_prop, Washington_prop, 
                    share_voters_voted_trump, share_white, share_unemployed_seasonal,
                    trump_over_romney)

# remove the first speech as it is very early 

d <- d[4:(nrow(d)-1), ]

n <- length(d$date)

topic_data <- c(d$ISIS_prop, 
                d$economy_prop,
                d$healthcare_prop,
                d$Immigration_trade_prop,
                d$Washington_prop)

topics <- c(rep("ISIS", n), rep("Economic Policy", n),
            rep("Healthcare Reform", n), rep("Immigration/Trade", n),
            rep("Washington", n))

date <- rep(d$date, 5)
Trump_vote <- rep(d$share_voters_voted_trump, 5) * 100
White <- rep(d$share_white) * 100
Unemployed <- rep(d$share_unemployed_seasonal, 5) * 100
state <- rep(d$State, 5)

df <- data.frame(date, state, topics, topic_data, Trump_vote, White, Unemployed)

y <- list(
  title = "Proportion of Speech",
  showticklabels = FALSE,
  exponentformat = "E")

x <- list(
  autorange = TRUE,
  title = "Date", 
  type = "date")

pal <- c("dodgerblue2", "orangered1", "darkorchid3", "grey60", "black")

p <- df %>% transform(id = as.integer(factor(topics))) %>%
  plot_ly(y = ~topic_data, x = ~date, 
          color= ~topics, 
          colors = pal,
          hoverinfo = "text",
          text = ~paste('State:', state,'<br>','Trump Vote: ', Trump_vote, '%', 
                        '<br>', 'Unemployment Rate', Unemployed, '%',
                        '<br>', 'White Population', White, '%'),
          yaxis = ~paste0("y", id)) %>%
  add_lines() %>%
  layout(xaxis = x, yaxis = y) %>%
  subplot(nrows = 5, shareX = TRUE) %>%
  layout(legend = list(orientation = 'h'))

