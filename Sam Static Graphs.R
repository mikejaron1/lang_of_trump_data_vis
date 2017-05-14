#Static Plots

ggplot(corpus_tidy[-1,], aes(x = date, y = sentiment)) + 
  geom_point( shape = 21, size = 4, fill = "red", 
              stroke = 1.3, alpha = .6) + 
  geom_point( shape = 21, size = 4,
              stroke = 1.3) +
  geom_smooth(se = FALSE) + 
  ggtitle("Speech Sentiment Over Time") +
  theme(plot.title = element_text(hjust = -1, size = 30)) +
  theme_fivethirtyeight() +
  geom_ribbon(aes(ymin = 0, ymax = .8, xmin = as.Date("2016-04-01"),
                  xmax = as.Date("2016-11-14")), 
              fill = "green", alpha = .05, show.legend = NA) +
  geom_ribbon(aes(ymin = -.8, ymax = 0, xmin = as.Date("2016-04-01"),
                  xmax = as.Date("2016-11-14")), 
              fill = "red", alpha = .05, show.legend = NA) +
  scale_x_date(date_breaks = "1 month", 
               labels = waiver())

#Job by Date

dtm_df %>% filter(term == "job") %>% group_by(State)  %>% 
  ggplot(aes(median_household_income, count)) +
  geom_smooth(se = F, method = "lm") + 
  geom_point( shape = 21, size = 4, fill = "red", 
              stroke = 1.3, alpha = .6) + 
  geom_point( shape = 21, size = 4,
              stroke = 1.3) +
  ggtitle("Income vs. Word Jobs") +
  theme_fivethirtyeight()

