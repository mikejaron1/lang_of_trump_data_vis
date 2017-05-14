#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# to get the app started
# library(rsconnect)
# rsconnect::deployApp("/Users/mikejaron/github/DataViz/final-project-team-america/Team_America")

library(shiny)
library(markdown)
library(rgdal)
library(dplyr)
library(leaflet)
library(plotly)
library(ggplot2)
library(ggthemes)
library(tibble)

# only for testing, make sure to comment out before sending to site
# setwd('/Users/mikejaron/github/DataViz/final-project-team-america/Team_America/')

# data, import before runnign scripts
# data_with_dict_2 = read.csv('data/data_with_dict_2.csv')
# similar_speeches <- read.csv("data/top_similar.csv")
top_words_speeches <- read.csv("data/top_words_speech.csv")

source("TopicOverTime.R")
source("map_v2.R")
source("MakeBarChart.R")
source("Last_5_Plots.R")
source("IncreaseDecreaseBar.R")
source("trump_romney_econ.R")
source("polling_code.R")


## add location to both dataframes for drop down list
top_words_speeches = merge(top_words_speeches, subset(speach_loca, select=c('title', 'location'), by='title'))
similar_speeches_sub = merge(similar_speeches_sub, subset(speach_loca, select=c('location', 'lat')), by='lat', 
                         all.x = FALSE)
similar_speeches_sub = similar_speeches_sub %>% distinct(location, .keep_all = TRUE)
  
# all of the rows are of total lenghth 12

ui <- fluidPage(theme = "bootstrap.css",                     
  fluidRow(column(h3("Language and the 2016 US Presidential Election"), br(), 
                  h4("Mike Jaron, Sam Gass, Jack Holder & Brandon Wolff"),
                  align='center', width=8, offset = 2
                  )
           ),
  fluidRow(
           column(width=6, offset=3, align='center', 
        br(), p("If there is any lesson to take out of the 2016 US Presidential Election, it is that messaging and language
                continue to dictate election outcomes despite campaigns' best efforts to utilize data to win races. Perhaps the 
                most underestimated aspect of Donald Trump's political prowess is his ability to connect with the electorate through
                his use of language. This project utilizes text analysis and natural language processing in an attempt to delineate 
                the methodology behind Donald Trump's use of language during the 2016 US Presidential Election as well as the strategies 
                he used that lead to a winning electoral outcome. "), br(),  
        p("The first rule of messaging is to know your audience. To assume that Donald Trump's messaging was uniform across 
        geographic regions and audience is to underestimate the complexity of his strategy. This map is an exploratory
        visualization that allows you to observe the characteristics of campaign speeches. Each point on the map represents
        an individual campaign speech delivered during the 2016 Presidential Election."), br(), 
        p("You can interact with the map to find
        out information about Trump's speeches during his 2016 campaign. Feel free to use the drop down lists  
        below or click on markers in the map, and hover over states, as you do that the info in the three boxes 
          below will change to your selection."
        )
           )
  ),
  
  fluidRow(
    column(width = 3, offset = 3,
               uiOutput("year"),
              selectInput("years",
                            "Election Year & Win Change/Margin",
                            c('2016' = '2016', 
                              '2012' = '2012', 
                              'Change (Trump/Romney)' = 'Change', 
                              'Margin (Trump/Clinton)' = 'Margin'),
                            selected=c('2016')
               )
           ),
    column(width=3,
            uiOutput("location"),
            selectInput("locations", "Speech Locations",
                           choices = loca_list,
                           selected = 'all'
               )
           )
    ),
  
  fluidRow( 
    column(width = 10, offset = 1,
           # uiOutput("leaf")
            leafletOutput("map", height = 550)
    )
  ),
  
  fluidRow(
    column(width = 2, offset = 2, align='center', style='padding:0px;',
        h4("Top Words"),
        tableOutput("topWords")
    ),
    column(width = 2, align='center', style='padding:0px;',
      h4("Similar Speeches"),
      tableOutput("similar")
    ),
    column(width = 4, align='center', style='padding:0px;',
      uiOutput("topics"),
      h4("Top Topics"),
      plotOutput("barTopics", height = 210)
    )
  ),
  
  # new row
  fluidRow(
    column(width = 6, offset=3,
        br(), p("While it is clear that Washington was one of his primary talking points, it is interesting,
                although maybe not surprising, that immigration and trade were more prominent in speeches closer
                to the border, most notably in Houston and Phoenix."), align='center')
  ),
    
  fluidRow(
    column(width = 6, offset = 3, align='center',
        br(), p("Below we show how the main topics discussed by Donald Trump changed over time.")
        )
  ),
  
  fluidRow(
    column(width = 6, offset = 3, align='center',
      br(), h4("Topics Over Time")
    )
  ),
  
  fluidRow(
    column(width = 8, offset = 2, align='center',
        plotlyOutput("topicsOverTime")
        )
  ),
  
  fluidRow(
    column(width = 6, offset = 3, align='center',
        br(), p("As can be seen, Healthcare became a more prominent topic in the latter stages of the campaign, possibly due 
           to Trump's attempt to appease the Republican establishment post nomination. Next we look at the most talked 
           about topics in states where Trump's vote increased and decreased vs. Mitt Romney.")
        )
  ),

  fluidRow(
    column(width = 8, offset = 2, align = 'center',
      br(), h4("Topics by Change in Republican Vote (2012-16)"), 
        plotlyOutput("IncreaseDecreaseBar")
    )
  ),
  
  fluidRow(
    column(width = 8, offset = 2, align='center',
           br(), h2("Topics vs Area Characteristics"))
  ),
  fluidRow(
    column(width = 6, offset=3, align='center',
           br(), br(), p("This section explores the relationship between the topics 
                         focused on in each speech and the characteristics of the state
                         the speech was given in. This analysis reveals some of the 
                         strategic element of campaign rallys. In our exploration we found
                         two distinct significant relationships between speech topics and 
                         state characteristics. These relationships give some ingsight into 
                         the stratic nature of these campaign stops and what determines the 
                         focus of each speech."))
  ),
  fluidRow(
    column(width = 6, offset = 3, align='center', br(), h4("Non-White Population vs. Prominence of the Economy in Speeches"),
           br(), br(), p("The most significant correlation found was between the non-white 
                         population in the state the speech was given in and the prominence of 
                         economic talking points in the spech. It appears that in states with higher
                         white populations, Trump focused significantly more on economic issues. 
                         This suggests that his focus on economic talking points as a whole relied
                         heavily on the white populations in each respective state."))
           ),
  fluidRow(
    column(width = 8, offset = 2, align='center',
      br(), 
      plotlyOutput("Economy_non_white"))
  ),
  fluidRow(
    column(width = 6, offset = 3, align='center', br(), br(), h4("Mention of ISIS in Speeches vs. Hate Crimes Committed 2010-2015"),
           br(), br(), p("The other significant relationship we found was between hate crimes
                         in each state (in 2015) and the focus on ISIS/Terrorism in the speech. 
                         In this context we are using hate crimes as a proxy for racial tension. 
                         So in states with higher racial tensions we find that Trump tended to 
                         focus more on the threat of ISIS and Terrorism in general."))
           ),
  fluidRow(
    column(width = 8, offset=2, align='center', 
      br(),  
      plotlyOutput("ISIS_hatecrimes")
      )
  ),
  fluidRow(
    column(width = 6, offset=3, align='center',
      br(), br(), h4("Mention of ISIS in Speeches vs. Hate Crimes Post Election (Nov 9-18 2016)"), 
      br(), br(), p("The same relationship holds when looking at racial tension in the week
                   following the election, most likely due to the correlation between the two.")  
    )
  ),
  fluidRow(
    column(width = 8, offset=2, align='center',
           plotlyOutput("ISIS_hate_crimes_per_100k_splc"))
    
  ),
  fluidRow(
    column(width = 8, offset=2, align='center',
           br(), h2("Polling and Topic Focus"))
    
  ),
  fluidRow(
    column(width = 6, offset=3, align='center',
           br(), p("Real time polling can influence campaign strategy by the day. In order
                   to explore the relationship between stump speech language and polling, 
                   we averaged out available polls by week and compared the language
                   used in Trump campaign speeches to the polling averages in that week."))
           ),
  fluidRow(
    column(width = 6, offset=3, align='center',
           br(), p("The following plot indicates the weekly polling averages calculated in this 
                   analysis."))
           ),
  fluidRow(
    column(width = 8, offset=2, align='center',
           plotlyOutput("poll_avg"))
  ),
  fluidRow(
    column(width = 6, offset=3, align='center',
           br(), p("In the course of this analysis, we found three significant relationships
                   between the weekly polling averages and topic selection. The following plot
                   indicates that as polling numbers increase Donald Trump tended to speak more
                   about the economy and less about ISIS and immigration. We view this as revealing 
                   that the campaign strategy focused on more controversial and emotional topics
                   as polling numbers fell and more stable, reasonable topics when polls were more 
                   favorable."))
           ),
  fluidRow(
    column(width = 8, offset=2, align='center',
           br(), h4("Topics By Polling"),
           plotlyOutput("poll_topic"))
),
    fluidRow(
      column(width = 8, offset=2, align='center',
             br(), h2("Topics and Increases in Voteshare"),
             h4("Effect of Topics on Voteshare Increase Over Romney (2012)"))
    ),
    fluidRow(
      column(width = 6, offset=3, align='center',
             br(), p("The primary analysis in this project focused on the characteristics
                     of Trump's campaign speeches that assisted him the most in winning
                     the 2016 Presidential election. In this conclusionary section we 
                     analyze the topics which, when focused on more, increased Donal Trump's
                     voteshare in that state over Mitt Romney in 2012."))
    ),
    fluidRow(
      column(width = 6, offset=3, align='center', h4("Effect of Different Topics on Voteshare Increase Over"),
             br(), p("The following plot represents the coefficients of individual OLS 
                     regressions where the dependent variable was voteshare of Trump against
                     Romney in the state and the independent variables were each calculated
                     topic separately. The indicated results reveal that, of the topics, the 
                     economy was the only topic with a statistically significant effect on 
                     the increased voteshare."))
  ),
  fluidRow(
    column(width = 8, offset=2, align='center',
           br(),
           plotlyOutput("econ_coef"))
  ),
fluidRow(
  column(width = 6, offset=3, align='center', h4("Effect of Different Topics on Voteshare Increase Over"),
         br(), p("If we observe this relationship in the aggregate, it is apparent that
                 the more Donald Trump spoke about the economy, the better his increase
                 in voteshare over Romney in that state. In this analysis, we assume that
                 the language in his campaign speech reflects, at least in part, 
                 the communication strategy for that state as a whole. In turn, we observe that
                 the more the campaign focused on the economy in a state, the greater their 
                 increased voteshare in that state over Mitt Romney in 2012."))
         ),
  fluidRow(
    column(width = 8, offset=2, align='center',
      br(),
      plotlyOutput("vote_econ"))
  ),
  fluidRow(
    column(width = 6, offset=3, align='center',
      br(), h4("Conclusion")
    )
  ),
  fluidRow(
    column(width = 6, offset=3, align='center',
        p("This project sought to highlight the characteristics of Donald Trump's language
          that may have led to his improbable electoral victory. In the course of this pursuit, 
          we also found interesting patterns that may delineate some of the rhetorical strategy 
          of the campaign. 
          
          Our primary finding indicates that it was Donald Trump's focus on economic issues
          that ultimately contributed to his electoral victory. We believe this study warrants
          further exploration into the dynamic between Donald Trump's economic talking 
          points and his ultimate electoral victory in the 2016 US Presidential Election."), br(), br()

        )
  )
)

               


# Define server logic required to draw a histogram
server <- function(input, output) {
  # output$leaf = renderUI({
  #   leafletOutput('map', width = "90%", height = 500)
  # })
  # render the plots
  output$map = renderLeaflet(m)
  output$topicsOverTime = renderPlotly(p)
  output$IncreaseDecreaseBar = renderPlotly(g)
  output$Economy_non_white = renderPlotly(Economy_non_white)
  output$Economy_trump_OVER_romney = renderPlotly(Economy_trump_OVER_romney)
  output$ISIS_hatecrimes = renderPlotly(ISIS_hatecrimes)
  output$ISIS_hate_crimes_per_100k_splc = renderPlotly(ISIS_hate_crimes_per_100k_splc)
  output$Sentiment_trump_OVER_romney = renderPlotly(Sentiment_trump_OVER_romney)
  output$vote_econ = renderPlotly(vote_econ)
  output$econ_coef = renderPlotly(econ_coef)
  output$poll_topic = renderPlotly(poll_topics)
  output$poll_avg = renderPlotly(polls_avg)
  
  
  # make them interactive if neccessary
  observe({
    colorBy <- input$years
      if(colorBy == '2016'){
        leafletProxy("map") %>% clearShapes() %>% clearControls() %>%
          addPolygons(data=data.shape, group=colorBy, fillColor=data.shape@data$color2016, 
                      fillOpacity = opacity, stroke=TRUE, color='white', weight=1.5,
                      highlight = highlightOptions(weight = 5,
                                                   color = "#666",
                                                   dashArray = "",
                                                   fillOpacity = 0.7,
                                                   bringToFront = TRUE),
                      label = data.shape@data$STATE_NAME,
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto")) %>%
          addLegend(title = "Party Winner", colors = legend_colors, labels = legend_labels,
                    values = data$color2016, opacity = opacity, position="bottomleft")
        
      }else if(colorBy == '2012'){
        leafletProxy("map") %>% clearShapes() %>% clearControls() %>%
          addPolygons(data=data.shape, group=colorBy, fillColor=data.shape@data$color2012, 
                      fillOpacity = opacity, stroke=TRUE, color='white', weight=1.5,
                      highlight = highlightOptions(weight = 5,
                                                   color = "#666",
                                                   dashArray = "",
                                                   fillOpacity = 0.7,
                                                   bringToFront = TRUE),
                      label = data.shape@data$STATE_NAME,
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto")) %>%
          addLegend(title = "Party Winner", colors = legend_colors, labels = legend_labels,
                    values = data$color2016, opacity = opacity, position="bottomleft")
        
      }else if(colorBy == 'Change'){
        labels <- sprintf(
          "<strong>%s</strong><br/>%g%% Change",
          data.shape@data$STATE_NAME, round(data.shape@data$change,2)
        ) %>% lapply(htmltools::HTML)
        
        leafletProxy("map") %>% clearShapes() %>% clearControls() %>%
          addPolygons(data=data.shape, group=colorBy, fillColor=~change_pal(data.shape@data$change), 
                      fillOpacity = opacity, stroke=TRUE, color='white', weight=1.5,
                      highlight = highlightOptions(weight = 5,
                                                   color = "#666",
                                                   dashArray = "",
                                                   fillOpacity = 0.7,
                                                   bringToFront = TRUE),
                      label = labels,
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto")) %>%
          addLegend(title = "Trump/Romney<br>Change", pal = change_pal, values = na.omit(data.shape@data$change),
                    opacity = opacity, position="bottomleft", labFormat = labelFormat(suffix = "%")
          )
        
      }else if(colorBy == 'Margin'){
        labels <- sprintf(
          "<strong>%s</strong><br/>%g%% Margin",
          data.shape@data$STATE_NAME, round(data.shape@data$margin,2)
        ) %>% lapply(htmltools::HTML)
        
        leafletProxy("map") %>% clearShapes() %>% clearControls() %>%
          addPolygons(data=data.shape, group=colorBy, fillColor=~margin_pal(data.shape@data$margin), 
                      fillOpacity = opacity, stroke=TRUE, color='white', weight=1.5,
                      highlight = highlightOptions(weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                        label = labels,
                        labelOptions = labelOptions(
                          style = list("font-weight" = "normal", padding = "3px 8px"),
                          textsize = "15px",
                          direction = "auto")) %>%
          addLegend(title = "Trump/Clintion<br>Margin", pal = margin_pal, values = na.omit(data.shape@data$margin),
                    opacity = opacity, position="bottomleft", labFormat = labelFormat(suffix = "%")
                    )
        
      }else{
      leafletProxy("map") %>% clearShapes() %>% clearControls()
    }
  })

  # subsetting by location
  observe({
    local <- input$locations
    print(local)
    sim_df = subset(similar_speeches_sub, location == local, 
                    select =c(X1.Similar, X2.Similar, X3.Similar, X4.Similar, X5.Similar, lon, lat, city_date))[1,]
    if(local == 'all'){
      bar_lat = "'all'"
      leafletProxy("map", data=speach_loca) %>% clearMarkers() %>%
        addAwesomeMarkers(speach_loca$lon, speach_loca$lat, label = ~speach_loca$location,
                          options=markerOptions(riseOnHover = TRUE, opacity=.8),
                          icon=awesomeIcons( icon = 'ion-record', iconColor = 'white', library = 'ion', 
                                             markerColor = 'blue'))    
      output$topWords = renderTable(c('Choose a Speech'), colnames = FALSE)
      output$similar = renderTable(c('Choose a Speech'), colnames = FALSE)
      }else{
      
        # get the data for the location
      local_ll = subset(speach_loca, location == local, select = c(lon, lat, date))[1,]
      bar_lat = local_ll$lat
      
      if(rowSums(is.na(sim_df)) == 0){
      loc1 = subset(similar_speeches_sub, city_date == as.character(sim_df$X1.Similar), select = c(lon, lat, city_date))
      loc2 = subset(similar_speeches_sub, city_date == as.character(sim_df$X2.Similar), select = c(lon, lat, city_date))
      loc3 = subset(similar_speeches_sub, city_date == as.character(sim_df$X3.Similar), select = c(lon, lat, city_date))
      loc4 = subset(similar_speeches_sub, city_date == as.character(sim_df$X4.Similar), select = c(lon, lat, city_date))
      loc5 = subset(similar_speeches_sub, city_date == as.character(sim_df$X5.Similar), select = c(lon, lat, city_date))
      
      ## needed to make if statements b/c of some weirdness when subsetting by 'Portsmouth 10-15'
      make_map = leafletProxy("map", data=speach_loca) %>% clearMarkers() %>%
        addAwesomeMarkers(speach_loca$lon, speach_loca$lat, label = ~speach_loca$location,
                          options=markerOptions(riseOnHover = TRUE, opacity=.15),
                          icon=awesomeIcons(icon = 'ion-record', iconColor = 'white', library = 'ion',
                                             markerColor = 'blue')) 
      if(nrow(sim_df) > 0){make_map = make_map %>%
        addAwesomeMarkers(sim_df$lon, sim_df$lat, label = sim_df$city_date, 
                          options=markerOptions(riseOnHover = TRUE),
                          icon=awesomeIcons( icon = 'ion-record', iconColor = 'white', library = 'ion', 
                                             markerColor = 'blue'))
      }
      if(nrow(loc1) >0){ make_map = make_map %>%
        addAwesomeMarkers(loc1$lon, loc1$lat, label=loc1$city_date,
                          icon=awesomeIcons(icon = 'ion-record', iconColor = 'white', library = 'ion', 
                                            markerColor = 'green')) 
      }
      if(nrow(loc2) >0){ make_map = make_map %>%
        addAwesomeMarkers(loc2$lon, loc2$lat, label=loc2$city_date,
                          icon=awesomeIcons(icon = 'ion-record', iconColor = 'white', library='ion',
                                            markerColor = 'green'))
      }
      if(nrow(loc3) >0){ make_map = make_map %>%
        addAwesomeMarkers(loc3$lon, loc3$lat, label=loc3$city_date,
                          icon=awesomeIcons(icon = 'ion-record', iconColor = 'white', library = 'ion',
                                            markerColor = 'green'))
      }
      if(nrow(loc4) >0){ make_map = make_map %>%
        addAwesomeMarkers(loc4$lon, loc4$lat, label=loc4$city_date,
                          icon=awesomeIcons(icon = 'ion-record', iconColor = 'white', library = 'ion', 
                                            markerColor = 'green'))
      }
      if(nrow(loc5) >0){ make_map = make_map %>%
        addAwesomeMarkers(loc5$lon, loc5$lat, label=loc5$city_date,
                          icon=awesomeIcons(icon = 'ion-record', iconColor = 'white', library = 'ion', 
                                            markerColor = 'green'))
        }
      }
      text_df = subset(top_words_speeches, location == local, 
                       select =c(Word_1, Word_2, Word_3, Word_4, Word_5))[1,]
      
      output$topWords = renderTable(c(as.character(text_df$Word_1), as.character(text_df$Word_2), 
                                      as.character(text_df$Word_3), as.character(text_df$Word_4),
                                      as.character(text_df$Word_5)), colnames = FALSE
      )
      output$similar = renderTable(c(as.character(sim_df$X1.Similar), as.character(sim_df$X2.Similar), 
                                     as.character(sim_df$X3.Similar), as.character(sim_df$X4.Similar),
                                     as.character(sim_df$X5.Similar)), colnames = FALSE
      )
      }
    output$barTopics = renderPlot(makebar(bar_lat))
  })
  
  # can do it by clicks as well
  observe({
    mapClick = input$map_marker_click
    print(mapClick)
    ## only change tables and chart if the map has been clicked
    if(length(mapClick$lat) != 0){
      output$barTopics = renderPlot(makebar(mapClick$lat))
      ## just bringing back first row in case of duplicates
      text_df = subset(top_words_speeches, lat == mapClick$lat, 
                       select =c(Word_1, Word_2, Word_3, Word_4, Word_5))[1,]
      
      output$topWords = renderTable(c(as.character(text_df$Word_1), as.character(text_df$Word_2), 
                                      as.character(text_df$Word_3), as.character(text_df$Word_4),
                                      as.character(text_df$Word_5)), colnames = FALSE
        )
      ## for some reason i cant subset by exact lat
      a = mapClick$lat + .000001
      b = mapClick$lat - .000001
      sim_df = subset(similar_speeches_sub, (lat > b & lat < a),
                      select =c(X1.Similar, X2.Similar, X3.Similar, X4.Similar, X5.Similar, city_date, lat, lon))[1,]
      output$similar = renderTable(c(as.character(sim_df$X1.Similar), as.character(sim_df$X2.Similar), 
                                     as.character(sim_df$X3.Similar), as.character(sim_df$X4.Similar),
                                     as.character(sim_df$X5.Similar)), colnames = FALSE
          )
      if(rowSums(is.na(sim_df)) == 0){
      
      loc1 = subset(similar_speeches_sub, city_date == as.character(sim_df$X1.Similar), select = c(lon, lat, city_date))
      loc2 = subset(similar_speeches_sub, city_date == as.character(sim_df$X2.Similar), select = c(lon, lat, city_date))
      loc3 = subset(similar_speeches_sub, city_date == as.character(sim_df$X3.Similar), select = c(lon, lat, city_date))
      loc4 = subset(similar_speeches_sub, city_date == as.character(sim_df$X4.Similar), select = c(lon, lat, city_date))
      loc5 = subset(similar_speeches_sub, city_date == as.character(sim_df$X5.Similar), select = c(lon, lat, city_date))

      make_map = leafletProxy("map") %>% clearMarkers() %>%
        addAwesomeMarkers(speach_loca$lon, speach_loca$lat, label = speach_loca$location,
                          options=markerOptions(riseOnHover = TRUE, opacity=.15),
                          icon=awesomeIcons( icon = 'ion-record', iconColor = 'white', library = 'ion', 
                                             markerColor = 'blue'))
      if(nrow(sim_df) > 0){make_map = make_map %>%
        addAwesomeMarkers(sim_df$lon, sim_df$lat, label = sim_df$city_date,
                          options=markerOptions(riseOnHover = TRUE),
                          icon=awesomeIcons(icon = 'ion-record', iconColor = 'white', library = 'ion', 
                                             markerColor = 'blue'))
      }
      if(nrow(loc1) > 0){make_map = make_map %>%
        addAwesomeMarkers(loc1$lon, loc1$lat, label=loc1$city_date,
                          icon=awesomeIcons(icon = 'ion-record', iconColor = 'white', library = 'ion', 
                                            markerColor = 'green'))
      }
      if(nrow(loc2) > 0){make_map = make_map %>%
        addAwesomeMarkers(loc2$lon, loc2$lat, label=loc2$city_date,
                          icon=awesomeIcons(icon = 'ion-record', iconColor = 'white', library='ion',
                                            markerColor = 'green'))
      }
      if(nrow(loc3) > 0){make_map = make_map %>%
        addAwesomeMarkers(loc3$lon, loc3$lat, label=loc3$city_date,
                          icon=awesomeIcons(icon = 'ion-record', iconColor = 'white', library = 'ion',
                                            markerColor = 'green'))
      }
      if(nrow(loc4) > 0){make_map = make_map %>%
        addAwesomeMarkers(loc4$lon, loc4$lat, label=loc4$city_date,
                          icon=awesomeIcons(icon = 'ion-record', iconColor = 'white', library = 'ion', 
                                            markerColor = 'green'))
      }
      if(nrow(loc5) > 0){make_map = make_map %>%
        addAwesomeMarkers(loc5$lon, loc5$lat, label=loc5$city_date, 
                          icon=awesomeIcons(icon = 'ion-record', iconColor = 'white', library = 'ion', 
                                            markerColor = 'green'))
      }
      }
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

