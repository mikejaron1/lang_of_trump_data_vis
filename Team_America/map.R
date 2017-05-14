# prepare data
data.shape<-readOGR(dsn="data/us_shape/")
data <-read.csv("data/map_data.csv")
# speach_loca = read.csv('data/meta_data_with_lat_lon.csv')

speach_loca = read.csv('data/data_with_dict_2.csv')
speach_loca = subset(speach_loca, select = c('title', 'date', 'City', 'State', 'lat', 'lon'))
speach_loca$location = paste(speach_loca$City, speach_loca$State, speach_loca$date, sep=', ')

# add in marginal and change
data$change = data$Trump - data$Romney
data$margin = data$Trump - data$Clinton

# add it to shape file
data.shape@data = data

# make a small df of locations names to make it easier to display
df<-data.frame(speach_loca$location, speach_loca$location)

# order them alphabetically
df <- df[order(df$speach_loca.location),]

# add in an 'all' option
de<-data.frame("All", "all")
names(de)<-c("speach_loca.location", "speach_loca.location.1")

# make it into 1 dataframe
newdf <- rbind(de, df)
loca_list = as.list(newdf$speach_loca.location.1)
names(loca_list) <- newdf$speach_loca.location

# make color pallette for marginal and change
margin_pal <- colorNumeric(
  palette = c("blue", "yellow", "red"),
  domain = data.shape@data$marginal
)
change_pal <- colorNumeric(
  palette = "YlGnBu",
  domain = data.shape@data$change
)

# set basic legend
legend_colors = c('#0040ff', '#ff4000')
legend_labels = c('Democrat Win', 'Repiblican Win')

# set opacity for colors
opacity = .7

# prepare most of map
m <- leaflet(data.shape) %>%  # Create a map widget
  addTiles(options = tileOptions(minZoom=4)) %>%
  setView(lat=39.8282, lng=-98.5795 , zoom=4) %>%# Add default OpenStreetMap map tiles
  # addMarkers(speach_loca$lon, speach_loca$lat, popup = ~paste(speach_loca$location,"<br>",speach_loca$date)) %>%
  # addLegend(title = "Party Winner", colors = c('#0040ff', '#ff4000'), labels = c('Democrat Win', 'Repiblican Win'),
  # values = data$color2016, opacity = opacity, position="bottomleft") %>%
  addEasyButton(easyButton(
    icon="fa-crosshairs", title="Locate Me",
    onClick=JS("function(btn, map){ map.setView(L.latLng(39.8282, -98.5795), 4); }"))) 
m
