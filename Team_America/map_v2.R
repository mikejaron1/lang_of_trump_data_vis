# library(sp)
# devtools::install_github("hrbrmstr/albersusa")
# library(albersusa)
# library(htmlwidgets)

# prepare data
data.shape <- readOGR(dsn="data/us_shape/")

# screws up color mapping with this
# data.shape = rmapshaper::ms_simplify(data.shape)
## this one just gets the us and moves alaska and hawaii closer
# data.shape = usa_composite()
# data.shape <- rmapshaper::ms_simplify(usa_composite())

map_data <-read.csv("data/map_data.csv")
# speach_loca = read.csv('data/meta_data_with_lat_lon.csv')
data_with_dict_2 = read.csv('data/data_with_dict_2.csv')
speach_loca = subset(data_with_dict_2, lat != 'all', select = c('title', 'date', 'City', 'State', 'lat', 'lon'))
speach_loca$lat = as.numeric(as.character(speach_loca$lat))
speach_loca$lon = as.numeric(as.character(speach_loca$lon))
speach_loca$location = paste(speach_loca$City, speach_loca$State, speach_loca$date, sep=', ')

similar_speeches <- read.csv("data/top_similar.csv")
similar_speeches_sub = subset(similar_speeches, lat != 'all')
similar_speeches_sub$lat = as.numeric(as.character(similar_speeches_sub$lat))
similar_speeches_sub$lon = as.numeric(as.character(similar_speeches_sub$lon))

# write.csv(a, 'new_map_data.csv')
# data1 <-read.csv("data/new_map_data.csv")

# add in marginal and change
map_data$change = map_data$Trump - map_data$Romney
map_data$margin = map_data$Trump - map_data$Clinton


# add it to shape file
# test = subset(data, select=c('STATE_NAME', 'color2016'))
# test1 = merge(data.shape@data, test, by.x='name', by.y='STATE_NAME')
# data.shape@data = test1

data.shape@data = map_data

rownames(data.shape@data) <- data.shape@data$rmapshaperid 

# make a small df of locations names to make it easier to display
df <-data.frame(speach_loca$location, speach_loca$location)

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
  palette = c("dodgerblue2", "darkorchid3",  "orangered1"),
  domain = data.shape@data$marginal
  )
change_pal <- colorNumeric(
  # palette = "YlGnBu",
  palette = "Reds",
  # palette = c("dodgerblue2", "darkorchid3", "orangered1"),
  domain = data.shape@data$change
  )

## add some curvature to the map to make it look nicer
epsg2163 <- leafletCRS(
  crsClass = "L.Proj.CRS",
  code = "EPSG:2163",
  proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
  resolutions = 2^(16:7))

# set basic legend
legend_colors = c('#0040ff', '#ff4000')
# legend_colors = c('dodgerblue2', 'orangered1')
legend_labels = c('Democrat Win', 'Republican Win')

# set opacity for colors
opacity = .7

# prepare most of map
m = leaflet(data.shape, options = leafletOptions(minZoom=3.4, maxZoom = 3.4, crs = epsg2163, 
                                                 zoomControl = FALSE, dragging = FALSE)) %>% 
  setView(lat=38.8282, lng=-98.5795 , zoom=3.4) 
  
  # addEasyButton(easyButton(
  #   icon="fa-crosshairs", title="Locate Me",
  #   onClick=JS("function(btn, map){ map.setView(L.latLng(37.8282, -98.5795), 3); }")))
m



