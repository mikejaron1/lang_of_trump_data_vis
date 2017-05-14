library(sp)
# devtools::install_github("hrbrmstr/albersusa")
library(albersusa)

# prepare data
# data.shape<-readOGR(dsn="data/us_shape/")
## this one just gets the us and moves alaska and hawaii closer
data.shape <- rmapshaper::ms_simplify(usa_composite())

data <-read.csv("data/map_data.csv")
speach_loca = read.csv('data/meta_data_with_lat_lon.csv')

# add in marginal and change
data$change = data$Trump - data$Romney
data$margin = data$Trump - data$Clinton

# add it to shape file
test = subset(data, select=c('STATE_NAME', 'color2016'))
test1 = merge(data.shape@data, test, by.x='name', by.y='STATE_NAME')
data.shape@data = test1

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
legend_labels = c('Democrat Win', 'Repiblican Win')

# set opacity for colors
opacity = .7

colorBy = '2016'

# prepare most of map
m <- leaflet(data.shape, options = leafletOptions(minZoom=2.5, maxZoom = 7, crs = epsg2163)) %>%
  addTiles(options = tileOptions(minZoom=4)) %>%
  setView(lat=37.8282, lng=-98.5795 , zoom=3) %>%
  addPolygons(data=data.shape, group=colorBy, fillColor= data.shape@data$color2016,
              fillOpacity = opacity, stroke=TRUE, color='white', weight=1.5,
              highlight = highlightOptions(weight = 5,
                                           color = "#666",
                                           dashArray = "",
                                           fillOpacity = 0.7,
                                           bringToFront = TRUE),
              label = data.shape@data$name,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addEasyButton(easyButton(
    icon="fa-crosshairs", title="Locate Me",
    onClick=JS("function(btn, map){ map.setView(L.latLng(37.8282, -98.5795), 3); }")))
m
