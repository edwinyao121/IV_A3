poi <- read.csv('data/filtered_poi.csv')
tram_point<-read.csv('data/tram_point.csv')


library(dplyr)

# gpt
haversine <- function(lon1, lat1, lon2, lat2) {
  R <- 6378137 # 
  delta_lat <- (lat2 - lat1) * (pi / 180)
  delta_lon <- (lon2 - lon1) * (pi / 180)
  
  a <- sin(delta_lat / 2) * sin(delta_lat / 2) + cos(lat1 * (pi / 180)) * cos(lat2 * (pi / 180)) * sin(delta_lon / 2) * sin(delta_lon / 2)
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  
  d <- R * c
  return(d)
}

find_nearby_poi <- function(name) {
  tram_location <- tram_point %>% filter(name == !!name) %>% select(lat, lon)

  if(nrow(tram_location) == 0) {
    return(data.frame(name=character(), lat=numeric(), lon=numeric()))
  }
  lat_tram <- tram_location$lat
  lon_tram <- tram_location$lon
  
  nearby_poi <- poi %>% rowwise() %>% 
    mutate(distance = haversine(lon, lat, lon_tram, lat_tram)) %>% 
    filter(distance <= 350) %>% 
    select(Feature.Name, Theme, Sub.Theme,lat, lon)
  
  return(nearby_poi)
}

#########
#Color
theme_color_mapping <- c('Retail' = 'red', 
                         'Leisure/Recreation' = 'orange', 
                         'Place of Worship' = 'blue', 
                         'Community Use' = 'green', 
                         'Place Of Assembly' = 'purple')

#Weather
Sys.setenv(OWM_API_KEY = "3614f35658ccb95466f1d5c29e4d73ff")

current_weather <- get_current("Melbourne, AU", units = "metric")
melbourne_cbd <- get_current("Melbourne, AU", units = "metric")
st_kilda <- get_current("St Kilda, Melbourne, AU", units = "metric")
camberwell <- get_current("Camberwell, Melbourne, AU", units = "metric")
fitzroy <- get_current("Fitzroy, Melbourne, AU", units = "metric")
docklands <- get_current("Docklands, Melbourne, AU", units = "metric")
southyarra <- get_current("South Yarra, Melbourne, AU", units = "metric")
hawthorn <- get_current("Hawthorn, Melbourne, AU", units = "metric")
brunswick <- get_current("Brunswick, Melbourne, AU", units = "metric")
footscray <- get_current("Footscray, Melbourne, AU", units = "metric")
toorak <- get_current("Toorak, Melbourne, AU", units = "metric")

forecast_data <- get_forecast("Melbourne, AU", units = "metric")
forecast_df <- data.frame(
  tmstmp =  forecast_data$list$dt_txt,
  temp = forecast_data$list$main.temp,
  fl_temp = forecast_data$list$main.feels_like,
  humidity = forecast_data$list$main.humidity
)
forecast_df$tmstmp <- ymd_hms(forecast_df$tmstmp)
forecast_df$tmstmp <- as.POSIXlt(forecast_df$tmstmp, tz="Australia/Sydney")

#Restaurant

#get the restaurants data and country geolocation from csv file
restaurants_data <- read.csv('data/cafes-and-restaurants-with-seating-capacity.csv') 

#popup for each restaurant seating type and number of seats
makeFoodPopup <- function(row) {
  paste0(strong(row$Trading.name), br(),
         ' (', row$Seating.type, ')', br(),
         'Number of seats: ', row$Number.of.seats, br())
}

#create Icon for restaurant
#the cafe image is downloaded from https://www.flaticon.com/free-icon/restaurant_4682602?term=restaurant&page=1&position=36&origin=tag&related_id=4682602
eatIcon <- makeIcon(
  'restaurant.png',
  iconWidth = 20,
  iconHeight = 20
)

restaurants_data$Popup <- by(restaurants_data, seq_len(nrow(restaurants_data)), makeFoodPopup)
########tableau img reference 
# https://www.anyrgb.com/en-clipart-srnkn
# https://www.freepik.com/icon/leisure_9712848
# https://www.freepik.com/icon/community_10204296
# https://www.freepik.com/icon/retail-store_9541240
# https://www.freepik.com/icon/night-club_3093990

########

################ silde img######GPT
carouselHead <-function(){
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css")
  )
}
carouselUI <- function() {
  tags$div(id = "myCarousel", class = "carousel slide", data_ride = "carousel",
           
           # Indicators
           tags$ol(class = "carousel-indicators",
                   tags$li(data_target = "#myCarousel", data_slide_to = "0", class = "active"),
                   tags$li(data_target = "#myCarousel", data_slide_to = "1"),
                   tags$li(data_target = "#myCarousel", data_slide_to = "2"),
                   tags$li(data_target = "#myCarousel", data_slide_to = "3")
           ),
           
           # Slides
           tags$div(class = "carousel-inner",
                    tags$div(class = "item active",
                             tags$img(src = "1.png", alt = "1"),
                             tags$div(class = "container",
                                      tags$div(class = "carousel-caption",
                                               tags$p(
                                                 tags$a(class = "btn btn-lg btn-primary", onclick = "$('li:eq(1) a').tab('show');", role = "button", "Check POI ")
                                               )
                                      )
                             )
                    ),
                    tags$div(class = "item",
                             tags$img(src = "2.png", alt = "2"),
                             tags$div(class = "container",
                                      tags$div(class = "carousel-caption",
                                               tags$a(class = "btn btn-lg btn-primary", onclick = "$('li:eq(2) a').tab('show');", role = "button", "Check Weather ")
                                      )
                             )
                    ),
                    tags$div(class = "item",
                             tags$img(src = "3.png", alt = "3"),
                             tags$div(class = "container",
                                      tags$div(class = "carousel-caption",
                                               tags$a(class = "btn btn-lg btn-primary", onclick = "$('li:eq(3) a').tab('show');", role = "button", "Check Restaurant ")
                                      )
                             )
                    ),
                    tags$div(class = "item",
                             tags$img(src = "4.png", alt = "4"),
                             tags$div(class = "container",
                                      tags$div(class = "carousel-caption",
                                               tags$a(class = "btn btn-lg btn-primary", onclick = "$('li:eq(4) a').tab('show');", role = "button", "Check Bar ")
                                      )
                             )
                    )
           ),
           
           # Left and right controls
           tags$a(class = "left carousel-control", href = "#myCarousel", 'data-slide' = "prev",
                  tags$span(class = "glyphicon glyphicon-chevron-left")
           ),
           tags$a(class = "right carousel-control", href = "#myCarousel", 'data-slide' = "next",
                  tags$span(class = "glyphicon glyphicon-chevron-right")
           )
  )
}
