poi <- read.csv('data/filtered_poi.csv')
tram_point<-read.csv('data/tram_point.csv')


library(dplyr)

# Haversine公式来计算地球上两点之间的距离
haversine <- function(lon1, lat1, lon2, lat2) {
  R <- 6378137 # 地球的半径，单位是米
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
richmond <- get_current("Richmond, Melbourne, AU", units = "metric")
fitzroy <- get_current("Fitzroy, Melbourne, AU", units = "metric")
docklands <- get_current("Docklands, Melbourne, AU", units = "metric")
southyarra <- get_current("South Yarra, Melbourne, AU", units = "metric")
brighton <- get_current("Brighton, Melbourne, AU", units = "metric")
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
data_description <- restaurants_data$Industry..ANZSIC4..description

#popup for each restaurant seating type and number of seats
makeFoodPopup <- function(row) {
  paste0(strong(row$Trading.name), br(),
         ' (', row$Seating.type, ')', br(),
         'Number of seats: ', row$Number.of.seats, br())
}

#create Icon for each category
#the cafe image is downloaded from https://www.flaticon.com/free-icon/cafe_9620447?term=cafe&page=1&position=20&origin=search&related_id=9620447
cafeIcon <- makeIcon(
  'cafe.png',
  iconWidth = 15,
  iconHeight = 15
)

#the image is downloaded from https://www.flaticon.com/free-icons/bakery
bakeryIcon <- makeIcon(
  'bakery.png',
  iconWidth = 15,
  iconHeight = 15
)

takeawayIcon <- makeIcon(
  'takeaway.png',
  iconWidth = 15,
  iconHeight = 15
)

#Icon_allocated <- sapply(data_description, function(type){
#if(type == "Cafes and Restaurants"){
#return(cafeIcon)
#}else if (type == 'Takeaway Food Services'){
#return(barIcon)
#}else if (type == "Pubs, Taverns and Bars"){
#return(takeawayIcon)
#}else{return(cafeIcon)
#}
#})

restaurants_data$Popup <- by(restaurants_data, seq_len(nrow(restaurants_data)), makeFoodPopup)
# 使用例子
# result <- find_nearby_poi("Melbourne Aquarium / Flinders Street")
# print(result)
# df_summary <- as.data.frame(table(result$Theme))
# 
# # 使用ggplot2绘制饼图
# ggplot(df_summary, aes(x = "", y = Freq, fill = Var1)) +
#   geom_bar(width = 1, stat = "identity") +
#   coord_polar("y", start = 0) + 
#   theme_void() +
#   labs(title = "Distribution of Themes", fill = "Theme")
################ silde img
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