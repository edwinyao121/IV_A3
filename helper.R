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
    tags$link(rel = "stylesheet", type = "text/css", href = "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css"),
    tags$script(type = "text/javascript", src = "https://code.jquery.com/jquery-3.2.1.slim.min.js"),
    tags$script(type = "text/javascript", src = "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.9/umd/popper.min.js"),
    tags$script(type = "text/javascript", src = "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.min.js")
  )
}
carouselUI <- function() {
  
  
  tags$div(class = "carousel slide", data_ride = "carousel", id = "myCarousel",
           
           # Indicators
           tags$ol(class = "carousel-indicators",
                   tags$li(data_target = "#myCarousel", data_slide_to = "0", class = "active"),
                   tags$li(data_target = "#myCarousel", data_slide_to = "1"),
                   tags$li(data_target = "#myCarousel", data_slide_to = "2")
           ),
           
           # Slides
           tags$div(class = "carousel-inner",
                    tags$div(class = "carousel-item active",
                             tags$img(src = "1.png", class = "d-block w-100"),
                             tags$div(class = "carousel-caption", "Caption for Image 1")
                    ),
                    tags$div(class = "carousel-item",
                             tags$img(src = "2.png", class = "d-block w-100"),
                             tags$div(class = "carousel-caption", "Caption for Image 2")
                    ),
                    tags$div(class = "carousel-item",
                             tags$img(src = "1.png", class = "d-block w-100"),
                             tags$div(class = "carousel-caption", "Caption for Image 3")
                    )
           ),
           
           # Left and right controls
           tags$a(class = "carousel-control-prev", href = "#myCarousel", role = "button", 'data-slide' = "prev",
                  tags$span(class = "carousel-control-prev-icon", aria_hidden = "true"),
                  tags$span(class = "sr-only", "Previous")
           ),
           tags$a(class = "carousel-control-next", href = "#myCarousel", role = "button", 'data-slide' = "next",
                  tags$span(class = "carousel-control-next-icon", aria_hidden = "true"),
                  tags$span(class = "sr-only", "Next")
           )
  )
}