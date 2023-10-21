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
                                               tags$h1(class = "fit-head", style = "color:white", "Get set to Discover Melbourne"),
                                               tags$p(class = "fit-text", "There's something new around every corner."),
                                               tags$p(
                                                 tags$a(class = "btn btn-lg btn-primary", onclick = "$('li:eq(1) a').tab('show');", role = "button", "Explore now »")
                                               )
                                      )
                             )
                    ),
                    tags$div(class = "item",
                             tags$img(src = "2.png", alt = "2"),
                             # tags$div(class = "container",
                             #          tags$div(class = "carousel-caption",
                             #                   tags$h1(class = "fit-head", style = "color:white", "Exploring Melbourne"),
                             #                   tags$p(class = "fit-text", "From vintage cafe to luxury malls, Melbourne has a dream space for everyone."),
                             #                   tags$a(class = "btn btn-lg btn-primary", onclick = "$('li:eq(2) a').tab('show');", role = "button", "Explore now »")
                             #          )
                             # )
                    ),
                    tags$div(class = "item",
                             tags$img(src = "1.png", alt = "3"),
                             # tags$div(class = "container",
                             #          tags$div(class = "carousel-caption",
                             #                   tags$h1(class = "fit-head", style = "color:white", "Exploring Melbourne"),
                             #                   tags$p(class = "fit-text", "From vintage cafe to luxury malls, Melbourne has a dream space for everyone."),
                             #                   tags$a(class = "btn btn-lg btn-primary", onclick = "$('li:eq(3) a').tab('show');", role = "button", "Explore now »")
                             #          )
                             # )
                    ),
                    tags$div(class = "item",
                             tags$img(src = "1.png", alt = "4"),
                             # tags$div(class = "container",
                             #          tags$div(class = "carousel-caption",
                             #                   tags$h1(class = "fit-head", style = "color:white", "Exploring Melbourne"),
                             #                   tags$p(class = "fit-text", "From vintage cafe to luxury malls, Melbourne has a dream space for everyone."),
                             #                   tags$a(class = "btn btn-lg btn-primary", onclick = "$('li:eq(4) a').tab('show');", role = "button", "Explore now »")
                             #          )
                             # )
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