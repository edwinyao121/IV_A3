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