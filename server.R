
server <- function(input, output, session) {
  
  output$myWordcloud <- renderWordcloud2({
    # Find the name of the hospital clicked by the user
    sta <- input$tableauViz_parameter_changed$value
    
    
    if(is.null(sta)) return()
    if(sta=='null') return()
    
    # Filter the births data to the hospital clicked by the user, and
    # transform it into the format required by ggplot's geom_line
    
    data <- find_nearby_poi(sta)
    # 计算Sub.Theme的频率
    data <- as.data.frame(data)
    word_freq <- table(data$Sub.Theme)
    
    sub_theme_colors <- theme_color_mapping[data$Theme]
    color_df <- data.frame(word = data$Sub.Theme, colors = sub_theme_colors)
    color_df_sorted <- color_df[match(names(word_freq), color_df$word), ]
    
    print(word_freq)
    # 使用wordcloud2生成词云
    
    ############
    data <- data.frame(
      word = c("Gallery/Museum",'Gallery/ Museum'),
      freq = c(1,0.0001),
      stringsAsFactors = FALSE
    )
    
    # 根据Sub.Theme列生成词云
    if(sta=='Spencer Street / La Trobe Street'){
      wordcloud2(data, size=0.2,minSize=1,color='purple')
    }else{
      wordcloud2(word_freq, size = 0.2, color=color_df_sorted$colors)
    }
    
    ############
    
    #wordcloud2(word_freq, size = 0.2, color=color_df_sorted$colors)
  })
  
  output$pie_near_poi <- renderGirafe({
    # Find the name of the hospital clicked by the user
    station_name <- input$tableauViz_parameter_changed$value
    
    if(is.null(station_name)) return()
    if(station_name=='null') return()
    
    # Filter the births data to the hospital clicked by the user, and
    # transform it into the format required by ggplot's geom_line
    
    result <- find_nearby_poi(station_name)
    
    df_summary <- as.data.frame(table(result$Theme))
    
    p <- ggplot(df_summary, aes(x = "", y = Freq, fill = Var1,
                                tooltip = paste(Var1, "'s count: ", Freq))) +
      geom_bar_interactive(width = 1, stat = "identity") +
      coord_polar("y", start = 0) + 
      theme_void() +
      labs(title = paste("Themes near", station_name), fill = "Theme")+
      scale_fill_manual(values = theme_color_mapping)
    
    
    girafe(ggobj=p, height_svg=4, width_svg=4)
  })
  
  output$welcomeMessage <- renderText({
    return("欢迎来到我的Shiny应用!")
  })
  
  #Weather
  output$cur_temp <- renderValueBox(
    valueBox(
      value = tags$p(paste(current_weather$main$temp, "ºC"), style = "font-size: 85%;"), 
      subtitle = paste("Temperature (°C)"),
      icon = fa_i("fas fa-temperature-three-quarters"), 
      color = "orange"
    )
  )
  
  output$wind_speed <- renderValueBox(
    valueBox(
      value = tags$p(paste(current_weather$wind$speed), style = "font-size: 85%;"), 
      subtitle = "Wind Speed (m/s)",
      icon = fa_i("wind"), color = "light-blue"
    )
  )
  
  output$current_condition <- renderValueBox(
    valueBox(
      value = tags$p(str_to_title(current_weather$weather$description), 
                     style = "font-size: 85%;"), 
      subtitle = "Weather Descreption",
      icon = fa_i("fas fa-circle-info"), color = "teal"
    )
  )
  
  output$weatherBoxes <- renderUI({
    # Depending on the chosen region, render a set of value boxes
    switch(input$region,
           "City of Melbourne" = div(
             valueBox(
               value = tags$p(paste(melbourne_cbd$main$temp, "ºC"), style = "font-size: 85%;"), 
               subtitle = paste("Temperature (°C)"),
               icon = fa_i("fas fa-temperature-three-quarters"), 
               color = "orange"
             ),
             valueBox(
               value = tags$p(paste(round(melbourne_cbd$main$temp_min), "-", 
                                    round(melbourne_cbd$main$temp_max), "ºC"),
                              style = "font-size: 85%;"), 
               subtitle = paste("Temperature Range"),
               icon = fa_i("fas fa-thermometer-half"), 
               color = "olive"
             ),
             valueBox(
               value = tags$p(paste(melbourne_cbd$main$humidity, "%"), style = "font-size: 85%;"), 
               subtitle = paste("Humidity (%)"),
               icon = icon("tint"), 
               color = "aqua"
             ),
           ),
           "St Kilda" = div(
             valueBox(
               value = tags$p(paste(st_kilda$main$temp, "ºC"), style = "font-size: 85%;"), 
               subtitle = paste("Temperature (°C)"),
               icon = fa_i("fas fa-temperature-three-quarters"), 
               color = "orange"
             ),
             valueBox(
               value = tags$p(paste(round(st_kilda$main$temp_min), "-",
                                    round(st_kilda$main$temp_max), "ºC"),
                              style = "font-size: 85%;"), 
               subtitle = paste("Temperature Range"),
               icon = fa_i("fas fa-thermometer-half"), 
               color = "olive"
             ),
             valueBox(
               value = tags$p(paste(st_kilda$main$humidity, "%"), style = "font-size: 85%;"), 
               subtitle = paste("Humidity (%)"),
               icon = icon("tint"), 
               color = "aqua"
             ),
           ),
           "Richmond" = div(
             valueBox(
               value = tags$p(paste(richmond$main$temp, "ºC"), style = "font-size: 85%;"), 
               subtitle = paste("Temperature (°C)"),
               icon = fa_i("fas fa-temperature-three-quarters"), 
               color = "orange"
             ),
             valueBox(
               value = tags$p(paste(round(richmond$main$temp_min), "-",
                                    round(richmond$main$temp_max), "ºC"),
                              style = "font-size: 85%;"), 
               subtitle = paste("Temperature Range"),
               icon = fa_i("fas fa-thermometer-half"), 
               color = "olive"
             ),
             valueBox(
               value = tags$p(paste(richmond$main$humidity, "%"), style = "font-size: 85%;"), 
               subtitle = paste("Humidity (%)"),
               icon = icon("tint"), 
               color = "aqua"
             ),
           ),
           "Docklands" = div(
             valueBox(
               value = tags$p(paste(docklands$main$temp, "ºC"), style = "font-size: 85%;"), 
               subtitle = paste("Temperature (°C)"),
               icon = fa_i("fas fa-temperature-three-quarters"), 
               color = "orange"
             ),
             valueBox(
               value = tags$p(paste(round(docklands$main$temp_min), "-",
                                    round(docklands$main$temp_max), "ºC"),
                              style = "font-size: 85%;"), 
               subtitle = paste("Temperature Range"),
               icon = fa_i("fas fa-thermometer-half"), 
               color = "olive"
             ),
             valueBox(
               value = tags$p(paste(docklands$main$humidity, "%"), style = "font-size: 85%;"), 
               subtitle = paste("Humidity (%)"),
               icon = icon("tint"), 
               color = "aqua"
             ),
           ),
           "Fitzroy" = div(
             valueBox(
               value = tags$p(paste(fitzroy$main$temp, "ºC"), style = "font-size: 85%;"), 
               subtitle = paste("Temperature (°C)"),
               icon = fa_i("fas fa-temperature-three-quarters"), 
               color = "orange"
             ),
             valueBox(
               value = tags$p(paste(round(fitzroy$main$temp_min), "-",
                                    round(fitzroy$main$temp_max), "ºC"),
                              style = "font-size: 85%;"), 
               subtitle = paste("Temperature Range"),
               icon = fa_i("fas fa-thermometer-half"), 
               color = "olive"
             ),
             valueBox(
               value = tags$p(paste(fitzroy$main$humidity, "%"), style = "font-size: 85%;"), 
               subtitle = paste("Humidity (%)"),
               icon = icon("tint"), 
               color = "aqua"
             ),
           ),
           "South Yarra" = div(
             valueBox(
               value = tags$p(paste(southyarra$main$temp, "ºC"), style = "font-size: 85%;"), 
               subtitle = paste("Temperature (°C)"),
               icon = fa_i("fas fa-temperature-three-quarters"), 
               color = "orange"
             ),
             valueBox(
               value = tags$p(paste(round(southyarra$main$temp_min), "-",
                                    round(southyarra$main$temp_max), "ºC"),
                              style = "font-size: 85%;"), 
               subtitle = paste("Temperature Range"),
               icon = fa_i("fas fa-thermometer-half"), 
               color = "olive"
             ),
             valueBox(
               value = tags$p(paste(southyarra$main$humidity, "%"), style = "font-size: 85%;"), 
               subtitle = paste("Humidity (%)"),
               icon = icon("tint"), 
               color = "aqua"
             ),
           ),
           "Brighton" = div(
             valueBox(
               value = tags$p(paste(brighton$main$temp, "ºC"), style = "font-size: 85%;"), 
               subtitle = paste("Temperature (°C)"),
               icon = fa_i("fas fa-temperature-three-quarters"), 
               color = "orange"
             ),
             valueBox(
               value = tags$p(paste(round(brighton$main$temp_min), "-",
                                    round(brighton$main$temp_max), "ºC"),
                              style = "font-size: 85%;"), 
               subtitle = paste("Temperature Range"),
               icon = fa_i("fas fa-thermometer-half"), 
               color = "olive"
             ),
             valueBox(
               value = tags$p(paste(brighton$main$humidity, "%"), style = "font-size: 85%;"), 
               subtitle = paste("Humidity (%)"),
               icon = icon("tint"), 
               color = "aqua"
             ),
           ),
           "Brunswick" = div(
             valueBox(
               value = tags$p(paste(brunswick$main$temp, "ºC"), style = "font-size: 85%;"), 
               subtitle = paste("Temperature (°C)"),
               icon = fa_i("fas fa-temperature-three-quarters"), 
               color = "orange"
             ),
             valueBox(
               value = tags$p(paste(round(brunswick$main$temp_min), "-",
                                    round(brunswick$main$temp_max), "ºC"),
                              style = "font-size: 85%;"), 
               subtitle = paste("Temperature Range"),
               icon = fa_i("fas fa-thermometer-half"), 
               color = "olive"
             ),
             valueBox(
               value = tags$p(paste(brunswick$main$humidity, "%"), style = "font-size: 85%;"), 
               subtitle = paste("Humidity (%)"),
               icon = icon("tint"), 
               color = "aqua"
             ),
           ),
           "Footscray" = div(
             valueBox(
               value = tags$p(paste(footscray$main$temp, "ºC"), style = "font-size: 85%;"), 
               subtitle = paste("Temperature (°C)"),
               icon = fa_i("fas fa-temperature-three-quarters"), 
               color = "orange"
             ),
             valueBox(
               value = tags$p(paste(round(footscray$main$temp_min), "-",
                                    round(footscray$main$temp_max), "ºC"),
                              style = "font-size: 85%;"), 
               subtitle = paste("Temperature Range"),
               icon = fa_i("fas fa-thermometer-half"), 
               color = "olive"
             ),
             valueBox(
               value = tags$p(paste(footscray$main$humidity, "%"), style = "font-size: 85%;"), 
               subtitle = paste("Humidity (%)"),
               icon = icon("tint"), 
               color = "aqua"
             ),
           ),
           "Toorak" = div(
             valueBox(
               value = tags$p(paste(toorak$main$temp, "ºC"), style = "font-size: 85%;"), 
               subtitle = paste("Temperature (°C)"),
               icon = fa_i("fas fa-temperature-three-quarters"), 
               color = "orange"
             ),
             valueBox(
               value = tags$p(paste(round(toorak$main$temp_min), "-",
                                    round(toorak$main$temp_max), "ºC"),
                              style = "font-size: 85%;"), 
               subtitle = paste("Temperature Range"),
               icon = fa_i("fas fa-thermometer-half"), 
               color = "olive"
             ),
             valueBox(
               value = tags$p(paste(toorak$main$humidity, "%"), style = "font-size: 85%;"), 
               subtitle = paste("Humidity (%)"),
               icon = icon("tint"), 
               color = "aqua"
             ),
           )
    )
  })
  
  output$forecast_temp <- renderHighchart({
    hc_theme = hc_theme_google()
    forecast_df$tmstmp <- datetime_to_timestamp(forecast_df$tmstmp)
    
    forecast_df %>%
      hchart("spline", hcaes(x = tmstmp, y = temp)) %>%
      hc_xAxis(type = "datetime",
               tickInterval = 24 * 3600 * 1000,
               dateTimeLabelFormats = list(day='%d %b %Y'), 
               labels = list(enabled = TRUE, format = '{value:%Y-%m-%d}'),
               title = list(text = "Next Five Days")) %>%
      hc_yAxis(title = list(text = "Forecast Temperature"), 
               labels = list(format = "{value} ºC")) %>%
      hc_title(text = "Forecast Temperature of Next 5 Days") %>%
      hc_subtitle(text = 'Source: <a href="https://home.openweathermap.org" 
                  target="_blank">OpenWeather</a>') %>%
      hc_tooltip(pointFormat = "<br/>Forecast Temperature: <b>{point.temp} ºC</b>
                 <br/>Feels Like: <b>{point.fl_temp} ºC</b>") %>%
      hc_plotOptions(series = list(animation = list(duration = 3000))) %>%
      hc_colors("#E6CC00") %>%
      hc_add_theme(hc_theme)
  })
  
  output$forecast_humidity <- renderHighchart({
    hc_theme = hc_theme_google()
    forecast_df$tmstmp <- datetime_to_timestamp(forecast_df$tmstmp)
    
    forecast_df %>%
      hchart("spline", hcaes(x = tmstmp, y = humidity)) %>%
      hc_xAxis(type = "datetime",
               tickInterval = 24 * 3600 * 1000,
               dateTimeLabelFormats = list(day='%d %b %Y'), 
               labels = list(enabled = TRUE, format = '{value:%Y-%m-%d}'),
               title = list(text = "Next Five Days")) %>%
      hc_yAxis(title = list(text = "Forecast Humidity"), 
               labels = list(format = "{value}%")) %>%
      hc_title(text = "Forecast Humidity of Next 5 Days") %>%
      hc_subtitle(text = 'Source: <a href="https://home.openweathermap.org" 
                  target="_blank">OpenWeather</a>') %>%
      hc_tooltip(pointFormat = "<br/>Forecast Humidity: <b>{point.humidity}%</b>") %>%
      hc_plotOptions(series = list(animation = list(duration = 3000))) %>%
      hc_colors("#03A9F4") %>%
      hc_add_theme(hc_theme)
  })
  
  ###Restaurant
  #reactive data filter from user selected
  getFilteredRegionData <- reactive({
    filter(restaurants_data, 
           if (input$food == 'Cafes and Restaurants') TRUE else Industry..ANZSIC4..description == input$food,
           if (input$Area == 'All') TRUE else CLUE.small.area == input$Area)
  })
  
  #create map with reactive(input) data
  output$map_food <- renderLeaflet({
    
    reactive_data <- getFilteredRegionData()
    
    # no output if there is no corresponding data, giving user advice. 
    # this code is based on the code at https://shiny.posit.co/r/articles/improve/validation/
    validate(
      need(reactive_data != "", "There is no available data, please choose other surburbs")
    )
    #those code based on tutorial work 
    leaflet(reactive_data) %>%
      addProviderTiles(providers$CartoDB) %>%
      addMarkers(lng=~Longitude, lat=~Latitude, icon =~eatIcon, clusterOptions = markerClusterOptions(),
                 label=~Trading.name,
                 popup=~Popup,
                 layerId=~Trading.name)
  })
  
  
  ###Bars###########
  
  barsdata <- read.csv('bars-and-pubs-with-patron-capacity.csv')
  
  trading_data <- barsdata %>% group_by( Trading.name  ) %>%
    summarise( number_of_parons=sum( Number.of.patrons ) ) %>%   ungroup() %>%
    arrange(desc( number_of_parons ) ) %>% 
    slice_head(n=20 ) 
  trading_data$Trading.name <- factor(trading_data$Trading.name, levels = trading_data$Trading.name)
  
  output$plot_lines <- renderGirafe({
    
    lineplot <-  trading_data %>% 
      rename(name= Trading.name   ) %>%  ggplot( aes(x = rev(name), y= number_of_parons  ,group=1 ,data_id = name)) +
      geom_bar_interactive(stat='identity', width=0.8, fill='#8f00b6')+
      # theme(axis.text.x = element_text(angle = 90, hjust = 1) ,panel.grid = element_blank(),
      #       panel.background = element_rect(fill = "white") ) +
      labs(x = NULL, y = "Number of patrons", title = "Ranking the popularity of bars in the surburb")+
      scale_y_continuous(limits = c(0, 60000))+
      coord_flip()+
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0, size = 10))
    
    
    girafe(ggobj=lineplot, height_svg=5)
  })
  
      observeEvent(input$plot_lines_selected, {
    
    print(input$plot_lines_selected )
    # Clear selection from bar chart
    session$sendCustomMessage(type='plot_lines_set', message=character(0))
    
    # Filter Tableau viz by the state that was clicked on the bar chart
    TradingName <- input$plot_lines_selected
    
    print(TradingName)
    
    runjs(
      
      sprintf('let viz = document.getElementById("tableauViz");
let sheet = viz.workbook.activeSheet;
console.log(sheet.worksheets[0] );
sheet.worksheets[0].applyFilterAsync("Trading name", ["%s"], FilterUpdateType.Replace);', TradingName)
      
      
    )
  })
  
}
