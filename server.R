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
  
  
}