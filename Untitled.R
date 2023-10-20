library('shiny')
library('ggplot2')
library('ggiraph')
library('shinyjs')
library(shinydashboard)
library(dashboardthemes)

library(wordcloud2)
library(dplyr)

# Load the GEOM90007 Tableau in Shiny library
source('tableau-in-shiny-v1.0.R')

# load data
source('helper.R')

##################
# USER INTERFACE #
##################

# poi_tab <- tabPanel(
#   title='Place Of Interest',
#   splitLayout(
#     tableauPublicViz(
#       id='tableauViz',
#       url='https://public.tableau.com/views/A3yyh/Dashboard1?:language=en-US&:display_count=n&:origin=viz_share_link',
#       height="800px",
#       weight="800px"
#     ),girafeOutput('plot_hosp_births_timeline', width="30%", height="600px")
#   )
# )
header <- dashboardHeader(
  # Define the header and insert local image as title
  title = tags$a(tags$img(src='header.png', height='50', width='160')),
  titleWidth = 250
)
sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    # menuItem("Home",
    #          tabName = "home",
    #          selected = T,
    #          #icon = icon('thumbs-up')
    #          ),
    menuItem("Places to Visit",
             tabName = "poi"
             #,icon = icon('map-location-dot')
             )
  )
)
body <- dashboardBody(
  tags$style(HTML("
      .content-wrapper, .right-side {
                                background-color: white;}")),
  tags$style(HTML("
    .white-hr {
      border-top: 1px solid white;
    }
  ")),
  setUpTableauInShiny(),
  tabItem("poi",
          fluidPage(
            fluidRow(
              # 这里将Tableau的可视化放入一个较宽的列中
              column(12,
                     titlePanel(strong("Place Of Interest in Melbourne")),
                     tableauPublicViz(
                       id='tableauViz',
                       url='https://public.tableau.com/views/A3yyh/Dashboard1?:language=en-US&:display_count=n&:origin=viz_share_link'
                       ,height="600px",
                       )
              ),
              
            ),
            hr(class="white-hr"),
            hr(class="white-hr"),

            
            fluidRow(
              
              column(4, titlePanel(strong("Different Themes near the selected station ")),#style = "display: flex; justify-content: center; align-items: center; height: 600px;",
                            girafeOutput('pie_near_poi', width="100%", height="600px")
                            ),
              column(8,  titlePanel(strong("Different Sub Themes near the selected station ")),
                     wordcloud2Output("myWordcloud"))
                     )
            
          )
  )
  ,
)

# ui <- navbarPage(
#   header=setUpTableauInShiny(),
#   title='Place Of Interest in Melbroune',
#   poi_tab
# )
# Putting the UI together
ui <- dashboardPage(
  title = "123",
  header, 
  sidebar, 
  body
)

server <- function(input, output, session) {

  output$myWordcloud <- renderWordcloud2({
    # Find the name of the hospital clicked by the user
    sta <- input$tableauViz_parameter_changed$value
    
    # If no hospital selected, stop
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
   
    # If no hospital selected, stop
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
  
  
}

#############
# Run Shiny #
#############

shinyApp(ui, server, options=list(launch.browser=TRUE))
