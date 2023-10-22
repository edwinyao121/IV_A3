library(shiny)
library(ggplot2)
library(ggiraph)
library(shinyjs)
library(shinydashboard)
library(dashboardthemes)
library(stringr)
library(owmr)
library(rgdal)
library(lubridate)
library(highcharter)
library(shinyWidgets)
library(fontawesome)
library(wordcloud2)
library(dplyr)
library(leaflet)
library(tidyr)
library(png)



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
    menuItem("Home",
             tabName = "home"
    ),
    menuItem("Places to Visit",
             tabName = "poi"
    ),
    menuItem("Melbourne Weather",
             tabName = "weather"
    ),
    menuItem("Places to Eat",
             tabName = "foodTab"
    ),
    menuItem("Places to Drink",
             tabName = "lineTab"
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
  tags$style(HTML("
    .wrap-text {
      white-space: normal;
      word-wrap: break-word;
    }
  ")),
  setUpTableauInShiny(),
  tabItems(
    tabItem("home",fluidPage(
      
      #
      carouselHead(),
      carouselUI()
      
    )),
    
    
    tabItem("poi",
            fluidPage(
              fluidRow(
                
                column(12,
                       titlePanel(strong("Place Of Interest in Melbourne")),
                       tableauPublicViz(
                         id='tableauViz',
                         url='https://public.tableau.com/views/A3yyh/Sheet3?:language=en-US&:display_count=n&:origin=viz_share_link'
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
    ),
    tabItem("weather",
            fluidPage(
              titlePanel(strong("Weather in Melbourne")),
              hr(),
              h5(strong(paste("Current Weather Overview of",
                              substr(as_datetime(current_weather$dt, tz = "Australia/Sydney"), 1, 10)))),
              fluidRow(
                column(4, valueBoxOutput("cur_temp", width = 20)),
                column(4, valueBoxOutput("wind_speed", width = 20)),
                column(4, valueBoxOutput("current_condition", width = 20)),
              ),
              hr(),
              h4(strong("Weather conditions in various areas of Melbourne")),
              fluidRow(
                column(12,
                       selectInput("region", "Choose a region:",
                                   choices = c("City of Melbourne", "St Kilda", "Camberwell", "Docklands"
                                               , "Fitzroy", "South Yarra", "Hawthorn", "Brunswick",
                                               "Footscray", "Toorak"))
                ),
                hr(),
                column(12,
                       uiOutput("weatherBoxes")
                )
              ),
              hr(),
              h4(strong("Melbourne's 5-day weather forecast")),
              fluidRow(
                column(12, highchartOutput("forecast_temp", height = 300))
              ),
              hr(),
              fluidRow(
                column(12, highchartOutput("forecast_humidity", height = 300))
              )
            )
    ),

    tabItem(tabName = "foodTab",
            titlePanel(strong("Places to eat")),
            sidebarLayout(
              sidebarPanel(
                selectInput(
                  inputId = 'Area',
                  label = 'Suburbs',
                  choices = c('All', sort(unique(restaurants_data$CLUE.small.area))),
                  selected = 'All'
                ),
                radioButtons(
                  inputId = 'food',
                  label = 'Restaurant type',
                  choices = c('Cafe' = 'Cafes and Restaurants',
                              'Takeaway food' = 'Takeaway Food Services',
                              'Bakery Product'= 'Bakery Product Manufacturing (Non-factory based)'),
                  selected = 'Cafes and Restaurants')
              ),
              mainPanel(
                leafletOutput('map_food', height = 600)
              )
            )
    ),

    tabItem("lineTab",
    fluidPage(
    titlePanel(strong("Places to drink")), #Popularity of bars
     splitLayout(
        tableauPublicViz(
          id='tableauViz',       
          url='https://public.tableau.com/views/cafes_16976382895680/Dashboard1?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link',
          height="600px"
        ),
      ) ,
   girafeOutput('plot_lines2' ,  height = 600),
    )
  )
 )
)
  

# Putting the UI together
ui <- dashboardPage(
  title = "Discover in Melbourne",
  header, 
  sidebar, 
  body
  )

#############
# Run Shiny #
#############

# shinyApp(ui, server, options=list(launch.browser=TRUE))