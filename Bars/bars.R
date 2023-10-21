library('shiny')
library('ggplot2')
library('ggiraph')
library('shinyjs')
library('tidyr')
library('dplyr')


source('tableau-in-shiny-v1.0.R')

data <- read.csv('bars-and-pubs-with-patron-capacity.csv')

trading_data <- data %>% group_by( Trading.name  ) %>%
  summarise( number_of_parons=sum( Number.of.patrons ) ) %>%   ungroup() %>%
  arrange(desc( number_of_parons ) ) %>% 
  slice_head(n=20 )
#trading_data$Trading.name <- factor(trading_data$Trading.name, levels = trading_data$Trading.name)


##################
# USER INTERFACE #
##################

line_tab <- tabPanel(
  title="Popularity of bars",
  h3("Ranking the popularity of bars in the surburb"),
  verticalLayout(
    splitLayout(
      girafeOutput('plot_lines' ,width = "100%", height = "600px"),
      tableauPublicViz(
        id='tableauViz',       
        url='https://public.tableau.com/shared/2Q5FXWFHJ?:display_count=n&:origin=viz_share_link',
        height="600px"
      ),
    ) ,
    girafeOutput('plot_lines2' ,width = "100%", height = "600px"),
  )
)

ui <- navbarPage(
  header=setUpTableauInShiny(),
  title='Melbourne Bars Distribution',
  line_tab
)

################
# SHINY SERVER #
################

server <- function(input, output, session) {
  output$plot_lines <- renderGirafe({
    
    lineplot <-    trading_data %>% rename(name= Trading.name   ) %>%  ggplot( aes(x = name, y= number_of_parons  ,group=1 ,data_id = name)) +
      geom_bar_interactive(stat='identity', width=0.8, fill='#8f00b6')+
      
      theme(axis.text.x = element_text(angle = 90, hjust = 1) ,panel.grid = element_blank(),
            panel.background = element_rect(fill = "white") )
    girafe(ggobj=lineplot, height_svg=5)
    
  })
  
  
  
  # React to clicks on the bar chart
  # (See Lab 7 (page 7.6.2) for an explanation of this code)
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

#############
# Run Shiny #
#############

shinyApp(ui, server, options=list(launch.browser=TRUE))
