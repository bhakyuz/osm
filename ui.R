library(shiny)
library(leaflet)
# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
  # Application title
    titlePanel("Open Street Map Project"),
  
    # Show a plot of the generated distribution
    tabsetPanel(
      
      tabPanel(title = "Discover", value = "discover_osm",icon = icon("globe",class = "font-awesome"),
               h4("In order to start some analysis on OSM, we need coordinates first. Please choose bounding box by filling following fields",align = "center"),
               textInput(inputId = "top_box", label = "Top",value = "48.8069000",width = "100%"),
               fluidRow(
                 column(6, textInput(inputId = "left_box", label = "Left",value = "2.1191000",width = "100%")  ),
                 column(6, textInput(inputId = "right_box", label = "Right",value = "2.1409000",width = "100%"))
               ),
               textInput(inputId = "bottom_box", label = "Bottom",value = "48.7987000",width = "100%"),
               fluidRow(
                 column(8, checkboxInput(inputId = "want_example",label = "I don't know what to put here, let me go with little example around Versailles, Ile-de-France (No coordinates needed)",value = 1, width = "100%")),
                 column(4, selectInput(inputId ="which_example", label = "Choose an example",choices = c( "Versailles","Paris-Notre Dame"), width = "100%" )  )
               ),
               #checkboxInput(inputId = "want_example",label = "I don't know what to put here, let me go with little example around Versailles, Ile-de-France (No coordinates needed)",value = 1, width = "100%"),
               actionButton("submit", "Begin the journey!",icon = icon("paper-plane"),width = "100%"),
               shiny::tags$br(),
               shiny::h3(textOutput("settlement"),align = "center"),
               shiny::tags$br(),
               fluidRow(
                 column(width = 4, 
                        uiOutput("nodes_general"),
                        plotOutput("nodes_class_summary"),
                        plotOutput("nodes_prop_summary")
                        ),
                 column(width = 4, 
                        uiOutput("ways_general"),
                        plotOutput("ways_class_summary"),
                        plotOutput("ways_prop_summary")
                 ),
                 column(width = 4, 
                        uiOutput("relations_general"),
                        plotOutput("relations_class_summary"),
                        plotOutput("relations_prop_summary")
                 )
      )
      ),
      tabPanel(title = "Search", value = "search_osm",icon = icon("search",class = "font-awesome"),
               shiny::h4("Important places in chosen area can be seen on below... (necessary to provide coordinates first in 'discover' panel) ",align = "center"),
               leafletOutput("search_map"),
               dataTableOutput("search_table")
      ),
      tabPanel(title = "Contribution", value = "cont_osm",icon = icon("users",class = "font-awesome"),
               shiny::h4("Contributions to OpenStreetMap can be seen in terms of days, hours and OSM users: (necessary to provide coordinates first in 'discover' panel)",align = "center"),
               fluidRow(
                 column(width = 6, 
                        plotOutput("cont_days")
                        ),
                 column(width = 6, 
                        plotOutput("cont_hours")
                        )
                 ),
               plotOutput("cont_users")
      )
    )
  )
)
