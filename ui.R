#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
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
                 column(4, textInput(inputId = "left_box", label = "Left",value = "2.1191000",width = "100%")  ),
                 column(4, textInput(inputId = "right_box", label = "Right",value = "2.1409000",width = "100%"),offset = 4 )
               ),
               textInput(inputId = "bottom_box", label = "Bottom",value = "48.7987000",width = "100%"),
               checkboxInput(inputId = "want_example",label = "I don't know what to put here, let me go with little example here",value = 1, width = "100%"),
               actionButton("submit", "Begin the journey!",icon = icon("paper-plane"))#icon("floppy-o"),
                 ,plotOutput("class_summary")
                 
      )
    )
  )
)
