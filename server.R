#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("osm.R")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  printError
  user_input<-eventReactive(input$submit,
                                {
                                  coords<-list(left_box=input$left_box,right_box=input$right_box, top_box=input$top_box, bottom_box=input$bottom_box)
                                  want_example=input$want_example
                                  return(list(coords=coords, want_example=want_example))
                                }
  )
  map_osm <- reactive(x = {
    load_map(load_example = TRUE) #User_input$want_example
  })
  matched <-reactive(x = {
    match_entities(df_tags = map_osm()$nodes$tags, df_k_mapping = df_k_mapping)
  })
  #output$class_summary<-renderPlot(draw_bar_chart(matched$class_summary))
  output$class_summary<-renderPlot(expr = {
    if(!is.null(matched()$class_summary)) draw_bar_chart(matched()$class_summary)
    })
  
  
})
