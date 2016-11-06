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
                                  coords<-list(left=input$left_box,right=input$right_box, top=input$top_box, bottom=input$bottom_box)
                                  want_example=input$want_example
                                  return(list(coords=coords, want_example=want_example))
                                }
  )
  map_osm <- reactive(x = {
    withProgress(message = 'Map is loading',
                 detail = 'This may take a while...', value = 0, {
                   map<-load_map(load_example = user_input()$want_example, coord_list = user_input()$coords ) #User_input$want_example
                   for (i in 1:15) incProgress(1/15)
                   return(map)
                 })
  })
  matched <-reactive(x = {
    match_entities(df_tags = map_osm()$nodes$tags, df_k_mapping = df_k_mapping)
  })
  output$class_summary<-renderPlot({
    draw_bar_chart(matched()$class_summary)
    })
  #output$class_summary<-renderUI(draw_bar_chart(matched()$class_summary))
  #output$class_summary<-renderUI(expr = {
  #  if(!is.null(matched()$class_summary)) {
  #    chart<-draw_bar_chart(matched()$class_summary)
  #    loader<-NULL
  #    } else{
  #      loader<-shiny::tags$div(class="loader", style="margin: auto;")
  #    } 
  #  return(shiny::tagList(chart,loader))
  #  })
  
  
})
