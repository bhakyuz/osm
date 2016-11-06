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
  matched_nodes <-reactive(x = {
    match_entities(df_tags = map_osm()$nodes$tags, df_k_mapping = df_k_mapping)
  })
  output$nodes_general <- renderUI(expr = {
    shiny::tagList(
      shiny::h4("Nodes",align = "center"),
      shiny::h5(paste("Total of", matched_nodes()$nb_of_unique_osm_id, "unique nodes"),align = "center"),
      shiny::h5(paste("with", matched_nodes()$nb_of_tags, "OSM tags"),align = "center")
    )
  })
  output$nodes_class_summary<-renderPlot({
    draw_bar_chart(matched_nodes()$class_summary)
    })
  output$nodes_prop_summary<-renderPlot({
    draw_bar_chart(matched_nodes()$prop_summary)
  })
  matched_ways <-reactive(x = {
    match_entities(df_tags = map_osm()$ways$tags, df_k_mapping = df_k_mapping)
  })
  output$ways_general <- renderUI(expr = {
    shiny::tagList(
      shiny::h4("Ways",align = "center"),
      shiny::h5(paste("Total of", matched_ways()$nb_of_unique_osm_id, "unique ways"),align = "center"),
      shiny::h5(paste("with", matched_ways()$nb_of_tags, "OSM tags"),align = "center")
    )
  })
  output$ways_class_summary<-renderPlot({
    draw_bar_chart(matched_ways()$class_summary)
  })
  output$ways_prop_summary<-renderPlot({
    draw_bar_chart(matched_ways()$prop_summary)
  })
  matched_relations <-reactive(x = {
    match_entities(df_tags = map_osm()$relations$tags, df_k_mapping = df_k_mapping)
  })
  output$relations_general <- renderUI(expr = {
    shiny::tagList(
      shiny::h4("Relations",align = "center"),
      shiny::h5(paste("Total of", matched_relations()$nb_of_unique_osm_id, "unique relations"),align = "center"),
      shiny::h5(paste("with", matched_relations()$nb_of_tags, "OSM tags"),align = "center")
    )
  })
  output$relations_class_summary<-renderPlot({
    draw_bar_chart(matched_relations()$class_summary)
  })
  output$relations_prop_summary<-renderPlot({
    draw_bar_chart(matched_relations()$prop_summary)
  })
  
  
  
})