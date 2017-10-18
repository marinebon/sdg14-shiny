library(leaflet)
library(leaflet.extras)
library(shiny)
library(sf)

ui <- leafletOutput("leafmap")

server <- function(input, output, session) {
  ply = read_sf('../conn/tmp_ply.geojson') %>% as('Spatial')
  
  output$leafmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite) %>%
      addPolygons(data=ply) %>%
      addDrawToolbar(editOptions = editToolbarOptions())
  })
  
  # Start of Drawing
  observeEvent(input$leafmap_draw_start,{
    print("Start of drawing")
    #print(input$leafmap_draw_start)
  })
  
  # Stop of Drawing
  observeEvent(input$leafmap_draw_stop,{
    print("Stopped drawing")
    #print(input$leafmap_draw_stop)
  })
  
  # New Feature
  observeEvent(input$leafmap_draw_new_feature,{
    print("New Feature")
    #print(input$leafmap_draw_new_feature)
  })
  
  # Edited Features
  observeEvent(input$leafmap_draw_edited_features,{
    print("Edited Features")
    #print(input$leafmap_draw_edited_features)
  })
  
  # Deleted features
  observeEvent(input$leafmap_draw_deleted_features,{
    print("Deleted Features")
    #print(input$leafmap_draw_deleted_features)
  })
  
  # We also listen for draw_all_features which is called anytime
  # features are created/edited/deleted from the map
  observeEvent(input$leafmap_draw_all_features,{
    print("All Features")
    #print(input$leafmap_draw_all_features)
  })
}

shinyApp(ui, server)