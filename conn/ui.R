shinyUI(fluidPage(
  
  titlePanel("Larval Connectivity Explorer"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput('sel_dir','Direction',c('Import','Export')),
      actionButton("update", "Update")),
    
    mainPanel(
      leafletOutput("map")))

))
