shinyUI(fluidPage(
  
  titlePanel("Larval Connectivity Explorer"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput('sel_dir','Direction',c('Import','Export')),
      checkboxInput('ck_self','Self-recruitment')),
      #actionButton("update", "Update")),
    
    mainPanel(
      leafletOutput("map")))

))
