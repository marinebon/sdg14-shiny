# user interface
shinyUI(fluidPage(
   
   titlePanel('Marine Biodiversity'),
   
   sidebarLayout(
      sidebarPanel(
        selectizeInput('sel_eez', 'EEZ - Territory', c('', eez$sov_ter), selected='', multiple=T)),
      mainPanel(
        leafletOutput('map', height = 550)
      )
   )
))
