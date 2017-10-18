# user interface
shinyUI(fluidPage(
   
   titlePanel('Satellite Explorer'),
   
   sidebarLayout(
      sidebarPanel(
        selectInput('sel_grd', 'Variable', grd_choices, multiple=F),
        uiOutput('ui_lyr'),               
        selectizeInput('sel_eez', 'EEZ - Territory', c('', eez_sf$sov_ter), selected=''), #, multiple=F),
        conditionalPanel(
          condition = "output.grd_type=='chl'",
          dygraphOutput('ts_dygraph')),
        conditionalPanel(
          condition = "output.grd_type=='seascape'",
            streamgraphOutput('ts_streamgraph'))),
      mainPanel(
        leafletOutput('map', height = 550) #,
        #uiOutput('test')
      )
   )
))
