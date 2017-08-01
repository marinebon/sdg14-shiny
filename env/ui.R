# user interface
shinyUI(fluidPage(
   
   titlePanel('Environmental Explorer'),
   
   sidebarLayout(
      sidebarPanel(
        selectInput('sel_nc', 'NetCDF', env_files, multiple=F),
        uiOutput('ui_var'),               
        selectizeInput('sel_eez', 'EEZ - Territory', c('', eez_sf$sov_ter), selected=''), #, multiple=F),
        dygraphOutput('ts_plot')
      ),
      mainPanel(
        leafletOutput('map', height = 550) #,
        #uiOutput('test')
      )
   )
))
