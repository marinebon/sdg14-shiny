# user interface
shinyUI(fluidPage(
   
   titlePanel('Environmental Explorer'),
   
   sidebarLayout(
      sidebarPanel(
        selectInput('sel_env', 'Environmental Data', env_files, multiple=F),
        selectizeInput('sel_eez', 'EEZ - Territory', c('', eez$sov_ter), selected=''), #, multiple=F),
        dygraphOutput('ts_plot')
      ),
      mainPanel(
        leafletOutput('map', height = 550) #,
        #uiOutput('test')
      )
   )
))
