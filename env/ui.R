# user interface
shinyUI(fluidPage(
   
   titlePanel('Environmental Explorer'),
   
   sidebarLayout(
      sidebarPanel(
        selectInput('sel_env', 'Environmental Data', env_files, multiple=F)),
      mainPanel(
        leafletOutput('map', height = 550) #,
        #uiOutput('test')
      )
   )
))
