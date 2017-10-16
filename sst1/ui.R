shinyUI(fluidPage(
  
  titlePanel("Environmental mapper prototype using WMS on GeoServer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        'sel_env', 'Variable:',
        c('Seascape','SST','Chl a')),
      selectInput(
        'sel_temporal', 'Temporal:', 
        c('Climatological', 'Contemporaneous'))),

    mainPanel(
      leafletOutput('map', height = 550),
      sliderInput(
        'sel_ym', 'Date', 
        min = dates[length(dates)], max = dates[1], value = dates[1],
        step = 30, animate = T, timeFormat='%b %Y', width='100%')))
))
