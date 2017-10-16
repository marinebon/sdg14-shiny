shinyUI(fluidPage(
  
  titlePanel("Old Faithful Geyser Data"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        'sel_ym', 'Date', 
        min = dates[length(dates)], max = dates[1],
        value = dates[1],
        step = 30, animate = T,
        timeFormat='%b %Y')),

    mainPanel(
      leafletOutput('map', height = 550))))
)
