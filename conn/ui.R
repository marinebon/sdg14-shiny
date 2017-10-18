shinyUI(fluidPage(
  
  titlePanel("Larval Connectivity Explorer"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput('sel_dir','Direction', c('Import','Export')),
      selectInput('sel_sanctuary','Sanctuary', c('Monterey Bay'='mbnms','Florida Keys'='fknms')),
      selectInput('sel_yr','Year', c('Normal (2009)'=2009)), # 'El Niño(2010)'=2010,'La Niña(2011)'=2011
      selectInput('sel_pld','Pelagic larval duration (days)', c(10,30,50)),
      selectInput('sel_buf_cel','Radius Extent/Cell Size (km)', c('300/27','500/45')),
      selectInput('sel_date','Date', c('01_25_2009','04_24_2009','07_21_2009','10_17_2009')),
  
      checkboxInput('ck_self','Self-recruitment')),
      #actionButton("update", "Update")),
    
    mainPanel(
      leafletOutput("map")))

))
