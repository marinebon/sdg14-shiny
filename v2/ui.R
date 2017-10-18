# user interface
dashboardPage(
  dashboardHeader(title='MBON SDG14 App'),
  dashboardSidebar(
    selectInput('sel_grd', 'Variable', grd_choices, multiple=F),
    uiOutput('ui_lyr'),               
    selectizeInput('sel_eez', 'EEZ - Territory', c('', eez_sf$sov_ter), selected='')),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(
        tabName ='Global',
        fluidRow(
          box(
            title='Plot',
            conditionalPanel(
              condition = "output.grd_type=='chl'",
              dygraphOutput('ts_dygraph')),
            conditionalPanel(
              condition = "output.grd_type=='seascape'",
              streamgraphOutput('ts_streamgraph')),
            collapsible=T),
          box(
            title='Map',
            leafletOutput('map', height = 550),
            collapsible=T))),
      tabItem(
        tabName ='Local',
        fluidRow(
          box(
            title='Infographic',
            '...')))) ))