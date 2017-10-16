dashboardPage(
  title = 'SDG14: env WMS prototype',
  dashboardHeader(
    title=tagList(icon('tint'), 'SDG14: Life in the Sea'),
    titleWidth = 250),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      #id = 'menu_var',
      menuItem(
        text='Environmental', icon=icon('thermometer'), tabName='env', selected=T, startExpanded=T,
        menuSubItem(
          'Seascape', tabName='seascape', icon=icon("angle-double-right"), selected=T),
        menuSubItem(
          'Temperature', tabName='sst', icon=icon("angle-double-right")),
        menuSubItem(
          'Chlorophyll', tabName='chl', icon=icon("angle-double-right"))),
      selectInput(
        'sel_temporal', 'Temporal:', 
        c('Contemporaneous','Climatological')))),
  dashboardBody(
    fluidRow(
      tabBox(
        id = 'tabset_viz', width=12,
        tabPanel(
          'Spatial',
          leafletOutput('map', height = 550),
          sliderInput(
            'sel_ym', 'Date:', 
            min = dates[length(dates)], max = dates[1], value = dates[1],
            step = 30, animate = animationOptions(interval = 2000), 
            timeFormat='%b %Y', width='100%'))))))
