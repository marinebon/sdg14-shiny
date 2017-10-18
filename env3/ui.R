dashboardPage(
  title = 'SDG14: env WMS prototype',
  dashboardHeader(
    title=tagList(icon('tint'), 'SDG14: Life in the Sea'),
    titleWidth = 250),
  dashboardSidebar(
    width = 250, 
    sidebarMenu(
      id = 'sel_var',
      menuItem(
        text='Environmental', icon=icon('thermometer'), startExpanded=T, expandedName='env',
        menuSubItem(
          tabName='seascape', text='Seascape', icon=icon("angle-double-right")),
        menuSubItem(
          tabName='sst', text='Temperature', icon=icon("angle-double-right"), selected=T),
        menuSubItem(
          tabName='chl', text='Chlorophyll', icon=icon("angle-double-right"))))),
      #selectInput(
        # 'sel_temporal', 'Temporal:', 
        # c('Contemporaneous','Climatological')))),
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
