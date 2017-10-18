dashboardPage(
  title = 'SDG14: env WMS prototype',
  dashboardHeader(
    title=tagList(icon('tint'), 'SDG14: Life in the Sea'),
    titleWidth = 250),
  dashboardSidebar(
    width = 250, 
    sidebarMenu(
      id = 'sel_menu',
      
      menuItem(
        'Biological', tabName='bio', icon=icon('tree')),
      menuItem(
        'Environmental', tabName = 'env', icon=icon('thermometer'), selected=T, startExpanded=T)),
    
    conditionalPanel(
      condition = "input.sel_menu == 'bio'",
      radioButtons(
        'sel_bio_var', label=NULL,
        choices = list(
          'Species Richness'='n_spp',
          '# of Observations'='n_obs',
          'Protection Metric'='idx_obis_wdpa'),
        selected = 'n_spp')),
    
    conditionalPanel(
      condition = "input.sel_menu == 'env'",
      radioButtons(
        'sel_env_var', label=NULL,
        choiceValues = list('chl','seascape','sst'),
        choiceNames = list(
          'Chlorophyll',
          tagList(
            span('Seascape', 
            actionLink(
              'show_seascape_info', 
              label=NULL, 
              icon=icon('question-circle', class='fa-li')))),
          'Temperature'),
        selected = 'sst'))),
  
  #selectInput(
  # 'sel_temporal', 'Temporal:', 
  # c('Contemporaneous','Climatological')))),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    tabItems(
      tabItem(
        tabName='env', fluidRow(
        tabBox(
          id = 'tabset_viz', width=12,
          tabPanel(
            'Spatial',
            leafletOutput('map_env', height = 550),
            sliderInput(
              'sel_env_ym', 'Date:', 
              min = dates[length(dates)], max = dates[1], value = dates[1],
              step = 30, animate = animationOptions(interval = 2000), 
              timeFormat='%b %Y', width='100%'))))), #)),
      tabItem(
        tabName='bio',
        h2('bio tab')))))
