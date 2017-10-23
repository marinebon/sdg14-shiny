dashboardPage(
  title = 'SDG14: Life in the Sea',
  dashboardHeader(
    title=tagList(icon('tint'), 'SDG14: Life in the Sea'),
    titleWidth = 250),
  dashboardSidebar(
    width = 250, 
    sidebarMenu(
      id = 'sel_menu',
      
      menuItem(
        'Biological', tabName='bio', icon=icon('tree'), selected=T, startExpanded=T),
      menuItem(
        'Environmental', tabName = 'env', icon=icon('thermometer'))),
    
    conditionalPanel(
      condition = "input.sel_menu != 'env'",
      radioButtons(
        'sel_bio_var', label=NULL,
        choices = list(
          'Species Richness'  = 'n_spp',
          '# of Observations' = 'n_obs',
          'Protection Metric' = 'idx_obis_wdpa'),
        selected = bio_var),
      selectInput(
        'rank', label = 'Taxa - Rank:', width='100%',
        taxa_ranks, multiple=F), 
      selectInput(
        'taxa', label = 'Taxa - Values:', width='100%',
        unique(eez_taxa[['taxonomicgroup']]), multiple=T)),
    
    conditionalPanel(
      condition = "input.sel_menu == 'env'",
      radioButtons(
        'sel_env_var', label=NULL,
        choiceValues = list('chl','seascape','sst'),
        choiceNames = list(
          'Chlorophyll',
          tagList(
            span('Seascape',class="control-label", 
            actionLink(
              'show_seascape_info', 
              label=NULL, 
              icon=icon('question-circle', class='fa-li')))),
          'Temperature'),
        selected = env_var)),
    
    selectizeInput('sel_eez', 'EEZ - Territory', c('', eez_sf$sov_ter), selected='')),
  
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
          id = 'tabset_env_viz', width=12,
          
          tabPanel(
            'Spatial',
            leafletOutput('map_env', height = 550),
            sliderInput(
              'sel_env_ym', 'Date:', 
              min = dates[length(dates)], max = dates[1], value = min(dates),
              step = 30, animate = animationOptions(interval = 2000), 
              timeFormat='%b %Y', width='100%')), #)),
          
          tabPanel(
            #tagList(icon('line-chart'), 'Temporal'),
            'Temporal',
            conditionalPanel(
              condition = "input.sel_env_var != 'seascape'",
              dygraphOutput('env_ts_dygraph')),
            conditionalPanel(
              condition = "input.sel_env_var == 'seascape'",
              streamgraphOutput('env_ts_streamgraph')))))),
          
      tabItem(
        tabName='bio',
        
        tabBox(
          id = 'tabset_bio_viz', width=12,
          
          tabPanel(
            'Spatial',
            leafletOutput('map_bio', height = 550)))))))
