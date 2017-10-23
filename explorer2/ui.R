dashboardPage(
  title = 'SDG14: Life in the Sea',
  dashboardHeader(
    title=tagList(icon('tint'), 'SDG14: Life in the Sea'),
    titleWidth = 250),
  dashboardSidebar(
    width = 250,
    # side menu ----
    sidebarMenu(
      id = 'sel_menu',
      
      menuItem(
        HTML('<span class="icon-octopus"></span> &nbsp;&nbsp; Biological'), tabName='bio', selected=T, startExpanded=T),
      menuItem(
        'Environmental', tabName = 'env', icon=icon('thermometer'))),
    
    # side bio conditional ----
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
    
    # side env conditional ----
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
    
    # side eez ----
    selectizeInput('sel_eez', 'EEZ - Territory', c('', eez_sf$sov_ter), selected=''),
    
    # side save ----
    actionButton('btn_save', 'Save Plot', icon=icon('save')),
    tags$div(id='ui_plots'),
    conditionalPanel(
      condition = "input.sel_plots != null",               
      div(
        style='margin: 6px 5px 6px 15px;',
        actionButton('btn_open_plot', 'Open Plot', icon=icon('folder-open-o')),
        dropdownButton(
          label = 'Save Report', icon=icon('file-o'), circle=F, size='sm',
          #bookmarkButton(id='btn_bookmark'),
          actionButton('btn_download_url', 'Bookmark (url)', icon=icon('bookmark-o')),
          downloadButton('btn_download_pdf', 'Portable (*.pdf)', icon=icon('file-pdf-o')),
          downloadButton('btn_download_doc', 'Word (*.docx)', icon=icon('file-word-o')),
          downloadButton('btn_download_htm', 'Web (*.html)', icon=icon('file-text-o')))))),
  
  #selectInput(
  # 'sel_temporal', 'Temporal:', 
  # c('Contemporaneous','Climatological')))),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "icomoon.io_mbon/style.css")),
    
    tabItems(
      
      # body env ----
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
         
      # body bio ---- 
      tabItem(
        tabName='bio',
        
        tabBox(
          id = 'tabset_bio_viz', width=12,
          
          tabPanel(
            'Spatial',
            leafletOutput('map_bio', height = 550)))))))
