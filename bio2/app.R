# TODO: seascapes, per https://marinebon.github.io/seascape-viz/prep.html & /mbon/data_big/satellite/seascapes/*

source('global.R')

# user interface, enableBookmarking
ui <- function(request) {
  dashboardPage(
    title = 'SDG14: Life in the Sea',
    dashboardHeader(
      title=tagList(icon('tint'), 'SDG14: Life in the Sea'),
      titleWidth = 250),
    dashboardSidebar(
      width = 250,
      # TODO: For dynamically-generated sidebar menus, see renderMenu and sidebarMenuOutput.
      sidebarMenu(
        id = 'menu_var',
        menuItem(
           text='Biological', icon=icon('tree'), tabName='bio', selected=T, startExpanded=T,
          menuSubItem(
            'Species Richness', tabName='n_spp', icon=icon("angle-double-right")),
          menuSubItem(
            '# of Observations', tabName='n_obs', icon=icon("angle-double-right")),
          menuSubItem(
            'Protection Metric', tabName='idx_obis_wdpa', icon=icon("angle-double-right"))),        
        menuItem(
          text='Environmental', icon=icon('thermometer'), tabName='env', selected=F, startExpanded=F,
          menuSubItem(
            'Seascape', tabName='seascape', icon=icon("angle-double-right"), selected=F),
          menuSubItem(
            'Temperature', tabName='sst', icon=icon("angle-double-right")),
          menuSubItem(
            'Chlorophyll', tabName='chl', icon=icon("angle-double-right")))),
      box(
        style = "background-color: #1e282c;", width=12,
        selectInput(
          'rank', label = 'Taxa - Rank:', width='100%',
          taxa_ranks, multiple=F), 
        selectInput(
          'taxa', label = 'Taxa - Values:', width='100%',
          unique(eez_taxa[['taxonomicgroup']]), multiple=T),
        selectizeInput('sel_eez', 'EEZ - Territory', c('', eez_territories), selected='')),
      box(
        style = "background-color: #1e282c;", width=12,        
        actionButton('btn_save', 'Save Plot', icon=icon('save')),
        tags$div(id='ui_plots'),
        conditionalPanel(
          condition = "input.sel_plots != null",               
          div(
            style='margin: 6px 5px 6px 15px;',
            #class='section.sidebar shiny-bound-input.action-link',
            actionButton('btn_open_plot', 'Open Plot', icon=icon('folder-open-o')),
            dropdownButton(
              label = 'Download Report', icon=icon('file-o'), circle=F, size='sm',
              bookmarkButton(id='btn_bookmark'),
              #actionButton('btn_download_url', 'Bookmark (url)', icon=icon('bookmark-o')),
              actionButton('btn_download_pdf', 'Portable (*.pdf)', icon=icon('file-pdf-o')),
              actionButton('btn_download_doc', 'Word (*.docx)', icon=icon('file-word-o')),
              actionButton('btn_download_htm', 'Web (*.html)', icon=icon('file-text-o')),
              actionButton('btn_download_rmd', 'Rmarkdown (*.rmd)', icon=icon('file-code-o'))))))),
    dashboardBody(
      fluidRow(
        tabBox(
          id = 'tabset_viz', width=12,
          tabPanel(
            #tagList(icon('globe'), 'Spatial'),
            'Spatial',
            leafletOutput('map', height = 550)),
          tabPanel(
            #tagList(icon('line-chart'), 'Temporal'),
            'Temporal',
            conditionalPanel(
              condition = "output.grd_type=='chl'",
              dygraphOutput('ts_dygraph')),
            conditionalPanel(
              condition = "output.grd_type=='seascape'",
              streamgraphOutput('ts_streamgraph')))))#,
      ))
}


#shinyServer(function(input, output, session) {
server <- function(input, output, session) {
    
  # exclude buttons from themselves being bookmarked
  setBookmarkExclude(c(
    'btn_bookmark','btn_save','btn_save_plot',
    'btn_download_doc','btn_download_htm','btn_download_pdf','btn_download_rmd'))

  get_eez_sf = reactive({
    
    if (is.null(input$taxa) || input$taxa == ''){
    
      eez_sf = read_sf(eez_s005005_shp) %>%
        filter(Pol_type=='200NM') %>%
        left_join(
          eez_metrics,
          by = c('Territory1'='eez_territory1')) # plot(eez_sf['n_obs'])
    } else {
      
      req(input$rank)

      eez_sf = read_sf(eez_s005005_shp) %>%
        filter(Pol_type=='200NM') %>%
        left_join(
          eez_taxa %>%
            group_by_(.dots = list('Territory1', input$rank)) %>%
            filter_(.dots = list(paste0(input$rank, " %in% '", input$taxa, "'"))) %>%
            summarize(
              n_obs         = sum(n_obs),
              n_spp         = sum(n_spp),            # TODO: fix n_spp!
              idx_obis_wdpa = first(idx_obis_wdpa)),
          by='Territory1') # TODO: fix idx_obis_wdpa!
    }
    eez_sf
  })
  
  # leaflet map ----  
  output$map <- renderLeaflet({
    
    eez_sf = get_eez_sf()
    
    #browser()
    var = input$menu_var # var = 'idx_obis_wdpa'

    pal = colorNumeric('Spectral', eez_sf[[var]])
    
    eez_labels <- sprintf(
      "<strong>%s</strong><br/>%s: %g",
      eez_sf$Territory1, var, eez_sf[[var]]
    ) %>% lapply(HTML)
    
    if (length(input$sel_eez) == 1 && input$sel_eez == ''){
      b = eez_sf %>%
        filter(Territory1 %in% eez_territories) %>%
        st_bbox()
    } else {
      b = eez_sf %>%
        filter(Territory1 %in% input$sel_eez) %>%
        st_bbox()
    }
    
    leaflet(
      options=leafletOptions(
        minZoom=2,
        worldCopyJump=T)) %>%
      addProviderTiles("Esri.OceanBasemap", group='Color Ocean') %>%
      addProviderTiles("Stamen.TonerLite", group='Gray Land') %>%
      addPolygons(
        data=eez_sf,
        group = 'Bio metric',
        layerId = ~Territory1,
        fillColor = ~pal(eez_sf[[var]]),
        fillOpacity = 0.7,
        weight = 2,
        opacity = 1,
        color = 'white',
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          bringToFront = TRUE),
        label = eez_labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(
        title=HTML(var),
        pal = pal, values = eez_sf[[var]], opacity = 0.7,
        position = "bottomright") %>%
      addGraticule()  %>%
      #addMiniMap(toggleDisplay=T, minimized=T, position='bottomleft')  %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
      addLayersControl(
        baseGroups    = c('Gray Land','Color Ocean'),
        overlayGroups = c('Bio metric'), options = layersControlOptions(collapsed=T)) %>%
      fitBounds(b[['xmin']], b[['ymin']], b[['xmax']], b[['ymax']]) %>%
      clearGroup('eez_hi') %>%
      addPolygons(
        data = eez_sf %>%
          filter(Territory1 %in% input$sel_eez),
        group = 'eez_hi',
        fill = F,
        weight = 4,
        color = 'yellow')
    
  })
  
  # zoom to eez ----
  observeEvent(input$map_shape_click, {
    updateSelectizeInput(session, 'sel_eez', 'Territory', c('', eez_territories), input$map_shape_click[['id']])
  })
  
  # update taxa with rank ----
  observe({
    
    if (is.null(input$rank)){
      x <- character(0)
    } else {
      x <- unique(eez_taxa[[input$rank]])
    }
    
    updateSelectInput(
      session, "taxa",
      choices = x)
  })
}

shinyApp(ui, server, enableBookmarking='server')