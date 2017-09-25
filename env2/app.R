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
      #   id = 'menu_var',
        menuItem(
           text='Biological', icon=icon('tree'), tabName='bio'),
        menuItem(
          text='Environmental', icon=icon('thermometer'), tabName='env', selected=T, startExpanded=T,
          menuSubItem(
            'Seascape', tabName='seascape', icon=icon("angle-double-right"), selected=T),
          menuSubItem(
            'Temperature', tabName='sst', icon=icon("angle-double-right")),
          menuSubItem(
            'Chlorophyll', tabName='chl', icon=icon("angle-double-right")))),
      box(
      #wellPanel(
        style = "background-color: #1e282c;", width=12,
        #background='#1e282c', width=8,
        #HTML('Filter'), 
        selectInput('sel_grd', 'Variable', grd_choices, multiple=F),
        uiOutput('ui_lyr'),
        selectizeInput('sel_eez', 'EEZ - Territory', c('', eez_sf$sov_ter), selected='')),
      box(
      #   background='#1e282c', width=8,
      #wellPanel(
        style = "background-color: #1e282c;", width=12,        
        #HTML('Output'),
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
      # fluidRow(
      #   box(
      #     id = 'Storyboard', width=12,
      #     bookmarkButton(),
      #     checkboxGroupInput(
      #       'countries', "Countries",
      #       choiceNames = mapply(countries, flags, FUN = function(country, flagUrl) {
      #         tagList(
      #           tags$img(src=flagUrl, width=20, height=15),
      #           country
      #         )
      #       }, SIMPLIFY = FALSE, USE.NAMES = FALSE),
      #       choiceValues = countries)))
      ))
}


#shinyServer(function(input, output, session) {
server <- function(input, output, session) {
    
  # Need to exclude the buttons from themselves being bookmarked
  setBookmarkExclude(c(
    'btn_bookmark','btn_save','btn_save_plot',
    'btn_download_doc','btn_download_htm','btn_download_pdf','btn_download_rmd'))
  
  get_s = reactive({
    req(input$sel_grd)
    
    s = stack(input$sel_grd)
    
    # dates
    attr(s, 'dates') = as_date(names(s), format='ymd_%Y.%m.%d')
    
    # update sel_lyr
    #cat(file=stderr(),'\nupdating sel_lyr: months for new s from sel_grd\n')
    month_choices = setNames(
      names(s),
      month.abb[month(attr(s, 'dates'))])
    updateSelectInput(session, 'sel_lyr', 'Month', month_choices)
    
    s
  })
  
  get_s_type = reactive({
    if (is.null(input$sel_grd))                 s_type = ''
    if (str_detect(input$sel_grd, 'chlor_a'))   s_type = 'chl'
    if (str_detect(input$sel_grd, 'seascapes')) s_type = 'seascape'
    s_type
  })
  
  output$grd_type = reactive({
    get_s_type()
  })
  outputOptions(output, 'grd_type', suspendWhenHidden=F)
  
  get_r = reactive({
    req(input$sel_grd)
    req(input$sel_lyr)
    req(get_s())
    req(input$sel_lyr %in% names(get_s()))
    
    #cat(file=stderr(), '\nget_r()\n')
    
    s = get_s()
    
    if (!input$sel_lyr %in% names(s)){
      r = NULL
    } else {
      r = raster(s, layer=input$sel_lyr)
    }
    
    r
  })
  
  output$ui_lyr <- renderUI({
    req(get_s())
    
    s = get_s()

    month_choices = setNames(
      names(s),
      month.abb[month(attr(s, 'dates'))])
    selectInput('sel_lyr', 'Month', month_choices)
  })
  
  output$map <- renderLeaflet({
    req(get_r())
    
    r = get_r()
    r_type = get_s_type()
    
    #cat(file=stderr(), sprintf('\nrenderLeaflet()\n  r_type:%s\n  sel_grd:%s\n  sel_lyr:%s\n', r_type, input$sel_grd, input$sel_lyr))
    
    if (r_type=='chl'){
      r = log(r)
      pal         = colorNumeric('Greens', values(r), na.color='transparent')
      r_group     = 'Chl'
      r_legend    = 'Chl (mg/m<sup>3</sup>)' 
      r_transform = function(x){ round(exp(x),2) }
    }
    if (r_type=='seascape'){
      pal = colorNumeric('Spectral', values(r), na.color='transparent', reverse=T)
      r_group  = 'Seascape'
      r_legend = 'Seascape Class'
      r_transform = function(x){ x }
    } 

    eez_labels <- sprintf(
      "<strong>%s</strong><br/>%g km<sup>2</sup>",
      eez_sf$Territory1, eez_sf$Area_km2
    ) %>% lapply(HTML)
    
    #browser()
    
    if (length(input$sel_eez) == 1 && input$sel_eez == ''){
      b = st_bbox(eez_sf)
    } else {
      b = st_bbox(
        eez_sf %>%
          filter(sov_ter %in% input$sel_eez)) # %>%
      # st_transform(crs_mol))
    }
    
    leaflet(
      options=leafletOptions(
        minZoom=2,
        worldCopyJump=T)) %>%
      addProviderTiles("Esri.OceanBasemap", group='Color Ocean') %>%
      addProviderTiles("Stamen.TonerLite", group='Gray Land') %>%
      addRasterImage(r, colors = pal, opacity = 0.8, project=F, group=r_group) %>%
      addPolygons(
        data=eez_sf,
        group = 'EEZ',
        layerId = ~sov_ter,
        fillColor = NA,
        weight = 2,
        opacity = 1,
        color = 'white',
        #fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          #fillOpacity = 0.7,
          bringToFront = TRUE),
        label = eez_labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(
        pal = pal, values = values(r),
        position = 'bottomright',
        labFormat = labelFormat(transform = r_transform),
        title = HTML(r_legend)) %>%
      #addMeasure() %>%
      addGraticule()  %>%
      addMiniMap(toggleDisplay=T, minimized=T, position='bottomleft')  %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
      #addDrawToolbar(
      #  targetGroup='Draw',
      #  editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>%
      #addStyleEditor() %>%
      addLayersControl(
        baseGroups    = c('Gray Land','Color Ocean'),
        #overlayGroups = c('Chl','Draw'), options = layersControlOptions(collapsed=T)) %>%
        overlayGroups = c(r_group,'EEZ'), options = layersControlOptions(collapsed=T)) %>%
    #leafletProxy('map', session) %>%
      fitBounds(b[['xmin']], b[['ymin']], b[['xmax']], b[['ymax']]) %>%
      clearGroup('eez_hi') %>%
      addPolygons(
        data = eez_sf %>%
          filter(sov_ter %in% input$sel_eez),
        group = 'eez_hi',
        fill = F,
        weight = 4,
        color = 'yellow')
  })
  
  #eez4plot = reactiveVal(NULL)
  
  output$ts_streamgraph = renderStreamgraph({
    req(input$sel_grd)
    req(input$sel_lyr)
    #req(eez4plot())
    req(input$sel_eez)

    grd = input$sel_grd
    lyr = input$sel_lyr
    #eez = eez4plot()
    eez = input$sel_eez
    s = get_s()
    date = attr(s,'dates')[which(names(s)==lyr)]
    mo = month(date)

    d_csv = sprintf('%s_eez-area-km2.csv', tools::file_path_sans_ext(grd))
    d = read_csv(d_csv)
    d = eez_sf %>%
      st_set_geometry(NULL) %>%
      left_join(d, by='MRGID')
    
    x = d %>%
      #filter(GeoName=="Spanish Exclusive Economic Zone") %>%
      filter(sov_ter==eez) %>%
      select(date, category, area_km2) %>%
      mutate(
        area_km2 = round(area_km2, 1))
    
    pal = colorNumeric('Spectral', 1:14, na.color='transparent')
    # TODO: fix palette to match raster -- streamgraph problem
    streamgraph(x, category, area_km2, date) %>%
      sg_legend(show=T, label="Class:") %>%
      sg_fill_manual(pal(1:14))
  })
  
  output$ts_dygraph = renderDygraph({
    # TODO: create *_eez-mean-sd.csv for other chl files & cache
    req(input$sel_grd)
    req(input$sel_lyr)
    #req(eez4plot())
    req(input$sel_eez)
    
    #eez = eez4plot()
    eez = input$sel_eez
    lyr = input$sel_lyr
    s = get_s()
    date = attr(s,'dates')[which(names(s)==lyr)]
    mo = month(date)

    #eez = 'Albania'
    nc_path <- "/mbon/data_big/satellite/chlor_a/clim_27km/A20032007_chlor_a_CLIM_MO_GLOB_27km.nc"
    d = read_csv(sprintf('%s_eez-mean-sd.csv', tools::file_path_sans_ext(nc_path)))
    d = eez_sf %>%
      st_set_geometry(NULL) %>%
      left_join(d, by='MRGID')
    
    x = d %>%
      filter(sov_ter==eez) %>%
      dplyr::select(ymd, mean, lwr_sd, upr_sd)
    x = xts(select(x, -ymd), order.by=x$ymd)
    
    #the axis label is passed as a date, this function outputs only the month of the date
    getMonth <- 'function(d){
      var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
      return monthNames[d.getMonth()]; }'
    
    #the x values are passed as milliseconds, turn them into a date and extract month and day
    getMonthDay <- 'function(d) {
      var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
      date = new Date(d);
      //return monthNames[date.getMonth()] + " " +date.getUTCDate(); }
      return monthNames[date.getMonth()]; }'
    
    dygraph(x, main=sprintf('Chl (2003 - 2007) for %s', eez)) %>%
      dySeries(c('lwr_sd', 'mean', 'upr_sd'), label = "Chl a") %>%
      dyAxis("x",valueFormatter=JS(getMonthDay), axisLabelFormatter=JS(getMonth)) %>%
      dyShading(from=sprintf('2005-%02d-01', mo), to=sprintf('2005-%02d-01', mo+1), color='rgb(200,200,200)')
  })
  
  # zoom to region
  observeEvent(input$map_shape_click, {
    # TODO: _mouseover, _mouseout
    updateSelectizeInput(session, 'sel_eez', 'EEZ - Territory', c('', eez_sf$sov_ter), input$map_shape_click[['id']])
  })
  
  observeEvent(input$btn_open_plot, {
    
  })
  
  #observeEvent(input$map_shape_mouseover, {
  #  eez4plot(input$map_shape_mouseover[['id']])
  #})

  # observeEvent(input$map_shape_mouseout, {
  #   eez4plot(NULL)
  # })
    
  # observeEvent(input$sel_eez, {
  #   
  #   cat(file=stderr(), sprintf('observeEvent(input$sel_eez: %s\n', input$sel_eez))
  #   
  #   eez4plot(input$sel_eez)
  #   
  #   #cat(file=stderr(), '... eez4plot(input$sel_eez)\n')
  #   
  #   browser() 
  #   
  #   if (length(input$sel_eez) == 1 && input$sel_eez == ''){
  #     b = st_bbox(eez_sf)
  #   } else {
  #     b = st_bbox(
  #       eez_sf %>%
  #         filter(sov_ter %in% input$sel_eez)) # %>%
  #     # st_transform(crs_mol))
  #   }
  #   
  #   leafletProxy('map', session) %>%
  #     fitBounds(b[['xmin']], b[['ymin']], b[['xmax']], b[['ymax']]) %>%
  #     clearGroup('eez_hi') %>%
  #     addPolygons(
  #       data = eez_sf %>%
  #         filter(sov_ter %in% input$sel_eez),
  #       group = 'eez_hi',
  #       fill = F,
  #       weight = 4,
  #       color = 'yellow')
  #   
  # })
  
  observeEvent(input$btn_save, {
    
    showModal(modalDialog(
      title='Save Plot',
      textInput('txt_plot_title', 'Title', value='Global Seascape Map'),
      textAreaInput('txt_plot_caption', 'Caption', value=''),
      HTML('TODO: associate with element in svg infographic scene'),
      footer = tagList(
        modalButton('Dismiss'),
        actionButton('btn_save_plot','Save'))))

  })
  
  observeEvent(input$btn_bookmark, {
    session$doBookmark()
  })
  
  values = reactiveValues(saved_plots = list())
  
  observeEvent(values$saved_plots, {
    req(!is_empty(values$saved_plots))
    
    removeUI(selector = '#ui_plots_sel')
    
    insertUI(
      selector = '#ui_plots',
      ui = tags$div(
        id = 'ui_plots_sel',
        selectInput('sel_plots', 'Plots', names(values$saved_plots), selectize=F, size=length(values$saved_plots))))
  })
  
  observeEvent(input$btn_open_plot, {
    req(input$sel_plots)
    
    #browser()
    url_plot = values$saved_plots[[input$sel_plots]][['url']]
    browseURL(url_plot)
    
  })
    
  observeEvent(input$btn_save_plot, {
    # session$doBookmark() -- without showBookmarkUrlModal(url)
    state <- shiny:::ShinySaveState$new(
      input = session$input, exclude = session$getBookmarkExclude())
    state$values$saved_plots <- values$saved_plots
    url <- shiny:::saveShinySaveState(state)
    
    clientData <- session$clientData
    url <- paste0(
      clientData$url_protocol, "//", clientData$url_hostname, 
      if (nzchar(clientData$url_port)) 
        paste0(":", clientData$url_port), clientData$url_pathname, 
      "?", url)
    
    #browser()
    values$saved_plots <- c(values$saved_plots, setNames(list(list('url'=url, 'caption'=input$txt_plot_caption)), input$txt_plot_title))
    cat(file=stderr(), c('values$saved_plots', str(values$saved_plots)))
    
    cat(file=stderr(), sprintf('bookmark url: %s\n', url))
    # readr::read_rds('./env2/shiny_bookmarks/a21fc9fd15caf506/input.rds')
    #browser()
    
    removeModal()
  })
  
  # onRestore(function(state) {
  #   #cat("Restoring from state bookmarked at", state$values$time, "\n")
  #   bkmrk_id = getQueryString()$`_state_id_`
  #   # bkmrk_id = '2fdd1702249423f1'; # http://127.0.0.1:3203/?_state_id_=2fdd1702249423f1
  #   input_rds = sprintf('./env2/shiny_bookmarks/%s/input.rds', bkmrk_id)
  #   input_bkmrk = readr::read_rds(input_rds)
  #   #browser()
  # })
  
  onBookmark(function(state) {
    #browser()
    state$values$saved_plots <- values$saved_plots
  })
  
  onRestore(function(state) {
    #browser()
    #values$saved_plots <- state$values$saved_plots
    #updateSelectInput(session, 'sel_eez', selected=NULL)
  })
  
  onRestored(function(state) {
    # http://127.0.0.1:3203/?_state_id_=2fdd1702249423f1

    #updateSelectInput(session, 'sel_grd', selected=NULL)
    #updateSelectInput(session, 'sel_lyr', selected=NULL)
    cat(file=stderr(), 'onRestored...\n')
    #updateSelectInput(session, 'sel_eez', selected='')
    
    #browser()
    #session$reload()
    
    # updateSelectInput(session, 'sel_grd', selected=state$input$sel_grd)
    # updateSelectInput(session, 'sel_lyr', selected=state$input$sel_lyr)
    # updateSelectInput(session, 'sel_eez', selected=state$input$sel_eez)
    #updateSelectInput(session, 'sel_eez', selected='Algeria')
    
    values$saved_plots <- state$values$saved_plots
    
  })
  
  
}

shinyApp(ui, server, enableBookmarking='server')