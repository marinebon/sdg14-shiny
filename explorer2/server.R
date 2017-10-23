shinyServer(function(input, output, session) {
  
  # env: map ----
  output$map_env <- renderLeaflet({
    msg('leaflet map_env, initial - renderLeaflet()')
    
    leaflet(
      options=c(
        leafletOptions(
          minZoom=2,
          worldCopyJump=T),
        attributionControl=F)) %>%
      addProviderTiles("Stamen.TonerLite", group='Gray Land') %>% 
      # eez
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
      # env
      addWMSTiles(
        baseUrl = 'http://mbon.marine.usf.edu:8080/geoserver/satellite/wms',
        group = 'env', layers = env_vars[[env_var]][['curr_lyr']],
        options = WMSTileOptions(
          version = '1.3.0', format  = 'image/png', transparent = T,
          time    = dates[1]))
  })
  
  # bio: map ----
  output$map_bio <- renderLeaflet({
    msg('leaflet map_bio - renderLeaflet()')
    
    bio_var = get_bio_var()
    
    pal = colorNumeric('Spectral', eez_sf[[bio_var]])
    
    eez_labels <- sprintf(
      "<strong>%s</strong><br/>%s: %g",
      eez_sf$Territory1, bio_var, eez_sf[[bio_var]]
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
        fillColor = ~pal(eez_sf[[bio_var]]),
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
        title=HTML(c('n_spp'='# species','n_obs'='# obs','idx_obis_wdpa'='protection<br>metric')[bio_var]),
        pal = pal, values = eez_sf[[bio_var]], opacity = 0.7,
        position = "bottomright") %>%
      #addGraticule()  %>%
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
  
  # env: sel_env_ym update ----
  observe({
    msg('updateSliderInput()')
    
    dates = get_env_dates()
    updateSliderInput(
      session, 'sel_env_ym', 'Date:', 
      min = dates[length(dates)], max = dates[1], 
      value = min(dates))
  })
  
  # bio: reactives ----
  get_bio_var = reactive({
    if (!is.null(input$sel_bio_var))
      bio_var = input$sel_bio_var
    bio_var
  })
  
  # bio: taxa update ----
  observe({
    
    if (is.null(input$rank)){
      x <- character(0)
    } else {
      x <- unique(eez_taxa[[input$rank]])
    }
    
    updateSelectInput(
      session, 'taxa',
      choices = x)
  })
  
  # env: reactives ----
  get_env_var = reactive({
    if (!is.null(input$sel_env_var))
      env_var = input$sel_env_var
    env_var
  })
  
  get_env_dates = reactive({
    if (!is.null(input$sel_env_var))
      env_var = input$sel_env_var
    env_vars[[env_var]][['curr_dates']]
  })
  
  get_env_ymd = reactive({
    if (!is.null(input$sel_env_ym)){
      ymd =  sprintf('%s-15', str_sub(as.character(input$sel_env_ym), 1,7))
    } else {
      if (!is.null(input$sel_env_var))
        env_var = input$sel_env_var
      ymd = min(env_vars[[env_var]][['curr_dates']])
    }
    ymd
  })
  
  get_env_eez = reactive({
    req(input$sel_eez)
    
    eez_mrgids = eez_sf %>%
      st_set_geometry(NULL) %>%
      filter(sov_ter == input$sel_eez) %>%
      .$MRGID
    
    env_vars[[get_env_var()]][['curr_eez']] %>%
      filter(eez_mrgid %in% eez_mrgids)
  })
    
  # env: map update ----
  observe({
    msg('update env WMSTiles - leafletProxy()')

    # ensure date match with time slice
    #msg(sprintf('  input$sel_menu=%s, get_env_var()="%s", get_env_ymd()="%s"', input$sel_menu, get_env_var(), get_env_ymd()))
    v = env_vars[[get_env_var()]]
    
    # update env WMSTile
    leafletProxy('map_env') %>%
      clearGroup('env') %>% 
      clearControls() %>% # remove legend
      addWMSTiles(
        baseUrl = 'http://mbon.marine.usf.edu:8080/geoserver/satellite/wms',
        group = 'env', layers = env_vars[[get_env_var()]][['curr_lyr']],
        options = WMSTileOptions(
          version = '1.3.0', format  = 'image/png', transparent = T,
          time    = get_env_ymd())) %>%
      addLegend(
        pal = v$pal, values = v$values,
        position = 'bottomright',
        labFormat = labelFormat(transform = v$transform),
        title = v$legend)
    
    
  })
  
  
  # eez: zoom ----
  observe({
    msg(sprintf('zoom to eez, input$sel_eez: %s', input$sel_eez))
  
    if (length(input$sel_eez) == 1 && input$sel_eez == ''){
      b = st_bbox(eez_sf)
    } else {
      b = st_bbox(
        eez_sf %>%
          filter(sov_ter %in% input$sel_eez)) # %>%
    }
    
    # TODO: based on default menu, only zoom to visible map
    #browser()
    #msg(sprintf('sel_menu: %s, sidebarItemExpanded: %s', input$sel_menu, input$sidebarItemExpanded))
    
    leafletProxy('map_env') %>%    
      fitBounds(b[['xmin']], b[['ymin']], b[['xmax']], b[['ymax']]) %>%
      clearGroup('eez_hi') %>%
      addPolygons(
        data = eez_sf %>%
          filter(sov_ter %in% input$sel_eez),
        group = 'eez_hi',
        fill = F,
        weight = 4,
        color = 'yellow')
    
    leafletProxy('map_bio') %>%    
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
  
  observeEvent(input$map_bio_shape_click, {
    updateSelectizeInput(session, 'sel_eez', 'EEZ - Territory', c('', eez_sf$sov_ter), input$map_bio_shape_click[['id']])
  })
  
  observeEvent(input$map_env_shape_click, {
    updateSelectizeInput(session, 'sel_eez', 'EEZ - Territory', c('', eez_sf$sov_ter), input$map_env_shape_click[['id']])
  })
  
  # env: streamgraph ----
  output$env_ts_streamgraph = renderStreamgraph({
    req(input$sel_eez)
    req(get_env_var()=='seascape')
    
    x = get_env_eez() %>%
      mutate(
        ymd = ymd(str_replace(raster, '^r_', ''))) %>%
      select(ymd, class=value, area_km2) %>%
      arrange(ymd, class)

    # TODO: fix palette to match raster -- streamgraph problem
    # TODO: figure out time zoom in/out for streamgraph, possibly combined w/ dygraph
    streamgraph(x, 'class', 'area_km2', 'ymd') %>%
      sg_legend(show=T, label="Class:") %>%
      sg_fill_manual(env_vars$seascape$pal(1:14))
  })
  
  # env: dygraph ----
  output$env_ts_dygraph = renderDygraph({
    req(input$sel_eez)
    req(get_env_var()!='seascape')
    
    var  = get_env_var()
    v    = env_vars[[var]]
    cols = c('chl'='green', 'sst'='red')[[var]]
    
    x = get_env_eez() %>%
      mutate(
        ymd    = ymd(str_replace(raster, '^r_', '')),
        mean   = v$transform(mean),
        lwr_sd = mean - sd,
        upr_sd = mean + sd) %>%
      select(ymd, mean, lwr_sd, upr_sd) %>%
      arrange(ymd)
    
    # TODO: apply chl transform
    
    x = xts(select(x, -ymd), order.by=x$ymd)
    
    dygraph(x, main=sprintf('%s for %s', v$dy_title, input$sel_eez)) %>%
      dySeries(c('lwr_sd', 'mean', 'upr_sd')) %>%
      dyAxis('y', label = v$dy_lab) %>%
      dyOptions(colors = cols)
    
    # TODO: use months for climatology
    #the axis label is passed as a date, this function outputs only the month of the date
    # getMonth <- 'function(d){
    #   var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
    #   return monthNames[d.getMonth()]; }'
    
    #the x values are passed as milliseconds, turn them into a date and extract month and day
    # getMonthDay <- 'function(d) {
    #   var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
    #   date = new Date(d);
    #   //return monthNames[date.getMonth()] + " " +date.getUTCDate(); }
    #   return monthNames[date.getMonth()]; }'
    
    
    # %>%
      #dyAxis("x",valueFormatter=JS(getMonthDay), axisLabelFormatter=JS(getMonth)) # %>%
      # TODO: sync with env date slider
      #dyShading(from=sprintf('2005-%02d-01', mo), to=sprintf('2005-%02d-01', mo+1), color='rgb(200,200,200)')
  })
  
  # save: button ----
  observeEvent(input$btn_save, {
    
    showModal(modalDialog(
      title='Save Plot',
      textInput('txt_plot_title', 'Title', value='Global Seascape Map'),
      textAreaInput('txt_plot_caption', 'Caption', value=''),
      #HTML('TODO: associate with element in svg infographic scene'),
      footer = tagList(
        modalButton('Dismiss'),
        actionButton('btn_save_plot','Save'))))
    
  })
  
  # save: reactives ----
  values = reactiveValues(saved_plots = list())

  plot_titles = function(){
    values$saved_plots %>% map_chr(function(x) x$txt_plot_title) }
  
  # save: ui_plots ----
  observeEvent(values$saved_plots, {
    req(!is_empty(values$saved_plots))
    
    removeUI(selector = '#ui_plots_sel')
    
    #browser()
    titles = plot_titles()
  
    insertUI(
      selector = '#ui_plots',
      ui = tags$div(
        id = 'ui_plots_sel',
        selectInput('sel_plots', 'Plots', titles, selectize=F, size=length(titles))))
  })
  
  # save: modal ----
  observeEvent(input$btn_save_plot, {
    
    p = list(
      txt_plot_title   = input$txt_plot_title,
      txt_plot_caption = input$txt_plot_caption,
      sel_eez          = input$sel_eez)
    
    values$saved_plots <- c(values$saved_plots, list(p))
    #cat(file=stderr(), c('values$saved_plots', str(values$saved_plots)))

    removeModal()
  })
  
  # save: bkmark() ----
  bkmark = function(session){
    # return url
    
    # session$doBookmark() -- without showBookmarkUrlModal(url)
    state <- shiny:::ShinySaveState$new(
      input = session$input, exclude = session$getBookmarkExclude())
    state$values$saved_plots <- values$saved_plots
    state$values$saved_time  <- Sys.time()
    
    url <- shiny:::saveShinySaveState(state)
    
    clientData <- session$clientData
    url <- paste0(
      clientData$url_protocol, "//", clientData$url_hostname, 
      if (nzchar(clientData$url_port)) 
        paste0(":", clientData$url_port), clientData$url_pathname, 
      "?", url)
    
    url
  }
  
  # save: open plot ---- 
  observeEvent(input$btn_open_plot, {
    req(input$sel_plots)
    
    #browser()
    #url_plot = values$saved_plots[[input$sel_plots]][['url']]
    
    load_plot(input$sel_plots)
    #browseURL(url_plot)
  })
  
  download_report = function(output_fmt){
    downloadHandler(
      filename = function() {
        paste0('mbon-sdg14-plots_', str_replace_all(format(Sys.time(), tz='GMT'), '[ ]', '.'), '-GMT.pdf')},
      content = function(file) {
        url = bkmark(session)
        render('report.Rmd', output_format=output_fmt, output_file=file, params = list(url=url))})
  }
  
  # save: btn_download_pdf ----
  output$btn_download_pdf = download_report('pdf_document')
  output$btn_download_doc = download_report('pdf_document')
  output$btn_download_htm = download_report('pdf_document')

# plots2rmd = function(values$saved_plots){
#   
# }
  
  # save: btn_download_url ----
  observeEvent(input$btn_download_url, {
    url = bkmark(session)
    browseURL(url)
  })
  
  # save: onRestored ----
  onRestored(function(state) {
    cat("Restoring from state bookmarked at", state$values$saved_time, "\n", file=stderr())
    
    values$saved_plots <- state$values$saved_plots
    
    load_plot(plot_titles()[1])
  })
  
  load_plot = function(plot_title){
    p = values$saved_plots[[which(plot_title == plot_titles())]]
    
    cat("Selecting EEZ", p$sel_eez, "\n", file=stderr())
    updateSelectizeInput(session, 'sel_eez', selected = p$sel_eez)
  }
  
  # env: seascape modal ----
  observeEvent(input$show_seascape_info, {
    showModal(modalDialog(
      title = 'Seascape Definitions',
      tagList(
        p('1. Highly oligotrophic subtropical gyres'),
        p('2. Oligotrophic subtropics'),
        p('3. Equatorial oligotrophic boundary'),
        p('4. Subtropical transition zone'),
        p('5. Equatorial mesotrophic'),
        p('6. Subpolar mesotrophic transition'),
        p('7. Subpolar HNLC'),
        p('8. Temperate shelf/transition'),
        p('9. Subpolar mesotrophic shelf'),
        p('10. Subtropical/Equatorial upwelling'),
        p('11. Temperate/subpolar productive pelagic'),
        p('12. Subtropical/tropical productive shelves'),
        p('13. Temperate upwelling/polar shelves'),
        p('14. Polar seasonal ice edge'))))
  })
  
})
