shinyServer(function(input, output, session) {
  
  # leaflet map_env, initial ----
  output$map_env <- renderLeaflet({
    msg('renderLeaflet()')
    
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
        group = 'env', layers = env_vars[[var]][['curr_lyr']],
        options = WMSTileOptions(
          version = '1.3.0', format  = 'image/png', transparent = T,
          time    = dates[1]))
    
  })
  
  # update sel_env_ym ----
  observe({
    msg('updateSliderInput()')
    
    dates = get_dates()
    updateSliderInput(
      session, 'sel_env_ym', 'Date:', 
      min = dates[length(dates)], max = dates[1], 
      value = min(dates))
  })
  
  # env reactives ----
  get_var = reactive({
    if (!is.null(input$sel_env_var))
      var = input$sel_env_var
    var
  })
  
  get_dates = reactive({
    if (!is.null(input$sel_env_var))
      var = input$sel_env_var
    env_vars[[var]][['curr_dates']]
  })
  
  get_ymd = reactive({
    if (!is.null(input$sel_env_ym)){
      ymd =  sprintf('%s-15', str_sub(as.character(input$sel_env_ym), 1,7))
    } else {
      if (!is.null(input$sel_env_var))
        var = input$sel_env_var
      ymd = min(env_vars[[var]][['curr_dates']])
    }
    ymd
  })
  
  get_env_eez = reactive({
    req(input$sel_eez)
    
    eez_mrgids = eez_sf %>%
      st_set_geometry(NULL) %>%
      filter(sov_ter == input$sel_eez) %>%
      .$MRGID
    
    env_vars[[get_var()]][['curr_eez']] %>%
      filter(eez_mrgid %in% eez_mrgids)
  })
    
  # update env WMSTiles - leafletProxy() ----
  observe({
    msg('update env WMSTiles - leafletProxy()')

    # ensure date match with time slice
    #msg(sprintf('  input$sel_menu=%s, get_var()="%s", get_ymd()="%s"', input$sel_menu, get_var(), get_ymd()))
    v = env_vars[[get_var()]]
    
    # update env WMSTile
    leafletProxy('map_env') %>%
      clearGroup('env') %>% 
      clearControls() %>% # remove legend
      addWMSTiles(
        baseUrl = 'http://mbon.marine.usf.edu:8080/geoserver/satellite/wms',
        group = 'env', layers = env_vars[[get_var()]][['curr_lyr']],
        options = WMSTileOptions(
          version = '1.3.0', format  = 'image/png', transparent = T,
          time    = get_ymd())) %>%
      addLegend(
        pal = v$pal, values = v$values,
        position = 'bottomright',
        labFormat = labelFormat(transform = v$transform),
        title = v$legend)
    
    
  })
  
  
  # zoom to eez - leafletProxy() ----
  observe({
    msg('zoom to eez - leafletProxy()')
  
    if (length(input$sel_eez) == 1 && input$sel_eez == ''){
      b = st_bbox(eez_sf)
    } else {
      b = st_bbox(
        eez_sf %>%
          filter(sov_ter %in% input$sel_eez)) # %>%
    }
    
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
    
  })
  
  observeEvent(input$map_env_shape_click, {
    updateSelectizeInput(session, 'sel_eez', 'EEZ - Territory', c('', eez_sf$sov_ter), input$map_env_shape_click[['id']])
  })
  
  # env_ts_streamgraph ----
  output$env_ts_streamgraph = renderStreamgraph({
    req(input$sel_eez)
    req(get_var()=='seascape')
    
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
  
  # env_ts_dygraph ----
  output$env_ts_dygraph = renderDygraph({
    req(input$sel_eez)
    req(get_var()!='seascape')
    
    x = get_env_eez() %>%
      mutate(
        ymd = ymd(str_replace(raster, '^r_', '')),
        lwr_sd = mean - sd,
        upr_sd = mean + sd) %>%
      select(ymd, mean, lwr_sd, upr_sd) %>%
      arrange(ymd)
    
    x = xts(select(x, -ymd), order.by=x$ymd)
    
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
    
    dygraph(x, main=sprintf('%s for %s', env_vars[[get_var()]]$legend, input$sel_eez)) %>%
      dySeries(c('lwr_sd', 'mean', 'upr_sd'), label = env_vars[[get_var()]]$legend) %>%
      #dyOptions(colors = brewer.pal(3, c('chl'='Greens', 'sst'='Reds')[[get_var()]]))
      dyOptions(colors = c('chl'='green', 'sst'='red')[[get_var()]])
    
    # %>%
      #dyAxis("x",valueFormatter=JS(getMonthDay), axisLabelFormatter=JS(getMonth)) # %>%
      # TODO: sync with env date slider
      #dyShading(from=sprintf('2005-%02d-01', mo), to=sprintf('2005-%02d-01', mo+1), color='rgb(200,200,200)')
  })
  
  
  
  # showModal seascape info ----
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
