shinyServer(function(input, output, session) {
  
  # leaflet map, initial ----
  output$map <- renderLeaflet({
    cat('renderLeaflet()\n', file=stderr())
    
    leaflet(
      options=c(
        leafletOptions(
          minZoom=2,
          worldCopyJump=T),
        attributionControl=F)) %>%
      addProviderTiles("Stamen.TonerLite", group='Gray Land') %>% 
      # initial env WMSTile
      addWMSTiles(
        baseUrl = 'http://mbon.marine.usf.edu:8080/geoserver/satellite/wms',
        group = 'env', layers = vars[[var]][['curr_lyr']],
        options = WMSTileOptions(
          version = '1.3.0', format  = 'image/png', transparent = T,
          time    = dates[1]))
    
  })
  
  # update sel_ym ----
  observe({
    req(input$sel_var)
    
    
    cat('updateSliderInput()\n', file=stderr())
    
    updateSliderInput(
      session, 'sel_ym', 'Date:', 
      min = dates[length(dates)], max = dates[1], 
      value = dates[1])
  })
  
  get_var = reactive({
    if (!is.null(input$sel_var))
      var = input$sel_var
    var
  })
  
  get_dates = reactive({
    if (!is.null(input$sel_var))
      var = input$sel_var
    vars[[var]][['curr_dates']]
  })
  
  get_ymd = reactive({
    if (!is.null(input$sel_ym)){
      ymd =  sprintf('%s-15', str_sub(as.character(input$sel_ym), 1,7))
    } else {
      if (!is.null(input$sel_var))
        var = input$sel_var
      ymd = vars[[var]][['curr_dates']][1]
    }
    ymd
  })
    
  # update env WMSTiles ----
  observe({
    cat('leafletProxy()\n', file=stderr())

    # ensure date match with time slice
    cat(file=stderr(), sprintf('  get_var()="%s", get_ymd()="%s"\n', get_var(), get_ymd()))
    
    # update env WMSTile
    leafletProxy('map') %>%
      clearGroup('env') %>% 
      addWMSTiles(
        baseUrl = 'http://mbon.marine.usf.edu:8080/geoserver/satellite/wms',
        group = 'env', layers = vars[[get_var()]][['curr_lyr']],
        options = WMSTileOptions(
          version = '1.3.0', format  = 'image/png', transparent = T,
          time    = get_ymd()))
    
  })
  
})
