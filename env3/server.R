shinyServer(function(input, output, session) {
  
  # leaflet map, initial ----
  output$map <- renderLeaflet({
    #cat('renderLeaflet()\n', file=stdout())
    
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
          time    = vars[[var]][['curr_dates']][1]))
    
  })
  
  # get_dates() ----
  get_dates  = reactive({
    req(input$sel_var)
    #cat('get_dates()\n', file=stdout())
    
    vars[[input$sel_var]][['curr_dates']]
  })
  
  # update sel_ym ----
  observe({
    
    req(input$sel_var)
    
    dates = get_dates()
    #cat('updateSliderInput()\n', file=stdout())
    
    updateSliderInput(
      session, 'sel_ym', 'Date:', 
      min = dates[length(dates)], max = dates[1], 
      value = dates[1])
  })
  
  # update env WMSTiles ----
  observe({
    req(input$sel_var)
    req(input$sel_ym)
    #cat('leafletProxy()\n', file=stdout())

    # ensure date match with time slice
    ymd =  sprintf('%s-15', str_sub(as.character(input$sel_ym), 1,7))
    cat(file=stderr(), sprintf('ymd: %s\n', ymd))
    
    # update env WMSTile
    leafletProxy('map') %>%
      clearGroup('env') %>% 
      addWMSTiles(
        baseUrl = 'http://mbon.marine.usf.edu:8080/geoserver/satellite/wms',
        group = 'env', layers = vars[[input$sel_var]][['curr_lyr']],
        options = WMSTileOptions(
          version = '1.3.0', format  = 'image/png', transparent = T,
          time    = ymd))
    
  })
  
})
