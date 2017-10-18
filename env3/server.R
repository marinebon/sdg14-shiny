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
      # initial env WMSTile
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
      value = dates[1])
  })
  
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
      ymd = env_vars[[var]][['curr_dates']][1]
    }
    ymd
  })
    
  # update env WMSTiles ----
  observe({
    msg('leafletProxy()')

    # ensure date match with time slice
    #msg(sprintf('  input$sel_menu=%s, get_var()="%s", get_ymd()="%s"', input$sel_menu, get_var(), get_ymd()))
    #browser()
    v = env_vars[[get_var()]]
    
    # update env WMSTile
    # if (get_var()=='seascape')
    #   browser()
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
