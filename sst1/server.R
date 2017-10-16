shinyServer(function(input, output) {
  
  output$map <- renderLeaflet({

    # base map
    leaflet(
      options=c(
        leafletOptions(
          minZoom=2,
          worldCopyJump=T), attributionControl=F)) %>%
      addProviderTiles("Esri.OceanBasemap", group='Color Ocean') %>%
      addProviderTiles("Stamen.TonerLite", group='Gray Land')
    
  })
  
  observe({
    req(input$sel_ym)
    
    # ensure date match with time slice
    ymd =  sprintf('%s-15', str_sub(as.character(input$sel_ym), 1,7))
    cat(file=stderr(), sprintf('ymd: %s\n', ymd))
    
    # update WMSTile
    leafletProxy('map') %>%
      clearGroup('env') %>% 
      addWMSTiles(
        baseUrl = 'http://mbon.marine.usf.edu:8080/geoserver/satellite/wms',
        group = 'env', layers = 'satellite:sst_monthly_mean_27km',
        options = WMSTileOptions(
          version = '1.3.0', format  = 'image/png', transparent = T,
          time    = ymd))
    
  })
  
})
