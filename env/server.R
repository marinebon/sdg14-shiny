shinyServer(function(input, output, session) {
  
  # output$test <- renderUI({
  #   list_path = function(path){
  #     sprintf('<br>\n<b>%s</b>\n<br><ul>\n<li>%s</li>\n</ul>\n', path, paste(list.files(path, recursive=T), collapse='</li>\n<li>'))
  #   }
  #   s = list_path('/mbon/data_big/satellite/chlor_a/anom_27km')
  #   #s = paste(s, list_path('/mbon/data_big/satellite/chlor_a/clim_27km'))
  #   s = paste(s, list_path('/mbon/data_big/satellite/chlor_a/clim_27km_b'))
  #   #s = paste(s, list_path('/mbon/data_big/satellite/clim_27km_b'))
  #   # s = paste(s, list_path('/mbon/data_big/satellite/chlor_a'))
  #   # s = paste(s, list_path('/mbon/data_big/satellite'))
  #   # s = paste(s, list_path('/mbon/data_big/'))
  #   # s = paste(s, list_path('/mbon'))
  #   
  #   #system('touch /mbon/shiny/test_shiny.txt')
  #   #system('touch /mbon/shiny/test_users/test_shiny.txt')
  #   system('touch /mbon/shiny-log/test_shiny.txt')
  # 
  #   HTML(s) 
  # })
  
  output$map <- renderLeaflet({
    
    if (!is.null(input$sel_env)){
      nc_path = file.path(env_dir, input$sel_env)
      #browser()
      #nc_path = file.path(env_dir, env_files[1])
      nc  = nc_open(nc_path) # names(nc$var)
      lon = ncvar_get(nc, "longitude")[,1]
      lat = ncvar_get(nc, "latitude")[1,]

      #r = raster(nc, varname="January_chlor_a_clim") # , ext=extent(min(lon), max(lon), min(lat), max(lat)))
      r = raster(nc_path, varname="January_chlor_a_clim") # , ext=extent(min(lon), max(lon), min(lat), max(lat)))

      r <- flip(r, direction="y")
      r <- setExtent(r, extent(min(lon), max(lon), min(lat), max(lat)))
      crs(r) <- "+proj=longlat +datum=WGS84 +no_defs"
      #plot(r)
      #r
      r_l = leaflet::projectRasterForLeaflet(r)

      pal <- colorNumeric(
        "Greens",  # RColorBrewer::display.brewer.all()
        values(r), na.color = "transparent")

      leaflet(
        options=leafletOptions(
          minZoom=2,
          worldCopyJump=T)) %>%
        addProviderTiles("Esri.OceanBasemap", group='Color Ocean') %>%
        addProviderTiles("Stamen.TonerLite", group='Gray Land') %>%
        addRasterImage(r_l, colors = pal, opacity = 0.8, project=F, group='Chl') %>%
        addLegend(pal = pal, values = values(r),
                  title = "Jan Chl") %>%
        addMeasure() %>%
        addGraticule()  %>%
        addMiniMap(toggleDisplay=T, position='bottomleft')  %>%
        addEasyButton(easyButton(
          icon="fa-globe", title="Zoom to Level 1",
          onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
        addDrawToolbar(
          targetGroup='Draw',
          editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>%
        addLayersControl(
          baseGroups    = c('Gray Land','Color Ocean'),
          overlayGroups = c('Chl','Draw'), options = layersControlOptions(collapsed=T)) %>%
        addStyleEditor()
    } else {
      leaflet(
        options=leafletOptions(
          minZoom=2,
          worldCopyJump=T)) %>%
        addProviderTiles("Esri.OceanBasemap", group='Color Ocean') %>%
        addProviderTiles("Stamen.TonerLite", group='Gray Land') %>%
        addMeasure() %>%
        addGraticule()  %>%
        addMiniMap(toggleDisplay=T, position='bottomleft')  %>%
        addEasyButton(easyButton(
          icon="fa-globe", title="Zoom to Level 1",
          onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
        addDrawToolbar(
          targetGroup='Draw',
          editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>%
        addLayersControl(
          baseGroups    = c('Gray Land','Color Ocean'),
          overlayGroups = c('Chl','Draw'), options = layersControlOptions(collapsed=T)) %>%
        addStyleEditor()
    }
  })
  
  
  # zoom to region
  observeEvent(input$sel_eez, {
    
    if (length(input$sel_eez) == 1 && input$sel_eez == ''){
      b = st_bbox(eez)
    } else {
      b = st_bbox(
        eez %>%
          filter(sov_ter %in% input$sel_eez)) # %>%
      # st_transform(crs_mol))
    }
    
    leafletProxy('map', session) %>%
      fitBounds(b[['xmin']], b[['ymin']], b[['xmax']], b[['ymax']]) %>%
      clearGroup('eez_hi') %>%
      addPolygons(
        data = eez %>%
          filter(sov_ter %in% input$sel_eez),
        group = 'eez_hi',
        fill = F,
        weight = 4,
        color = 'yellow')
    
  })
})
