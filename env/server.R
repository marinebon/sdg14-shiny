shinyServer(function(input, output, session) {
  
  #get_nc = reactive({
     # nc_path = file.path(env_dir, env_files[[1]])
     # names(nc$var)
  #})
  
  output$ui_var <- renderUI({
    nc_path = file.path(env_dir, input$sel_nc)
    nc = nc_open(nc_path)
    #vars = setdiff(names(nc$var), c('longitude','latitude'))
    vars = names(nc$var)
    vars = vars[str_detect(vars, '.*_clim$')]
    selectInput('sel_var', 'Variable', vars)
  })
  
  output$map <- renderLeaflet({
    
    req(input$sel_nc)
    req(input$sel_var)
    # TODO: 
    # - select varname month
    # - cache r_l
    # 
    nc_path = file.path(env_dir, input$sel_nc)
    nc = nc_open(nc_path)
    lon = ncvar_get(nc, "longitude")[,1]
    lat = ncvar_get(nc, "latitude")[1,]

    #r = raster(nc, varname="January_chlor_a_clim") # , ext=extent(min(lon), max(lon), min(lat), max(lat)))
    r = raster(nc_path, varname=input$sel_var) # , ext=extent(min(lon), max(lon), min(lat), max(lat)))

    r <- flip(r, direction="y")
    r <- setExtent(r, extent(min(lon), max(lon), min(lat), max(lat)))
    crs(r) <- "+proj=longlat +datum=WGS84 +no_defs"
    #plot(r)
    #r
    r_l = leaflet::projectRasterForLeaflet(log(r))
    
    # pal <- colorNumeric(
    #   "Greens",  # RColorBrewer::display.brewer.all()
    #   values(r), na.color = "transparent")
    pal <- colorNumeric('Greens', values(r_l), na.color='transparent')

    eez_labels <- sprintf(
      "<strong>%s</strong><br/>%g km<sup>2</sup>",
      eez_sf$Territory1, eez_sf$Area_km2
    ) %>% lapply(HTML)
    
    leaflet(
      options=leafletOptions(
        minZoom=2,
        worldCopyJump=T)) %>%
      addProviderTiles("Esri.OceanBasemap", group='Color Ocean') %>%
      addProviderTiles("Stamen.TonerLite", group='Gray Land') %>%
      addRasterImage(r_l, colors = pal, opacity = 0.8, project=F, group='Chl') %>%
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
        pal = pal, values = values(r_l),
        position = 'bottomright',
        labFormat = labelFormat(transform = function(x) round(exp(x),2)),
        title = HTML(sprintf('Chl (mg/m^3) for<br>%s', input$sel_var))) %>%
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
        overlayGroups = c('Chl','EEZ'), options = layersControlOptions(collapsed=T)) 
    
  })
  
  eez4plot = reactiveVal(NULL)
  
  output$ts_plot = renderDygraph({
    # TODO: create *_eez-mean-sd.csv & cache

    req(eez4plot())
    req(input$sel_var)
    
    eez = eez4plot()
    #browser()
    mo = plyr::mapvalues(
      input$sel_var,
      sprintf('%s_chlor_a_clim', month.name),
      1:12) %>% as.integer()
    
    #eez = 'Albania'
    nc_path <- "/mbon/data_big/satellite/chlor_a/clim_27km/A20032007_chlor_a_CLIM_MO_GLOB_27km.nc"
    d = read_csv(sprintf('%s_eez-mean-sd.csv', tools::file_path_sans_ext(nc_path)))
    d = eez_sf %>%
      st_set_geometry(NULL) %>%
      left_join(d, by='MRGID')
    
    #View(d) # names(d)
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

  observeEvent(input$map_shape_mouseover, {
    eez4plot(input$map_shape_mouseover[['id']])
  })

  # observeEvent(input$map_shape_mouseout, {
  #   eez4plot(NULL)
  # })
    
  observeEvent(input$sel_eez, {
    
    eez4plot(input$sel_eez)
    
    if (length(input$sel_eez) == 1 && input$sel_eez == ''){
      b = st_bbox(eez_sf)
    } else {
      b = st_bbox(
        eez_sf %>%
          filter(sov_ter %in% input$sel_eez)) # %>%
      # st_transform(crs_mol))
    }
    
    leafletProxy('map', session) %>%
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
})
