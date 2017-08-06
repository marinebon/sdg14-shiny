# TODO: seascapes, per https://marinebon.github.io/seascape-viz/prep.html & /mbon/data_big/satellite/seascapes/*

shinyServer(function(input, output, session) {
  
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
      r_transform = function(x) round(exp(x),2)
    }
    if (r_type=='seascape'){
      pal = colorNumeric('Spectral', values(r), na.color='transparent', reverse=T)
      r_group  = 'Seascape'
      r_legend = 'Seascape Class'
      r_transform = function(x) x
    } 

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
        overlayGroups = c(r_group,'EEZ'), options = layersControlOptions(collapsed=T)) 
    
  })
  
  eez4plot = reactiveVal(NULL)
  
  output$ts_streamgraph = renderStreamgraph({
    req(input$sel_grd)
    req(input$sel_lyr)
    req(eez4plot())

    grd = input$sel_grd
    lyr = input$sel_lyr
    eez = eez4plot()
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
    req(eez4plot())
    
    eez = eez4plot()
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
