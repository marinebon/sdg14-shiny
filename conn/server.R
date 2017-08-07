
server <- function(input, output, session) {

  output$map <- renderLeaflet({
    
    pal = colorNumeric('Spectral', values(r_pid), reverse=T, na.color='transparent')
    
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite) %>%
      addRasterImage(
        r_pid, group='PatchID', 
        colors=pal, opacity=0.3) %>%
      addDrawToolbar(
        targetGroup='Draw', #opacity=0.5, fillOpacity=0.3, 
        polylineOptions=F, markerOptions=F, singleFeature=T,
        editOptions = editToolbarOptions(
          selectedPathOptions = selectedPathOptions()))  %>%
      addLayersControl(
        overlayGroups = c('Draw','PatchID','Selected','Larvae'), 
        options = layersControlOptions(collapsed=T)) %>%
      addLegend(
        pal = pal, 
        values = values(r_pid), title='Patch ID')

  })
  

  observeEvent(input$map_draw_stop,{
    
    ply = input$map_draw_new_feature %>%
      as.json() %>% geojson_sp()
    # ply = read_sf('tmp_ply.geojson') %>% as('Spatial')
    #browser()
    
    r_ply = raster::extract(r_pid, ply, cellnumbers=T)[[1]]
    i_r = r_ply[,'cell']
    sel_patchids = r_ply[,'value']
    
    r_hi = r_pid
    r_hi[setdiff(1:ncell(r_pid), i_r)] = NA

    #conn_lns = readOGR(file.path(dir_results, 'output.gdb'), 'Connectivity', verbose=F)
    conn = read_csv(file.path(dir_results, 'connectivity.csv')) %>%  as_tibble()
    
    #/Volumes/Best HD/mbon_data_big/connectivity/mbnms_2009/10day_300buf-27km/01_25_2009_mbnms_10day_300buf-27km_results/connectivity.csv
    #/Volumes/Best HD/mbon_data_big/connectivity/fknms_2009/10day_300buf-27km/01_25_2009_fknms_10day_300buf-27km_results/connectivity.csv
    
    # Import first...
    if (input$sel_dir == 'Import'){
      
      #conn_tbl = conn_lns@data %>%
      conn_tbl = conn %>%
        filter(ToPatchID %in% sel_patchids)
      
      #browser()
      
      if (!input$ck_self){
        conn_tbl = conn_tbl %>%
          filter(!FromPatchID %in% sel_patchids)
      }
      
      conn_tbl = conn_tbl %>%
        group_by(FromPatchID) %>%
        summarize(quantity = sum(Quantity)) %>%
        #ungroup() %>%
        mutate(percent = quantity / sum(quantity)) %>%
        arrange(desc(percent)) %>%
        select(patchid=FromPatchID, percent)
      
    } else {
      
      conn_tbl = conn %>%
        filter(FromPatchID %in% sel_patchids)
      
      if (!input$ck_self){
        conn_tbl = conn_tbl %>%
          filter(!ToPatchID %in% sel_patchids)
      }
      
      conn_tbl = conn_tbl %>%
        group_by(ToPatchID) %>%
        summarize(quantity = sum(Quantity)) %>%
        #ungroup() %>%
        mutate(percent = quantity / sum(quantity)) %>%
        arrange(desc(percent)) %>%
        select(patchid=ToPatchID, percent)
    }
    
    r_pct = subs(r_pid, conn_tbl, by='patchid', which='percent', subsWithNA=T)
    #plot(r_pct)
    # sel_patchids=c(39, 40, 41, 42, 43, 52, 53, 54, 55, 65, 66, 67, 68, 69)
    sel_ply = subs(r_pid, data_frame(patchid=sel_patchids, one=1)) %>% 
      raster::rasterToPolygons(dissolve=T) %>%
      st_as_sf() %>%
      st_transform(4326)
    
    pal = colorNumeric('Spectral', values(r_pct), reverse=T, na.color='transparent')
    
    leafletProxy('map') %>% 
      clearControls() %>%         # clear legend
      hideGroup('PatchID') %>%    # hide patchid
      hideGroup('Draw') %>%       # hide drawn polygon
      clearGroup('Larvae') %>%    # clear previous selected pixels
      clearGroup('Selected') %>%  # clear previous selected polygon
      addRasterImage(
        r_pct, group='Larvae', colors=pal, opacity=0.8) %>%
      addPolygons(
        data=sel_ply, group='Selected') %>%
      addLegend(
        pal = pal, values = values(r_pct), title = "% Larvae",
        labFormat = labelFormat(
          suffix = '%',  transform = function(x) 100 * x))
    
  })
}

