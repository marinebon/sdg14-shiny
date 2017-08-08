
server <- function(input, output, session) {

  output$map <- renderLeaflet({
    
    r_pid = get_r_pid()
  
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
  
  get_conn_tbl = reactive({
    buf = str_split(input$sel_buf_cel, '/')[[1]][1] %>% as.integer()
    cel = str_split(input$sel_buf_cel, '/')[[1]][2] %>% as.integer()
    
    conn_csv = sprintf(
      '%s/%s_%d/%dday_%dbuf-%dkm/%s_%s_%dday_%dbuf-%dkm_results/connectivity.csv',
      dir_root, input$sel_sanctuary, 
      as.integer(input$sel_yr), as.integer(input$sel_pld), 
      buf, cel, 
      input$sel_date, input$sel_sanctuary, 
      as.integer(input$sel_pld), 
      buf, cel)
    
    if(!file.exists(conn_csv)) stop(sprintf('CSV not found: %s', conn_csv))
    read_csv(conn_csv) %>% 
      as_tibble()
  })
  
  get_r_pid = reactive({
    buf = str_split(input$sel_buf_cel, '/')[[1]][1] %>% as.integer()
    cel = str_split(input$sel_buf_cel, '/')[[1]][2] %>% as.integer()
    
    pid_tif = sprintf(
      '%s/habitats/%s_%dbuf-%dkm_patchid.tif',
      dir_root, input$sel_sanctuary, buf, cel)
    
    if(!file.exists(pid_tif)) stop(sprintf('TIF not found: %s', pid_tif))
    raster(pid_tif)
  })

  observeEvent(input$map_draw_new_feature,{
    
    conn_tbl = get_conn_tbl()
    r_pid    = get_r_pid()
    
    ply = input$map_draw_new_feature %>%
      as.json() %>% geojson_sp()
    # ply = read_sf('tmp_ply.geojson') %>% as('Spatial')

    r_ply = raster::extract(r_pid, ply, cellnumbers=T)[[1]]
    sel_patchids = r_ply[,'value'] %>% na.omit()
    req(length(sel_patchids) > 0)

    if (input$sel_dir == 'Import'){
      
      conn_tbl = conn_tbl %>%
        filter(ToPatchID %in% sel_patchids)
      
      if (!input$ck_self){
        conn_tbl = conn_tbl %>%
          filter(!FromPatchID %in% sel_patchids)
      }
      
      conn_tbl = conn_tbl %>%
        group_by(FromPatchID) %>%
        summarize(quantity = sum(Quantity)) %>%
        mutate(percent = quantity / sum(quantity)) %>%
        arrange(desc(percent)) %>%
        select(patchid=FromPatchID, percent)
      
    } else {
      
      conn_tbl = conn_tbl %>%
        filter(FromPatchID %in% sel_patchids)
      
      if (!input$ck_self){
        conn_tbl = conn_tbl %>%
          filter(!ToPatchID %in% sel_patchids)
      }
      
      conn_tbl = conn_tbl %>%
        group_by(ToPatchID) %>%
        summarize(quantity = sum(Quantity)) %>%
        mutate(percent = quantity / sum(quantity)) %>%
        arrange(desc(percent)) %>%
        select(patchid=ToPatchID, percent)
    }
    
    r_pct = subs(r_pid, conn_tbl, by='patchid', which='percent', subsWithNA=T)
    sel_ply = subs(r_pid, data_frame(patchid=sel_patchids, one=1)) %>% 
      #raster::rasterToPolygons(dissolve = T) %>%  # crashes RStudio Desktop
      raster::rasterToPolygons() %>%
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
      hideGroup('Selected') %>%   # hide selected grid
      addLegend(
        pal = pal, values = values(r_pct), title = "% Larvae",
        labFormat = labelFormat(
          suffix = '%',  transform = function(x) 100 * x))
    
  })
}

