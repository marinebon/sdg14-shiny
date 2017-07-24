shinyServer(function(input, output, session) {
  
  output$map <- renderLeaflet({
    
    pal = colorNumeric('Spectral', eez$Area_km2)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g km<sup>2</sup>",
      eez$Territory1, eez$Area_km2
    ) %>% lapply(HTML)
    
    leaflet(
      eez,
      options = leafletOptions(
        crs=leafletCRS(
          crsClass='L.Proj.CRS', code='ESRI:53009',
          proj4def= crs_mol, # ))) %>%
          resolutions = c(65536, 32768, 16384, 8192, 4096, 2048)))) %>%
      addGraticule(style= list(color= '#999', weight= 0.5, opacity= 1)) %>%
      addGraticule(sphere = TRUE, style= list(color= '#777', weight= 1, opacity= 0.25)) %>%
      addPolygons(
        data=countries, group = 'land', weight = 1, color = '#4D4D4D') %>% # gplots::col2hex('gray30'): '#4D4D4D'
      addPolygons(
        group = 'eez',
        fillColor = ~pal(Area_km2),
        weight = 2,
        opacity = 1,
        color = "white",
        #dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          #dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(
        title=HTML('Area (km<sup>2</sup>)'),
        pal = pal, values = ~Area_km2, opacity = 0.7,
        position = "bottomright") #%>%
    #
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
