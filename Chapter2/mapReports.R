MapReports <- function(hexSpecSize){
  
  classes <- 7
  style_method <- "fisher"
  pal1 <- brewer.pal(classes, "YlOrRd")
  
  palDataNor <- classIntervals(hexSpecSize$totRatio, n = classes, style=style_method, pal = pal1)
  hexSpecSize$colorNor <- findColours(palDataNor, pal1)%>%
    as.factor(.)
  palNor <- colorBin(pal1, domain = palDataNor$brks, bins = palDataNor$brks, pretty = FALSE)
  
  HexNorMap <- leaflet(hexSpecSize) %>%
    setView(-74.9, 40.4, 6) %>%
    addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
    addPolygons(fillColor = ~colorNor,
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "2",
                fillOpacity = 0.7,
                popup = ~htmlEscape(sprintf("Reports per hexagon: %f",
                                            totRatio))) %>%
    addLegend(pal = palNor,
              values = ~totRatio, 
              opacity = 0.7, 
              title = "Reports Density",
              position = "bottomright")
  
  palDataTot <- classIntervals(hexSpecSize$total, n = classes, style=style_method, pal = pal1)
  hexSpecSize$colorTot <- findColours(palDataTot, pal1)%>%
    as.factor(.)
  palTot <- colorBin(pal1, domain = palDataTot$brks, bins = palDataTot$brks, pretty = FALSE)
  
  HexTotMap <- leaflet(hexSpecSize) %>%
    setView(-74.9, 40.4, 6) %>%
    addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
    addPolygons(fillColor = ~colorTot,
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "2",
                fillOpacity = 0.7,
                popup = ~htmlEscape(sprintf("Reports per hexagon: %i",
                                            total))) %>%
    addLegend(pal = palTot,
              values = ~total, 
              opacity = 0.7, 
              title = "# of Reports",
              position = "bottomright")
  
  sync(HexTotMap, HexNorMap)
  
}