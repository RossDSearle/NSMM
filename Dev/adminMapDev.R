

  



  if(mt=='Online'){
    param <- 'Depth'
    legTitle = 'Max Sensor Depth'
    tbl <- SensorSummary %>% group_by(Network, SiteName) %>% summarise(Latitude = first(Latitude), Longitude = first(Longitude), Depth=max(LowerDepth))
    pcol<-colorNumeric(palette = "viridis", tbl$Depth)
  }
  else if(mt=='Network'){
    param <- 'Network'
    legTitle = 'Network'
    tbl <- SensorSummary %>% group_by(Network, SiteName) %>% summarise(Latitude = first(Latitude), Longitude = first(Longitude))
    pcol<-colorFactor(palette = "magma", tbl$Network)
  }
  else if(mt=='Length'){
    param <- 'Length'
    legTitle = 'Duration'
    tbl <- SensorSummary %>% group_by(Network, SiteName) %>% summarise(Latitude = first(Latitude), Longitude = first(Longitude), Length=max(Count))
    pcol<-colorNumeric(palette = "magma", tbl$Length)
  }
  
  pts <- st_as_sf(tbl, coords = c("Longitude", "Latitude"), crs = 4326)

  labs <- 'test'
  
  leaflet::leaflet() %>% addProviderTiles('Esri.WorldImagery') %>%
    setView(lng = 135, lat = -28, zoom = 3) %>%
    addProviderTiles("CartoDB.PositronOnlyLabels")  %>%
    addCircleMarkers(data=pts, radius=6,  color = pcol(tbl[[param]]), stroke=FALSE, fillOpacity=1, group="locations", layerId = ~SiteName, label=lapply(labs, HTML)) %>% 
    leaflet::addLegend(title=legTitle, pal = pcol, values = tbl[[param]],  position = "topright")

