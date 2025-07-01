#### __________________________________________________  #########
#####       Author : Ross Searle                             #####
#####       Date :  Tue Dec  3 08:36:36 2024                 #####
#####       Purpose : Shiny app to display National Soil     #####
#####                 Moisture Monitoring project data       #####
#####       Comments :                                       #####
#### __________________________________________________  #########

####. #############
#### _____Insantiate the App___________ ####
####. #################################################################################

library(shiny)
library(bslib)
library(DT)
library(rhandsontable)
library(dygraphs)
library(leaflet)
library(sf)
library(reactable)
library(shinyjs)



# setwd <- 'C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/NSMM/app.R'

machineName <- as.character(Sys.info()['nodename'])
if (machineName=='soils-discover2') {
  source('/srv/plumber/SensorFederator2/Backends/ObjectStore.R')
}else{
  setwd <- 'C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/TernLandscapes/APIs/SensorFederator2'
  source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/TernLandscapes/APIs/SensorFederator2/Backends/ObjectStore.R')
}


source('Styles/headerLayout.R')
source('UI_Modules/Tab_SoilMoistureMain.R')
source('UI_Modules/Tab_SoilMorphology.R')
source('UI_Modules/Tab_SMIPS.R')
source('UI_Modules/Tab_About.R')
source('UI_Modules/Tab_Admin.R')
source('Server_Modules/Server_AdminTab.R')




######^^^ Get DB Data  ####
glbNetworks <<- BE$Shared$Queries$doSelectQuery("select * from Networks")
glbSites <<-  BE$Shared$Queries$doSelectQuery("select * from Sites")
SensorSummary <<- BE$Shared$Reporting$SensorSummary()





####.  #####
#### _______ UI CODE ____________  #########
####.  #####

ui <- fluidPage(

  PageHeader_UI(),
  
  useShinyjs(),

  navbarPage("", id = "mainTabset", 

            theme = shinythemes::shinytheme("flatly"),
             
            Tab1_UI(),
            Tab2_UI(),
            Tab4_UI(),
            Tab3_UI(),
            Tab_Admin_UI(),
  )
                      
)


####_______   SERVER CODE  ________   #########################################
server <- function(input, output, session) {
  
####   Instantiate Reactive Valuse  ######  
  RV <- reactiveValues()
  RV$User <- NULL
  RV$CurrentSite <- NULL
  RV$ShowReporting <- F
  RV$CurrentSensorSummaryTable <- NULL
  RV$Admin_SensorSummaryTable_SelectedRow <- NULL
  RV$CanPlotAdminSensors <- F
  
####  Read URL Parameters  #######  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['usr']])) {
      RV$User <- query[['usr']]
    }
  })

#####  Show/Hide UI Components   #######

  observe({
    req(RV$User)
    if(RV$User=='Admin'){
        RV$ShowReporting <- T
    }else{
      RV$ShowReporting <- F
    }
  })
  
  observe({
    if(RV$ShowReporting){
      
      showTab(inputId = "mainTabset", target = "Admin")
    }else{
     # hideTab(inputId = "mainTabset", target = "Admin")
    }
  })
  
  

  
  # output$wgt_Admin_SensorSummaryTable = renderRHandsontable({
  #   req( RV$CurrentSensorSummaryTable)
  #   df <- RV$CurrentSensorSummaryTable
  #   
  #   row_highlight <- which(df$IsActive=='FALSE')
  #        rhandsontable( df, selectCallback = TRUE,  manualColumnResize = T, readOnly = TRUE, rowHeaders = F,
  #                       
  #                       afterDeselect = htmlwidgets::JS(
  #                         "function() {Shiny.onInputChange('wgtDisplayTimeTable_select', null);
  #                  }"
  #                       )) %>%
  #          hot_cols(columnSorting = TRUE)
  # })
  # 
  # observe({
  #   req( input$wgt_Admin_SensorSummaryTable_select$select$r)
  #   sid <- input$wgt_Admin_SensorSummaryTable_select$select$r
  #  print(sid)
  #  rec <- RV$CurrentSensorSummaryTable[sid, ]
  #  print(rec)
  #  RV$Admin_SensorSummaryTable_SelectedRow <- rec
  # })
  
  
 
  
  
  # output$wgt_Admin_SensorDetails_SensorTimeseries <- renderPlot({
  #   
  #   rec <- RV$Admin_SensorSummaryTable_SelectedRow
  #   ts <- BE$Shared$Queries$GetSensorDataAsTS(rec$sensorNum)
  #   print(head(ts))
  #   df <- data.frame(a=c(1,2,3,4), b=c(5,6,7,8))
  #   plot(df)
  #   
  # })
  
  
  output$wgt_Admin_SensorDetails_SensorTimeseries <- renderDygraph({
    
    
       rec <- RV$Admin_SensorSummaryTable_SelectedRow
       ts <- BE$Shared$Queries$GetSensorDataAsTS(rec$sensorNum)
       
       dg <- dygraph(ts,  ylab =  'Values', group = 'Main') %>%
         dyAxis("y", label ='Volumetric Moisture (mm)', valueRange = c(0, 100))
       
       dg
    
    # yr <- year(dt)
    # isolate({
    #   colnames(RV$currentRainTS)<-'Rain'
    #   dygraph(RV$currentRainTS,  ylab = 'Rain (mm)', group = 'Main') %>% 
    #     dyBarChart() %>% 
    #     dyOptions(colors = 'blue', retainDateWindow = TRUE) %>% 
    #     dyEvent( dt, label = dt, labelLoc = c("top"), color = "black", strokePattern = "solid") %>%
    #     dyRangeSelector(dateWindow = c(paste0(yr, "-01-01"), paste0(2021, "-08-31")), height = 20)  
    # })
  })
  
####. #####
####^ Soil Probes Main Page   ####
####^^ Probe Map  ########################################################################## 
  
   
  #####   Draw the main map 
  
  output$wgt_SMmain_ProbeLocationeMap <- renderLeaflet({
    pts <- st_as_sf(glbSites, coords = c("Longitude", "Latitude"), crs = 4326)
    
    labs <- lapply(seq(nrow(pts)), function(i) {
      paste0( '<li>Network : ', pts[i, ]$Network, '</li>
                  <li>Site Name : ', pts[i, ]$SiteName, '</li>
                  <li>Access : ', pts[i, ]$Access, '</li>
                  <li>Install Date : <b>', pts[i, ]$InstallDate, '</b></li>
                
              ')})
    
    #sid <- paste0(pts$Network, '$', pts$SiteName)
    sid <- paste0(pts$SiteID)
    
    leaflet::leaflet() %>% addProviderTiles('Esri.WorldImagery') %>%
      setView(lng = 135, lat = -28, zoom = 3) %>%
      addProviderTiles("CartoDB.PositronOnlyLabels") %>%
      addCircleMarkers(data=pts, radius=6, color="black", stroke=FALSE, fillOpacity=1, group="locations", layerId = ~sid, label=lapply(labs, HTML))
  })
  
  ####  default look for the selected site
  acm_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius=6, color="black", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId="Selected")
  
  ####  Select a site
  observeEvent(input$wgt_SMmain_ProbeLocationeMap_marker_click, { # update the map markers and view on map clicks
    p <- input$wgt_SMmain_ProbeLocationeMap_marker_click
    proxy <- leafletProxy("wgt_SMmain_ProbeLocationeMap")
    if(p$id=="Selected"){
      proxy %>% removeMarker(layerId="Selected")
    } else {
      RV$CurrentSite <- p$id
      print(RV$CurrentSite)
      proxy %>% setView(lng=p$lng, lat=p$lat, input$wgt_SMmain_ProbeLocationeMap_zoom) %>% acm_defaults(p$lng, p$lat)
    }
  })
  
  
  ####^^ Render the Timeseries charts for the site  ##########################################################################   
  observe({
   
    
 })
   
   output$wgt_SMmain_rawTSChart <- renderDygraph({
     
     req(RV$CurrentSite)
     smTS <- BE$Shared$SensorData$getSensorDataStreams(usr = BE$Shared$Constants$AccessTypes$Public, pwd =  BE$Shared$Constants$AccessTypes$Public,
                                                       siteID = RV$CurrentSite, sensorType = BE$Shared$Constants$knownFeatures$SoilMoisture)
     maxVal <- max(smTS)
     
     #dt <- as.Date(input$datePker)
     dygraph(smTS,  group = 'Main') %>%
       dyLegend(show = "follow", width = 250, showZeroValues = TRUE, labelsDiv = NULL, labelsSeparateLines = FALSE, hideOnMouseOut = TRUE) %>%
       dyAxis("y", label ='Raw Probe Values', valueRange = c(0, maxVal))  #%>%
      # dyEvent( dt, label = dt, labelLoc = c("top"), color = "black", strokePattern = "solid")
   })
   
   output$wgt_SMmain_rainTSChart <- renderDygraph({
     
     req(RV$CurrentSite)
     rainTS <- BE$Shared$SensorData$getSensorDataStreams(usr = BE$Shared$Constants$AccessTypes$Public, pwd =  BE$Shared$Constants$AccessTypes$Public,
                                                         siteID = RV$CurrentSite, sensorType = BE$Shared$Constants$knownFeatures$Rainfall)
     maxVal <- max(rainTS)
     
     #dt <- as.Date(input$datePker)
     dygraph(rainTS,  group = 'Main') %>%
       dyLegend(show = "follow", width = 250, showZeroValues = TRUE, labelsDiv = NULL, labelsSeparateLines = FALSE, hideOnMouseOut = TRUE) %>%
       dyAxis("y", label ='Rainfall', valueRange = c(0, maxVal))  #%>%
     # dyEvent( dt, label = dt, labelLoc = c("top"), color = "black", strokePattern = "solid")
   })
   
   
###. ####   
### ^ -----  Admin Tab  ------ ####  
###. #### 
### ^^ Admin Sensor Map Sub Tab Table  #### 
### ^^^^ Render Sensors Summary Table  ####   
   
   output$wgtAdminNetworksTable <- renderReactable({
     Admin_SensorsTable(df=RV$CurrentSensorSummaryTable )
   })
     
###  ^^^^ Update SiteName combo box choices   ####   
   observe({
     req(input$wgt_Admin_SensorSummaryTable_FilterNetwork)
     sql <- paste0("Select SiteName from Sites where Network = '", input$wgt_Admin_SensorSummaryTable_FilterNetwork, "' order by SiteName ASC")
     chcs <- BE$Shared$Queries$doSelectQuery(sql)
     updateSelectInput(session = session, inputId = 'wgt_Admin_SensorSummaryTable_SiteName', choices = c('All', chcs$SiteName ))
   })
   

###  ^^^^ Get filtered Sensor Info from DB   ####      
   observeEvent(input$wgt_Admin_SensorSummaryTable_ShowButton, {
     # RV$CurrentSensorSummaryTable <- BE$Shared$Reporting$SensorSummaryFromLiveData()
     
     r <- list()
     if(input$wgt_Admin_SensorSummaryTable_FilterDatatype != 'All'){
       r$Sensors.DataType = input$wgt_Admin_SensorSummaryTable_FilterDatatype
     }
     if(input$wgt_Admin_SensorSummaryTable_FilterNetwork != 'All'){
       r$Networks.Network = input$wgt_Admin_SensorSummaryTable_FilterNetwork
     }
     if(input$wgt_Admin_SensorSummaryTable_SiteName != 'All'){
       r$Sites.SiteName= input$wgt_Admin_SensorSummaryTable_SiteName
     }
     
     filt <- NULL
     if(length(r) > 0){
       if(r[1] != 'All'){
          filt <- paste0( names(r)[1], " = '", r[[1]], "' " )
       }
     }
     if(length(r) > 1){
       for (z in 2:length(r)) {
         if(r[z] != 'All'){
         filt <- paste0(filt, ' and ', names(r)[z], " = '", r[[z]], "' " )
         }
       }
       
     }
     
     if(length(r) ==3){
       shinyjs::show('wgtButton_Admin_SensorSummaryTable_ShowTimeseries')
       RV$CanPlotAdminSensors <- T
     }else{
       shinyjs::hide('wgtButton_Admin_SensorSummaryTable_ShowTimeseries')
       RV$CanPlotAdminSensors <- F
     }
     
     print(filt)
     
     RV$CurrentSensorSummaryTable <- BE$Shared$Reporting$SensorSummary(filter = filt)
     print(nrow(RV$CurrentSensorSummaryTable))
     
   })
   
   
###  ^^^^ Render Sensor data for select site   ####   
   output$wgtAdmin_SensorChart <- renderDygraph({
     
     req( RV$CanPlotAdminSensors)
     
     if( RV$CanPlotAdminSensors){
       
       
       smTS <- BE$Shared$SensorData$getSensorDataFromDBUsingSensorSummaryRecs(RV$CurrentSensorSummaryTable)
       print(smTS)
       maxVal <- max(smTS)
       
       dygraph(smTS,  group = 'Main') %>%
         dyLegend(show = "follow", width = 250, showZeroValues = TRUE, labelsDiv = NULL, labelsSeparateLines = FALSE, hideOnMouseOut = TRUE) %>%
         dyAxis("y", label ='Raw Probe Values', valueRange = c(0, maxVal))  #%>%
       
     }
     
    
   })
   
###  ^^^^ Popup modal dialog for TS Charts  ####      
   observeEvent(input$wgtButton_Admin_SensorSummaryTable_ShowTimeseries, {
     
     req( RV$CanPlotAdminSensors)
     
     if( RV$CanPlotAdminSensors){
     
     showModal(
       
       modalDialog(
         tags$h2(paste0(RV$CurrentSensorSummaryTable$SiteName[1], ' : ', RV$CurrentSensorSummaryTable$DataType[1])),
         dygraphOutput('wgtAdmin_SensorChart', width = "1150px", height = "350px"),
         footer=tagList(
           modalButton('Close')
         )
       ))
     
     }
     
   })
   
   
 output$wgtNetworkSummaryListView <- renderJsonedit({
   
   s <- BE$Admin$generateDBSummary()
   
   jsonedit(s)
 })
 
 output$wgtAdminHarvestLogTable <- renderReactable({

   Admin_HarvestLogTable()
 })
 
 
###. ####   
### ^^ Admin Map Sub Tab  ####
##### ^^^^   Draw the Admin Site Map  ####
 
 output$wgt_Admin_ProbeLocationsMap <- renderLeaflet({
  
   leaflet::leaflet() %>% addProviderTiles('Esri.WorldImagery') %>%
     setView(lng = 135, lat = -28, zoom = 3) %>%
     addProviderTiles("CartoDB.PositronOnlyLabels")
 })
 
 observe({
   req(input$wgtAdminMapViewType)
   
   mt <- input$wgtAdminMapViewType
   if(mt=='Online'){
     tbl <- SensorSummary %>% group_by(Network, SiteName) %>% summarise(Latitude = first(Latitude), Longitude = first(Longitude), Depth=max(LowerDepth))
     
     pcol<-colorFactor(palette = "viridis", tbl$Depth)
   }else if(mt=='Network'){
     tbl <- SensorSummary %>% group_by(Network, SiteName) %>% summarise(Latitude = first(Latitude), Longitude = first(Longitude), Depth=max(LowerDepth))
     
     pcol<-colorFactor(palette = "magma", tbl$Network)
   }
   
   print(tbl)
   
   # networks <- BE$Shared$SensorData$getAllNetworkInfo()
   # spcol<-colorFactor(palette = "viridis", networks$Network)
   # 
   # sites <- BE$Shared$SensorData$getAllSiteInfo()
   pts <- st_as_sf(tbl, coords = c("Longitude", "Latitude"), crs = 4326)
   # 
   # labs <- lapply(seq(nrow(pts)), function(i) {
   #   paste0( '<li>Network : ', pts[i, ]$Network, '</li>
   #                <li>Site Name : ', pts[i, ]$SiteName, '</li>
   #                 <li>Access : ', pts[i, ]$Access, '</li>
   #                 <li>Install Date : <b>', pts[i, ]$InstallDate, '</b></li>
   #                 
   #            ')})
   labs <- 'test'
   
   #sid <- paste0(pts$Network, '$', pts$SiteName)
  # sid <- paste0(pts$SiteID)
   
   proxy <- leafletProxy("wgt_Admin_ProbeLocationsMap")
   proxy %>% clearMarkers()
   proxy %>%  clearControls()
  # proxy %>%  addCircleMarkers(data=pts, radius=6,  color = spcol(sites$Network), stroke=FALSE, fillOpacity=1, group="locations", layerId = ~sid, label=lapply(labs, HTML))
   proxy %>%  addCircleMarkers(data=pts, radius=6,  color = pcol, stroke=FALSE, fillOpacity=1, group="locations", layerId = ~SiteName, label=lapply(labs, HTML))
   
 })
 
 
     
   
  
}

shinyApp(ui = ui, server = server)
