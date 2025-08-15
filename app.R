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
library(htmlwidgets)
library(bslib)
library(DT)
library(rhandsontable)
library(dygraphs)
library(leaflet)
library(sf)
library(reactable)
library(shinyjs)
library(dplyr)
#library(flexdashboard)
library(plotrix)
library(c3)
library(cookies)



# setwd <- 'C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/NSMM/app.R'

machineName <- as.character(Sys.info()['nodename'])
if (machineName=='soils-discover2') {
  source('/srv/plumber/SensorFederator2/Backends/ObjectStore.R')
}else{
  setwd <- 'C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/TernLandscapes/APIs/SensorFederator2'
  source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/TernLandscapes/APIs/SensorFederator2/Backends/ObjectStore.R')
}


source('Secrets/configs.R')
source('Styles/headerLayout.R')
source('UI_Modules/Tab_SoilMoistureMain.R')
source('UI_Modules/Tab_SiteInformation.R')
source('UI_Modules/Tab_SMIPS.R')
source('UI_Modules/Tab_About.R')
source('UI_Modules/Tab_Admin.R')
source('Server_Modules/Server_AdminTab.R')
source('Functions/Plotting.R')



######^^^ Get DB Data  ####
glbNetworks <<- BE$Shared$Queries$doSelectQuery("select * from Networks")
glbSites <<-  BE$Shared$Queries$doSelectQuery("select * from Sites")
glbHarvestDates <<- BE$Shared$Queries$doSelectQuery('select distinct DT from HarvestLog order by DT DESC') 
SensorSummary <<- BE$Shared$Reporting$SensorSummary()
glbCurrentSurfaceSoilMoisture <<- BE$Shared$Reporting$LatestSurfaceSoilMoisture()





####.  #####
#### _______ UI CODE ____________  #########
####.  #####

ui <- fluidPage(

  PageHeader_UI(),
  
  useShinyjs(),
  cookie_dependency(),
  
  navbarPage("", id = "mainTabset", 

            theme = shinythemes::shinytheme("flatly"),
             
            Tab1_UI(),
            Tab2_UI(),
         #   Tab4_UI(),
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
  RV$CurrentAdminSiteSensorInfo = NULL
  RV$CurrentSitePhotos = NULL
  RV$CurrentSiteSMTimeSeries = NULL
  RV$Cookie <- NULL
  # RV$Cookie$Network = NULL
  # RV$Cookie$Site = NULL

 
  

  
####  Read URL Parameters  #######  
#####  Show/Hide UI Components   #######
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['adminkey']])) {
      akey <- query[['adminkey']]
      if(akey == AdminKey){
        RV$ShowReporting <- T
      }else{
        RV$ShowReporting <- F
      } 
    }
    
    ck <- get_cookie(cookie_name = "NSMMDefaultSite", session = session)
    isolate({
 
   if(!is.null(ck)){
     bits <- stringr::str_split(ck, '[|]')
     RV$Cookie$Network <- bits[[1]][1]
     RV$Cookie$Site <- bits[[1]][2]
   }else{
     RV$Cookie$Network <- 'NSMM'
     RV$Cookie$Site <- 'Thoona'
   }
  })
  })
  
  observe({
    req(input$wgt_SMmain_Network)
    
  
     df <- glbSites[glbSites$Network==input$wgt_SMmain_Network, ]
     site <- ''
    isolate({
        if(!is.null(RV$Cookie$Network) && input$wgt_SMmain_Network==RV$Cookie$Network){
          site <-  RV$Cookie$Site
        }else{
         
        }
    })
    updateSelectInput(session, inputId = 'wgt_SMmain_SiteName', choices = sort(df$SiteName), selected = site)
  }) 

  
  observe({
    req(input$wgt_SMmain_SiteName)
    input$wgt_SaveCookie
   isolate( set_cookie(cookie_name = "NSMMDefaultSite", cookie_value = paste0(input$wgt_SMmain_Network,'|',input$wgt_SMmain_SiteName), session = session))
    
  })

  observe({
    if(RV$ShowReporting){
      
      showTab(inputId = "mainTabset", target = "Admin")
    }else{
      hideTab(inputId = "mainTabset", target = "Admin")
    }
  })
  
  
  output$wgt_Admin_SensorDetails_SensorTimeseries <- renderDygraph({
    
       rec <- RV$Admin_SensorSummaryTable_SelectedRow
       ts <- BE$Shared$Queries$GetSensorDataAsTS(rec$sensorNum)
       
       dg <- dygraph(ts,  ylab =  'Values', group = 'Main') %>%
         dyAxis("y", label ='Volumetric Moisture (mm)', valueRange = c(0, 100))
       dg
  })
  
####. #####
####^ Soil Probes Main Page   ####
####^^ Probe Map  ########################################################################## 
 
  
   
  #####   Draw the main map 
  
  output$wgt_SMmain_ProbeLocationeMap <- renderLeaflet({
    
    
    tbl <- glbCurrentSurfaceSoilMoisture
    pcol <-colorNumeric(palette = 'Blues', tbl$SoilMoisture)
    
    pts <- st_as_sf(tbl, coords = c("Longitude", "Latitude"), crs = 4326)
    
    labs <- lapply(seq(nrow(pts)), function(i) {
      paste0( '<li>Network : ', pts[i, ]$Network, '</li>
                  <li>Site Name : ', pts[i, ]$SiteName, '</li>
                  <li>Access : ', pts[i, ]$Access, '</li>
                  <li>Install Date : <b>', pts[i, ]$InstallDate, '</b></li>
                
              ')})
    
    sid <- paste0(pts$Network, '!', pts$SiteName)
    
    leaflet::leaflet() %>% addProviderTiles('Esri.WorldImagery') %>%
      setView(lng = 135, lat = -28, zoom = 3) %>%
      addProviderTiles("CartoDB.PositronOnlyLabels") %>%
      onRender(
        "function(el, x) {
          L.control.zoom({position:'topright'}).addTo(this);
        }") %>%
      addCircleMarkers(data=pts, radius=6, color = pcol(tbl$SoilMoisture), stroke=FALSE, fillOpacity=1, group="locations", layerId = ~sid, label=lapply(labs, HTML)) %>%
      leaflet::addLegend(title='SM', pal = pcol, values = tbl$SoilMoisture,  position = "topleft")
    
  })
  
  ####  default look for the selected site
  acm_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius=6, color="black", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId="Selected")
  
  ####  Select a site
  observeEvent(input$wgt_SMmain_ProbeLocationeMap_marker_click, { # update the map markers and view on map clicks
    p <- input$wgt_SMmain_ProbeLocationeMap_marker_click
    bits <- str_split( p$id, '!')
    network <- bits[[1]][1]
    site <- bits[[1]][2]
    updateSelectInput(session, inputId = 'wgt_SMmain_Network', selected=network)
    updateSelectInput(session, inputId = 'wgt_SMmain_SiteName', selected=site)
  })
  
  
  
  observe({
      req(input$wgt_SMmain_Network, input$wgt_SMmain_SiteName)
      
      proxy <- leafletProxy("wgt_SMmain_ProbeLocationeMap")
      proxy %>% removeMarker(layerId="Selected")
      rec <- glbSites[glbSites$Network==input$wgt_SMmain_Network & glbSites$SiteName==input$wgt_SMmain_SiteName,]
  
       lat <- rec$Latitude   
       lng <- rec$Longitude   
        
      # proxy %>% setView(lng=lng, lat=lat, input$wgt_SMmain_ProbeLocationeMap_zoom) %>% acm_defaults(lng, lat)
       proxy %>% acm_defaults(lng, lat)
       
      sl <- list()
      sl$Network <- input$wgt_SMmain_Network
      sl$SiteName <- input$wgt_SMmain_SiteName
      
      RV$CurrentSite <- sl
      
  })
  

  
  ####^^ Get Site Data  #####  
  
  observe({
    
    as.volumetric=T

    req(input$wgt_SMmain_Network, input$wgt_SMmain_SiteName)
  
    
    isolate({
    idxs <- which(SensorSummary$Network==RV$CurrentSite$Network & SensorSummary$SiteName==RV$CurrentSite$SiteName
                  & SensorSummary$DataType == BE$Shared$Constants$knownFeatures$SoilMoisture  )
    
    if(length(idxs)>0){
      mts <- BE$Shared$SensorData$getSensorDataFromDBUsingSensorSummaryRecs(SensorSummary[idxs,])
      RV$CurrentSiteSMTimeSeries$rawSM <- mts
      
      if(as.volumetric){
        mtsV <- BE$Calibrations$getAllLayersVolumeticTS(Network = RV$CurrentSite$Network, SiteName=RV$CurrentSite$SiteName, rawTS=mts, type='Available')
        RV$CurrentSiteSMTimeSeries$volSM <- mtsV
      }
    }
      
    }) 
  })
  
  
  
  observe({
    
    req(RV$CurrentSiteSMTimeSeries$rawSM)
    
    updateSliderInput(session, inputId = 'datePker', min = start(RV$CurrentSiteSMTimeSeries$rawSM),
                      max = end(RV$CurrentSiteSMTimeSeries$rawSM) , 
                      value = end(RV$CurrentSiteSMTimeSeries$rawSM)
                      
    )
  })

  
  
  
  ####^^ Render the Timeseries charts for the site  ##########################################################################   
  
  
  output$wgt_SMmain_volTSChart <- renderDygraph({
    
    req(RV$CurrentSiteSMTimeSeries$rawSM)
        
    
    mtsV <- RV$CurrentSiteSMTimeSeries$volSM
    
    calibs <- BE$Shared$Queries$doSelectQuery(paste0("select * from probeCalibration where Network='", RV$CurrentSite$Network, "' and SiteName='", RV$CurrentSite$SiteName, "'"))
    calibs <- makeDepthIntervals(calibs)
    
    bucketSize =  as.integer(sum( (calibs$DrainedUpperLimit - calibs$LowerLimit)/100 * (calibs$IntervalLower-calibs$IntervalUpper)))
    maxVal <- bucketSize
    
    #maxVal <- max(rowSums(mtsV), na.rm = T)
        bits <- str_split(names(mtsV), '_')
        updepth <- as.numeric(sapply(bits, function (x) x[2]))/10
        lowdepth <- as.numeric(sapply(bits, function (x) x[3]))/10
        names(mtsV) <- paste0(updepth, "_", lowdepth)

        dygraph(mtsV,  ylab =  'Vol', group = 'Main') %>%
          dyAxis("y", label ='Volumetric Moisture (mm),', 'Volumetric Moisture', valueRange = c(0, maxVal)) %>%

          dyOptions(axisLineWidth = 1.5, fillGraph = F, drawGrid = T, titleHeight = 26) %>%
          dyOptions(colors = RColorBrewer::brewer.pal(11, "Paired"), stackedGraph=T, retainDateWindow = TRUE) %>%
          dyLegend(show = "follow", width = 250, showZeroValues = TRUE, labelsDiv = NULL, labelsSeparateLines = FALSE, hideOnMouseOut = TRUE) %>%
        dyEvent( as.Date(input$datePker), label = as.Date(input$datePker), labelLoc = c("top"), color = "black", strokePattern = "solid")
      #  dyRangeSelector(dateWindow = c(Sys.Date()-20, Sys.Date()), height = 20)
        
  })


   output$wgt_SMmain_rawTSChart <- renderDygraph({

    # req(RV$CurrentSite, input$wgt_SMmain_Network, input$wgt_SMmain_SiteName)
    #  idxs <- which(SensorSummary$Network==RV$CurrentSite$Network & SensorSummary$SiteName==RV$CurrentSite$SiteName
    #                & SensorSummary$DataType == BE$Shared$Constants$knownFeatures$SoilMoisture )

     req(RV$CurrentSiteSMTimeSeries$rawSM)


         smTS <-RV$CurrentSiteSMTimeSeries$rawSM

         maxVal <- max(smTS, na.rm = T)

         bits <- str_split(names(smTS), '_')
         updepth <- as.numeric(sapply(bits, function (x) x[2]))/10
         lowdepth <- as.numeric(sapply(bits, function (x) x[3]))/10
         names(smTS) <- paste0(updepth, "_", lowdepth)

         dygraph(smTS,  group = 'Main', main = 'Soil Moisture') %>%
           dyLegend(show = "follow", width = 250, showZeroValues = TRUE, labelsDiv = NULL, labelsSeparateLines = FALSE, hideOnMouseOut = TRUE)  %>%
           dyAxis("y", label ='Raw Values', valueRange = c(0, maxVal)) %>%
           dyEvent( as.Date(input$datePker), label = as.Date(input$datePker), labelLoc = c("top"), color = "black", strokePattern = "solid")
 
          # dyLegend(width = 400) 
     # dyRangeSelector(dateWindow = c(Sys.Date()-20, Sys.Date()), height = 20)

     #}
     })

   output$wgt_SMmain_rainTSChart <- renderDygraph({
     
     req(RV$CurrentSite, input$wgt_SMmain_Network, input$wgt_SMmain_SiteName)
     idxs <- which(SensorSummary$Network==input$wgt_SMmain_Network & SensorSummary$SiteName==RV$CurrentSite$SiteName 
                   & SensorSummary$DataType == BE$Shared$Constants$knownFeatures$Rainfall )
     
     if(length(idxs)>0){
       indf <- SensorSummary[idxs,]
       rainTS <- BE$Shared$SensorData$getSensorDataFromDBUsingSensorSummaryRecs(SensorSummary[idxs,])     
       maxVal <- max(rainTS, na.rm = T)
       
       names(rainTS) <- 'Rainfall (mm)'
       dygraph(rainTS,  group = 'Main', main = 'Rainfall' ) %>%
         dyBarChart() %>% 
         dyLegend(show = "follow", width = 250, showZeroValues = TRUE, labelsDiv = NULL, labelsSeparateLines = FALSE, hideOnMouseOut = TRUE) %>%
         dyAxis("y", label ='Rainfall (mm)', valueRange = c(0, maxVal)) %>%
         dyEvent( as.Date(input$datePker), label = as.Date(input$datePker), labelLoc = c("top"), color = "black", strokePattern = "solid") %>% 
         dyRangeSelector(dateWindow = c(Sys.Date()-20, Sys.Date()), height = 20)
     }
   })
   
   
   ### ^^ Render bucket plot  ####
    output$wgt_SMmain_volProfile <- renderPlot({
  
     req( RV$CurrentSiteSMTimeSeries$volSM, input$datePker)
      calibs <- BE$Shared$Queries$doSelectQuery(paste0("select * from probeCalibration where Network='", RV$CurrentSite$Network, "' and SiteName='", RV$CurrentSite$SiteName, "'"))
      dt <- as.Date(input$datePker)
      day <- RV$CurrentSiteSMTimeSeries$volSM[dt]

      if(nrow(day)>0){
        plotPAWProfile(ts = day, calibs)
      }
      
    })
   
   
   
   ### ^^ Render bucket colour gradient plot  ####
   output$wgt_SMmain_volProfile_colGradient <- renderPlot({
     
     req( RV$CurrentSiteSMTimeSeries$volSM)
     req(RV$CurrentSite, input$wgt_SMmain_Network, input$wgt_SMmain_SiteName)
     calibs <- BE$Shared$Queries$doSelectQuery(paste0("select * from probeCalibration where Network='", RV$CurrentSite$Network, "' and SiteName='", RV$CurrentSite$SiteName, "'"))
     
     dt <- as.Date(input$datePker)
     day <- RV$CurrentSiteSMTimeSeries$volSM[dt]
     
     plotPAWProfileGradientColours(ts = day, calibs)
     
   })
   
   
   ### ^^ Render Gauge  ####
   output$wgt_SMmain_guage <- renderC3({
     
     req( req( RV$CurrentSiteSMTimeSeries$volSM))
     calibs <- BE$Shared$Queries$doSelectQuery(paste0("select * from probeCalibration where Network='", RV$CurrentSite$Network, "' and SiteName='", RV$CurrentSite$SiteName, "'"))
     calibs <- makeDepthIntervals(calibs)
     
    bucketSize =  as.integer(sum( (calibs$DrainedUpperLimit - calibs$LowerLimit)/100 * (calibs$IntervalLower-calibs$IntervalUpper)))
     
    dt <- as.Date(input$datePker)
    smvals <- RV$CurrentSiteSMTimeSeries$volSM[dt]
   # smvals <- tail(RV$CurrentSiteSMTimeSeries$volSM, 1)
    sdf <- data.frame(upperD=calibs$IntervalUpper, lowerD=calibs$IntervalLower, vals=as.numeric(as.character(smvals))/100)
   
   totwat <- sum((sdf$lowerD-sdf$upperD) * sdf$vals)
   percWat <- (totwat/bucketSize) * 100
    
     data.frame('Available Moisture'=percWat) %>%
       c3() %>% c3_gauge(title = 'Colors', label = 'Available Moisture', pattern = c('brown', '#f5e1c1', '#84d3f5', "#042d5e"))
     
   })
   
   
   output$vtext <- renderText({
     
     req( req( RV$CurrentSiteSMTimeSeries$volSM))
     calibs <- BE$Shared$Queries$doSelectQuery(paste0("select * from probeCalibration where Network='", RV$CurrentSite$Network, "' and SiteName='", RV$CurrentSite$SiteName, "'"))
     calibs <- makeDepthIntervals(calibs)
     
     bucketSize =  as.integer(sum( (calibs$DrainedUpperLimit - calibs$LowerLimit)/100 * (calibs$IntervalLower-calibs$IntervalUpper)))
     
     dt <- as.Date(input$datePker)
     smvals <- RV$CurrentSiteSMTimeSeries$volSM[dt]
     # smvals <- tail(RV$CurrentSiteSMTimeSeries$volSM, 1)
     sdf <- data.frame(upperD=calibs$IntervalUpper, lowerD=calibs$IntervalLower, vals=as.numeric(as.character(smvals))/100)
     
     totwat <- sum((sdf$lowerD-sdf$upperD) * sdf$vals)
     percWat <- (totwat/bucketSize) * 100
     HTML(  paste0("<p style='color:green; font-weight: bold;'><br><br>Total Bucket Size = ", round(bucketSize), " mm",
                   "<br>Available Soil Water = ", round(totwat), " mm",
                   "<br>Total Rain = ", '0', " mm",
                   "<br>&nbsp;In Season Rain = ", '0', " mm",
                   "</p>"
     )
     )
     
   })
   
  
   
   
   ###. ####   
   ### ^ -----  Site Description Tab  ------ ####  
   ###. #### 
   ### ^^ Photos #### 
   ### ^^^^ Render all the photos   ####   
   
   output$WgtSiteInfoTab_SoilSiteImage <- renderUI({
   })
   
   observe({  
     
     req(input$wgt_SMmain_Network, input$wgt_SMmain_SiteName)
     df <- BE$Shared$Queries$doSelectQuery(paste0("select  photoNum, Network, SiteName, imageName, imgFormat, theDate, width, height, Description, Type from images where Network='", input$wgt_SMmain_Network,"' and SiteName = '", input$wgt_SMmain_SiteName,"'"))
     updateSelectInput(session, inputId = 'wgt_SiteInformation_Network_PhotoType', choices=unique(df$Type))
     RV$CurrentSitePhotos <- df
   })
   
   observe({

     req(input$wgt_SMmain_Network, input$wgt_SMmain_SiteName, input$wgt_SiteInformation_Network_PhotoType, RV$CurrentSitePhotos)
     descs <- RV$CurrentSitePhotos[RV$CurrentSitePhotos$Type == input$wgt_SiteInformation_Network_PhotoType, ]
     namelst <- descs$photoNum
     names(namelst) <- descs$Description
    # updateSelectInput(session, inputId = 'wgt_SiteInformation_PhotoDescription', choices =paste0(descs$photoNum, ' : ', descs$Description))
     updateSelectInput(session, inputId = 'wgt_SiteInformation_PhotoDescription', choices = namelst)
   })
   
   output$WgtSiteInfoTab_SoilSiteImage <- renderUI({
     
     req( input$wgt_SiteInformation_PhotoDescription, RV$CurrentSitePhotos) 
     
   #  photoNum <- str_split( input$wgt_SiteInformation_PhotoDescription, ' : ')[[1]][1]
     photoNum <- input$wgt_SiteInformation_PhotoDescription
     df <- BE$Shared$Queries$doSelectQuery(paste0("select * from images where photoNum = ", photoNum))
     
     if(nrow(df)>0){
     html <- ''
 #    for (i in 1:nrow(df)) {
       rec <- df[1,]
       
       if(rec$URL != ''){
         html <- paste0(html, '<img src="', rec$URL, '" 
          height="', 600, '"<BR><BR>')
       }else{
         m <- max(rec$width, rec$height)
         prop <- 600/m
         w = ceiling(rec$width * prop )
         h = ceiling(rec$height * prop )
         
         html <- paste0(html, '<img src="data:image/png;base64,', rec$b64ImageData, '" 
          width="', w, '" 
          height="', h, '"<BR><BR>')
       }
       
  #   }
     HTML(paste0(html) )
     }else{
       HTML('<h2>No Images available for this Site</h2>' )
     }
     
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
     
     RV$CurrentSensorSummaryTable <- BE$Shared$Reporting$SensorSummary(filter = filt)
     
   })
   
   
###  ^^^^ Render Sensor data for select site   ####   
   output$wgtAdmin_SensorChart <- renderDygraph({
     
     req( RV$CanPlotAdminSensors)
     
     if( RV$CanPlotAdminSensors){
       smTS <- BE$Shared$SensorData$getSensorDataFromDBUsingSensorSummaryRecs(RV$CurrentSensorSummaryTable)
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
 
 
 
###. ####   
### ^^ Admin Map Sub Tab  ####
#####^^^^   Draw the Admin Site Map  ####
 
 output$wgt_Admin_ProbeLocationsMap <- renderLeaflet({
  
   updateSelectInput(session, inputId = 'wgtAdminMapViewType', selected = 'Network')
   leaflet::leaflet() %>% addProviderTiles('Esri.WorldImagery') %>%
     setView(lng = 135, lat = -28, zoom = 4) %>%
     addProviderTiles("CartoDB.PositronOnlyLabels") %>%
     onRender(
       "function(el, x) {
          L.control.zoom({position:'topright'}).addTo(this);
        }")
 })
 
 observe({
   
   req(input$wgtAdminMapViewType)
   
   mt <- input$wgtAdminMapViewType
   if(mt=='Depth'){
         param <- 'Depth'
         legTitle = 'Max Sensor Depth'
         tbl <- SensorSummary %>% group_by(Network, SiteName) %>% summarise(Latitude = first(Latitude), Longitude = first(Longitude), Depth=max(LowerDepth))
         pcol<-colorNumeric(palette = "viridis", tbl$Depth)
     
     }else if(mt=='Network'){
         param <- 'Network'
         legTitle = 'Network'
         tbl <- SensorSummary %>% group_by(Network, SiteName) %>% summarise(Latitude = first(Latitude), Longitude = first(Longitude))
         pcol<-colorFactor(palette = "viridis", tbl$Network)
     
     }else if(mt=='Length of Record'){
         param <- 'Length'
         legTitle = 'Duration'
         tbl <- SensorSummary %>% group_by(Network, SiteName) %>% summarise(Latitude = first(Latitude), Longitude = first(Longitude), Length=max(Count))
         pcol <-colorNumeric(palette = "plasma", tbl$Length)
   
     }else if(mt=='Latest Surface Moisture'){
          param = 'SoilMoisture'
          legTitle = 'Surface Soil Moisture'
          tbl <- glbCurrentSurfaceSoilMoisture
          pcol <-colorNumeric(palette = 'Blues', tbl$SoilMoisture)
   
    }else if(mt=='All Active'){
         param <- 'allActive'
         legTitle = 'All Active'
         sites <- unique(SensorSummary[c('Network', 'SiteName')])
         odf <- data.frame()
           for (i in 1:nrow(sites)) {
             rec <- sites[i,]
             s <- rec$SiteName   
             n <- rec$Network 
             sensors <- SensorSummary[SensorSummary$Network==n & SensorSummary$SiteName==s,]
             allActive =  !any(sensors$IsActive==F)
             rdf <- data.frame(Network=n, SiteName=s, Latitude=sensors$Latitude[1], Longitude=sensors$Longitude[1], allActive=allActive )
             rdf$allActive <- ifelse(rdf$allActive, "Yes", "No")
             odf <- rbind(odf, rdf)
           }
         tbl<-odf
         pcol <- colorFactor(palette = c('green', 'red'), levels = c('Yes', 'No'))
       
     }else if(mt=='Quality Index'){
         param = 'QualityIndex'
         legTitle = 'Quality Index'
         tbl <- SensorSummary %>% group_by(Network, SiteName) %>% summarise(Latitude = first(Latitude), Longitude = first(Longitude),  QualityIndex=mean(QualityIndex))
         pcol <-colorNumeric(palette = 'RdYlGn', tbl$QualityIndex)
   }
   
   pts <- st_as_sf(tbl, coords = c("Longitude", "Latitude"), crs = 4326)
   

   labs <- lapply(seq(nrow(pts)), function(i) {
     paste0( '<li>Network : ', pts[i, ]$Network, '</li>
                  <li>Site Name : ', pts[i, ]$SiteName, '</li>
                   <li>', param, ' : ', pts[i, ][[param]], '</li>')})

   
   proxy <- leafletProxy("wgt_Admin_ProbeLocationsMap")
   proxy %>% clearMarkers()
   proxy %>%  clearControls()
   proxy %>%  addCircleMarkers(data=pts, radius=6, color = pcol(tbl[[param]]), stroke=FALSE, fillOpacity=1, group="locations", layerId = ~paste0(Network, '!', SiteName), label=lapply(labs, HTML)) 
   proxy %>%  leaflet::addLegend(title=legTitle, pal = pcol, values = tbl[[param]],  position = "bottomright")
   
   
 })
 
 observeEvent(input$wgt_Admin_ProbeLocationsMap_marker_click, { 
   p <- input$wgt_Admin_ProbeLocationsMap_marker_click  # typo was on this line
   bits <- str_split(p$id, '!')
  sdf <- BE$Shared$Reporting$SensorSummary(filter =  paste0(" Networks.Network = '", bits[[1]][1], "' and Sites.SiteName = '", bits[[1]][2], "'"))
   RV$CurrentAdminSiteSensorInfo <- sdf
 })
 
 
 ###  ^^^^ Popup modal dialog for Admin Map Sensor Display  ####      
 observe({
   
   req( RV$CurrentAdminSiteSensorInfo)
   
   df <- RV$CurrentAdminSiteSensorInfo
     showModal(
       
       modalDialog(
         tags$h2(paste0(df$Network[1], ' - ', df$SiteName[1])),
         selectInput(inputId =  'wgtAdmin_CurrentSiteSensorsDataTypeSelect',label = 'DataType', choices = NULL),
         dygraphOutput('wgtAdmin_CurrentSiteSensorsChart', width = "1150px", height = "250px"),
         HTML('<BR><BR>'),
         reactableOutput('wgtAdmin_CurrentSiteSensorsTable', height = "350px"),
         
         footer=tagList(
           modalButton('Close')
         )
       ))
 })
 
 output$wgtAdmin_CurrentSiteSensorsTable  <- renderReactable({
   req( RV$CurrentAdminSiteSensorInfo)
   updateSelectInput(session,inputId = 'wgtAdmin_CurrentSiteSensorsDataTypeSelect', choices = RV$CurrentAdminSiteSensorInfo$DataType )
   df <- RV$CurrentAdminSiteSensorInfo
   Admin_SensorsTable( df)
 })
   
 ###  ^^^^ Render Sensor data for select site   ####   
 output$wgtAdmin_CurrentSiteSensorsChart <- renderDygraph({
   
   req( input$wgtAdmin_CurrentSiteSensorsDataTypeSelect)
   
  idxs <- which(RV$CurrentAdminSiteSensorInfo$DataType == input$wgtAdmin_CurrentSiteSensorsDataTypeSelect)
   
     smTS <- BE$Shared$SensorData$getSensorDataFromDBUsingSensorSummaryRecs(RV$CurrentAdminSiteSensorInfo[idxs,])
     maxVal <- max(smTS)
     
     dygraph(smTS,  group = 'Main') %>%
       dyLegend(show = "follow", width = 250, showZeroValues = TRUE, labelsDiv = NULL, labelsSeparateLines = FALSE, hideOnMouseOut = TRUE) %>%
       dyAxis("y", label ='Raw Probe Values', valueRange = c(0, maxVal))  #%>%
   
 })
   
   
   
  
 
 
 
###.#### 
#####^^   Admin Harvest Sub Tab ####
 
 output$wgtAdminHarvestLogTable <- renderReactable({
   req(input$wgtAdminHarvestLogDates)
   Admin_HarvestLogTable(input$wgtAdminHarvestLogDates)
 })
 
 
 output$wgtHarvestSummaryHTML  <- renderUI({
   
  HTML( '<p1><B>HarvestSummary</B></p1>')
   
 })
   
  
}

shinyApp(ui = ui, server = server)
