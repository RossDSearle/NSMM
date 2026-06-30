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

dev=T

library(shiny)
library(shinybusy)
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
library(plotrix)
library(c3)
library(cookies)
library(stringr)
library(httr)
library(jsonlite)
library(listviewer)
library(lubridate)
machineName <- as.character(Sys.info()['nodename'])
if (machineName=='landscapes-soils') {
  APIUrl <<- 'http:'

  source('/srv/plumber/TERNLandscapes/SoilMoistureProbesAPI/Backends/ObjectStore.R')
}else{
 
 # source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/TernLandscapes/APIs/SoilMoistureProbesAPI/Backends/ObjectStore.R')
  APIUrl <<- 'http://127.0.0.1:8071/SensorAPI/'  ### need to run script to make this available - C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/TernLandscapes/APIs/SoilMoistureProbesAPI/WebAPI/RunAPIAsync.R
  setwd('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/NSMM')
  }


source('Functions/constants.R')
source('Functions/Helpers.R')
source('Secrets/configs.R')
source('Styles/headerLayout.R')
source('UI_Modules/Tab_SoilMoistureMain.R')
source('UI_Modules/Tab_SiteInformation.R')
source('UI_Modules/Tab_SMIPS.R')
source('UI_Modules/Tab_About.R')
source('UI_Modules/Tab_Admin.R')
source('Server_Modules/Server_AdminTab.R')
source('Functions/Plotting.R')

### Get data from the API

# if(dev){
#     load("c:/projects/NSMM/api.RData")
# }else{
#    
# }

gd <- getData(what='globals')
glbNetworks <<- gd$glbNetworks
glbSites <<- gd$glbSites
glbHarvestDates <<- gd$glbHarvestDates
glbSensorSummary <<-  gd$SensorSummary
glbCurrentSurfaceSoilMoisture <<- gd$glbCurrentSurfaceSoilMoisture
glbSensorDataTypes <<- gd$glbSensorDataTypes
glbCalibrationInfo <<- gd$CalibrationInfo 


rainChartMaxYVal = 30


####.  #####
#### _______ UI CODE ____________  #########
####.  #####

ui <- fluidPage(
  
  
  tags$head(
    tags$style(HTML("
      #shiny-notification-panel {
        position: fixed;
        top: 30%;
        left: 50%;
        transform: translate(-50%, -50%);
        bottom: auto;
        right: auto;
        width: 600px; /* Adjust width as needed */
        max-width: 90%;
        z-index: 9999; /* Ensures it stays on top of other elements */
      }
    ")),
    
    
    #### side bar
    tags$style(HTML("
      #slidePanel {
        position: fixed;
        width: 300px;
        height: 100%;
        top: 0;
        left: -300px;
        background-color: #f8f9fa;
        padding: 20px;
        box-shadow: 2px 0 5px rgba(0,0,0,0.3);
        transition: left 0.4s ease-in-out;
        z-index: 1050;
      }
      #slidePanel.open {
        left: 0;
      }
    ")),
    
    
    tags$style(HTML("
        div[id^='DyTSlegendDiv']  {
  margin-bottom: 2px !important; /* Set a very tight bottom margin */
  padding: 0 !important;
   display: inline-block; 
          font-size: 10px !important;
          font-weight: bold;
          line-height: 1.2;
  
  }
         
      "))
    
    ),
  
  add_busy_spinner(spin = "flower", position = "full-page", color = 'darkgreen', timeout = 1000, height = '80px', ),
  tags$style(HTML("
  .shinybusy-overlay {
    background-color: transparent !important;
    box-shadow: none !important;
  }
")),
  
  # Custom CSS to hide the colored bar and match the background track
  tags$head(
    tags$style(HTML("
      .irs-bar, .irs-bar-edge {
        background: #e1e4e9 !important;
        border-top: 1px solid #d4d7dc !important;
        border-bottom: 1px solid #d4d7dc !important;
      }
    "))
  ),
  
  

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
  RV$Email <- NULL
  RV$CurrentSite <- NULL
  RV$CurrentDate <- Sys.Date()
  RV$ShowReporting <- F
  RV$CurrentSensorSummaryTable <- NULL
  RV$Admin_SensorSummaryTable_SelectedRow <- NULL
  RV$CanPlotAdminSensors <- F
  RV$CurrentAdminSiteSensorInfo = NULL
  RV$CurrentSitePhotos = NULL
  RV$CurrentSiteData = NULL
  RV$Cookie <- NULL
  
  
  

 
#### ^ Set up a temp directory per user  ####
  user_temp_dir <- file.path(tempdir(), paste0('NSMMDataCache/', session$token)) # if dev=T this is set to null in the call to getAllSiteData - may leave it this way
  if (!dir.exists(user_temp_dir)) {
    dir.create(user_temp_dir, recursive = TRUE)
  }
  session$onSessionEnded(function() {
    if (dir.exists(user_temp_dir)) {
    #  if(!dev){
        unlink(user_temp_dir, recursive = TRUE)
    #  }
      
    }
  })  

  
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
    
    readCookie()

  })    
    
###  ^ Cookie management ####
    
  ###  ^ ^ Write the Settings cookies  ####
 readCookie <- reactive({
    ck_network <- get_cookie(cookie_name = "NSMMDefaultNetwork", session = session)
    ck_site <- get_cookie(cookie_name = "NSMMDefaultSite", session = session)
    ck_email <- get_cookie(cookie_name = "NSMMDefaultEmail", session = session)
    ck_tidy <- get_cookie(cookie_name = "NSMMDefaultTidyTS", session = session)
    
    isolate({
         if(!is.null(ck_network)){
           RV$Cookie$Network <- ck_network
          }else{
           RV$Cookie$Network <- 'NSMM'
          }
      
          if(!is.null(ck_site)){
            RV$Cookie$Site <- ck_site
          }else{
            RV$Cookie$Site <- 'Thoona'
          }
      
          if(!is.null(ck_email)){
            RV$Cookie$Email <- ck_email
          }else{
            RV$Cookie$Email <- ''
          }
        
          if(!is.null(ck_tidy)){
            RV$Cookie$TidyTS <- ck_tidy
          }else{
            RV$Cookie$TidyTS <- 'true'
          }
    })
})
  
  # ###  ^ ^ Save the Site Name cookies  ####
  # observeEvent(input$wgt_SaveCookie, {
  #   req(input$wgt_SMmain_SiteName,input$wgt_SMmain_Network)
  #   isolate( set_cookie(cookie_name = "NSMMDefaultSite", cookie_value = paste0(input$wgt_SMmain_Network,'|',input$wgt_SMmain_SiteName), session = session))
  #   showNotification(
  #     paste("Site saved :",  paste0(input$wgt_SMmain_Network,' - ',input$wgt_SMmain_SiteName)), 
  #     type = "message", 
  #     duration = 3,
  #     closeButton = T
  #   )
  # })
  # 
  

  
  
  ###   ^ App Settings  #####
  # 1. Open the panel when 'Settings' is clicked
  observeEvent(input$toggleBtn, {
    
    print('Readings cookies....')
    readCookie()
    
    shinyjs::addClass("slidePanel", "open")
    updateSelectInput(session = session, inputId = 'wgt_settings_default_Network', selected=RV$Cookie$Network)
    new_choices <- as.character(glbSites[glbSites$network==RV$Cookie$Network, ]$sitename)
    isolate( updateSelectInput(session = session, inputId = 'wgt_settings_default_site', choices=new_choices, selected=as.character(RV$Cookie$Site)))
    updateTextInput(session = session, inputId = 'wgt_settings_user_email', value=RV$Cookie$Email)
    updateCheckboxInput(session = session, inputId = 'wgt_settings_tidyTS', value=as.logical(RV$Cookie$TidyTS))

  })
  
  observeEvent(input$wgt_settings_default_Network, {
    new_choices <- as.character(glbSites[glbSites$network==input$wgt_settings_default_Network, ]$sitename)
    isolate( updateSelectInput(session = session, inputId = 'wgt_settings_default_site', choices=new_choices, selected=as.character(RV$Cookie$Site)))
  })
  # 2. Close the panel when 'Save' is clicked
  
  
  observeEvent(input$wgt_settings_cancel, {
    shinyjs::removeClass("slidePanel", "open")
  })
  ###  ^ ^ Save the Site Name cookies  ####
  
  observeEvent(input$wgt_settings_save, {
    shinyjs::removeClass("slidePanel", "open")
    print('Writing Cookie')
    isolate({
      
      RV$Cookie$Network <- input$wgt_settings_default_Network
      RV$Cookie$Site <- input$wgt_settings_default_site
      RV$Cookie$Email <- input$wgt_settings_user_email
      RV$Cookie$TidyTS <- input$wgt_settings_tidyTS
    
      set_cookie(cookie_name = "NSMMDefaultNetwork", cookie_value = paste0(RV$Cookie$Network), session = session)
      set_cookie(cookie_name = "NSMMDefaultSite", cookie_value = paste0(RV$Cookie$Site), session = session)
      set_cookie(cookie_name = "NSMMDefaultEmail", cookie_value = paste0(RV$Cookie$Email), session = session)
      set_cookie(cookie_name = "NSMMDefaultTidyTS", cookie_value = paste0(RV$Cookie$TidyTS), session = session)
      
      print(get_cookie(cookie_name = "NSMMDefaultSite"))
    })
  })
  
 
  
  # observeEvent(input$wgt_settings_default_Network, {
  #    new_choices <- glbSites[glbSites$network==input$wgt_settings_default_Network, ]$sitename
  #    print(new_choices)
  #    updateSelectInput(session, 'wgt_settings_default_site',
  #                      choices = as.character(new_choices),
  #                      selected = new_choices[1])
  # })
  
  
  observe({
    req(input$wgt_SMmain_Network)
    
     df <- glbSites[glbSites$network==input$wgt_SMmain_Network, ]
     
     site <- ''
    isolate({
        if(!is.null(RV$Cookie$Network) && input$wgt_SMmain_Network==RV$Cookie$Network){
          site <-  RV$Cookie$Site
        }else{
         
        }
    })
    updateSelectInput(session, inputId = 'wgt_SMmain_SiteName', choices = sort(df$sitename), selected = site)
  }) 


  observe({
    if(RV$ShowReporting){
      
      showTab(inputId = "mainTabset", target = "Admin")
    }else{
      hideTab(inputId = "mainTabset", target = "Admin")
    }
  })
  
  
  output$wgt_Admin_SensorDetails_SensorTimeseries <- renderDygraph({
    
    ###  To Be Fixed
       # rec <- RV$Admin_SensorSummaryTable_SelectedRow
       # ts <- BE$Shared$Queries$GetSensorDataAsTS(rec$sensorNum)
       # 
       # dg <- dygraph(ts,  ylab =  'Values', group = 'Main') %>%
       #   dyAxis("y", label ='Volumetric Moisture (mm)', valueRange = c(0, 100))
       # dg
  })
  
####. #####
####^ Soil Probes Main Page   ####
####^^ Probe Map  ########################################################################## 
 
  
   
  #####   Draw the main map 
  
  output$wgt_SMmain_ProbeLocationeMap <- renderLeaflet({
    
    
    tbl <- glbCurrentSurfaceSoilMoisture
    pcol <-colorNumeric(palette = 'Blues', tbl$soilmoisture)
    
    pts <- st_as_sf(tbl, coords = c("longitude", "latitude"), crs = 4326)
    
    labs <- lapply(seq(nrow(pts)), function(i) {
      paste0( '<li>Network : ', pts[i, ]$network, '</li>
                  <li>Site Name : ', pts[i, ]$sitename, '</li>

                  <li>Current Value : ', sprintf("%.2f",  pts[i, ]$soilmoisture) , '</li>
                 

                  <li>Access : ', pts[i, ]$access, '</li>
                  <li>Install Date : <b>', pts[i, ]$installdate, '</b></li>

                
              ')})
    
    sid <- paste0(pts$network, '!', pts$sitename)
    
    leaflet::leaflet() %>% addProviderTiles('Esri.WorldImagery') %>%
      setView(lng = 135, lat = -28, zoom = 3) %>%
      addProviderTiles("CartoDB.PositronOnlyLabels") %>%
      onRender(
        "function(el, x) {
          L.control.zoom({position:'topright'}).addTo(this);
        }") %>%
      addCircleMarkers(data=pts, radius=6, color = pcol(tbl$soilmoisture), stroke=FALSE, fillOpacity=1, group="locations", layerId = ~sid, label=lapply(labs, HTML)) %>%
      leaflet::addLegend(title='SM', pal = pcol, values = tbl$soilmoisture,  position = "topleft")
    
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
      rec <- glbSites[glbSites$network==input$wgt_SMmain_Network & glbSites$sitename==input$wgt_SMmain_SiteName,]
  
       lat <- rec$latitude   
       lng <- rec$longitude   
        
      # proxy %>% setView(lng=lng, lat=lat, input$wgt_SMmain_ProbeLocationeMap_zoom) %>% acm_defaults(lng, lat)
       proxy %>% acm_defaults(lng, lat)
       
      sl <- list()
      sl$Network <- input$wgt_SMmain_Network
      sl$SiteName <- input$wgt_SMmain_SiteName
      
      RV$CurrentSite <- sl
      
  })
  

  
  ####^^ Get Site Data and massage it into the various Timeseries #####  
  
  observe({
    req(input$wgt_SMmain_Network, input$wgt_SMmain_SiteName)
   # isolate({
      if(dev){user_temp_dir=NULL}
    #browser()
    
    tryCatch({
      sd <- getAllSiteData(user=user_temp_dir, Network = input$wgt_SMmain_Network, SiteName = input$wgt_SMmain_SiteName, Email=RV$Email, makeTS = T)
      RV$CurrentSiteData <- sd
      
    }, error = function(e) {
      showNotification(
        paste("Failed to get the data for this site :", e$message), 
        type = "error", 
        duration = 10,
        closeButton=T
        
      )
    })
    
    
    

    #    }) 
   
  })
  
  
  
  observe({
    
    req(RV$CurrentSiteSMTimeSeries$rawSM)
    
    updateSliderInput(session, inputId = 'datePker', min = min(RV$CurrentSiteSMTimeSeries$Date),
                      max = max(RV$CurrentSiteSMTimeSeries$Date), 
                      value = max(RV$CurrentSiteSMTimeSeries$Date)
                      
    )
  })

  
  
  
  
  ####^^ Render the Timeseries charts for the site  ##########################################################################   
  
  
  output$wgt_SMmain_volTSChart <- renderDygraph({
    
    req( RV$CurrentSiteData)
    
    mtsV <- RV$CurrentSiteData$TS$volumetric_mm
    
   # bucketSize =  as.integer(sum( (calibs$DrainedUpperLimit - calibs$LowerLimit)/100 * (calibs$IntervalLower-calibs$IntervalUpper)))
   # maxVal <- bucketSize
    
    maxVal <- max(rowSums(mtsV), na.rm = T)
        bits <- str_split(names(mtsV), '_')
        updepth <- as.numeric(sapply(bits, function (x) x[2]))/10
        lowdepth <- glbCalibrationInfo[glbCalibrationInfo$network==RV$CurrentSiteData$Network & glbCalibrationInfo$sitename==RV$CurrentSiteData$SiteName, ]$lowerdepth/10
        names(mtsV) <- paste0(updepth, " to ", lowdepth, ' cm ')
                                                      # , main = 'Total Volumetric Water'
        dygraph(mtsV, , group = 'Main') %>%
          dyAxis("y",  valueRange = c(0, maxVal)) %>%

          dyOptions(axisLineWidth = 1.5, fillGraph = F, drawGrid = T, titleHeight = 26,drawPoints = TRUE) %>%
          dyOptions(colors = RColorBrewer::brewer.pal(11, "Paired"), stackedGraph=T, retainDateWindow = TRUE,drawPoints = TRUE) %>%
          #dyLegend(show = "follow", width = 250, showZeroValues = TRUE, labelsDiv = NULL, labelsSeparateLines = FALSE, hideOnMouseOut = TRUE) %>%
          dyEvent( as.Date(RV$CurrentDate), label = as.Date(RV$CurrentDate), labelLoc = c("top"), color = "black", strokePattern = "solid") %>%
          
          dyLegend(
            labelsDiv = "DyTSlegendDivVolId",
            labelsSeparateLines = TRUE
          )  %>%
          
          dyCallbacks(
            clickCallback = "function(e, x, points) {
          Shiny.setInputValue('chart_clicked_time', x);
          }")
          
       # dyEvent( as.Date(input$datePker), label = as.Date(input$datePker), labelLoc = c("top"), color = "black", strokePattern = "solid")
      #  dyRangeSelector(dateWindow = c(Sys.Date()-20, Sys.Date()), height = 20)
        
  })
  


   output$wgt_SMmain_rawTSChart <- renderDygraph({

     req(RV$CurrentSiteData)


         smTS <-RV$CurrentSiteData$TS$rawProbe

         #maxVal <- max(smTS, na.rm = T)
         maxVal <- max(rowSums(smTS), na.rm = T)

         bits <- str_split(names(smTS), '_')
         updepth <- as.numeric(sapply(bits, function (x) x[2]))/10
         lowdepth <- glbCalibrationInfo[glbCalibrationInfo$network==RV$CurrentSiteData$Network & glbCalibrationInfo$sitename==RV$CurrentSiteData$SiteName, ]$lowerdepth/10
         names(smTS) <- paste0(updepth, " to ", lowdepth, ' cm ')
        
                                        # , main = 'Raw Probe Values'
         dygraph(smTS,  group = 'Main') %>%
          dyAxis("y",  valueRange = c(0, maxVal)) %>%
          # dyEvent( as.Date(RV$CurrentDate), label = as.Date(RV$CurrentDate), labelLoc = c("top"), color = "black", strokePattern = "solid") %>%
           dyOptions(colors = RColorBrewer::brewer.pal(11, "Paired"), stackedGraph=T, retainDateWindow = TRUE) %>%
           dyEvent( as.Date(RV$CurrentDate), label = as.Date(RV$CurrentDate), labelLoc = c("top"), color = "black", strokePattern = "solid") %>%
           
           dyLegend(
             labelsDiv = "DyTSlegendDivRawId",
             labelsSeparateLines = TRUE
           ) %>% 
         dyCallbacks(
           clickCallback = "function(e, x, points) {
          Shiny.setInputValue('chart_clicked_time', x);
          }")
 

     # dyRangeSelector(dateWindow = c(Sys.Date()-20, Sys.Date()), height = 20)

     #}
     })

   output$wgt_SMmain_rainTSChart <- renderDygraph({
     
     req(RV$CurrentSiteData, input$wgt_SMmain_Network, input$wgt_SMmain_SiteName)
     
     if(!is.null(RV$CurrentSiteData$TS$Rainfall)){
       rainTS <- RV$CurrentSiteData$TS$Rainfall 
       #maxVal <- max(rainTS, na.rm = T)
       maxVal <- rainChartMaxYVal
       
       names(rainTS) <- 'Daily Total'
                                       #, main = 'Rainfall'
       dygraph(rainTS,  group = 'Main' ) %>%
         dyBarChart() %>% 
       #  dyLegend(show = "follow", width = 250, showZeroValues = TRUE, labelsDiv = NULL, labelsSeparateLines = FALSE, hideOnMouseOut = TRUE) %>%
         dyAxis("y",  valueRange = c(0, maxVal)) %>%
         #dyEvent( as.Date(input$datePker), label = as.Date(input$datePker), labelLoc = c("top"), color = "black", strokePattern = "solid") %>% 
         
         dyEvent( as.Date(RV$CurrentDate), label = as.Date(RV$CurrentDate), labelLoc = c("top"), color = "black", strokePattern = "solid") %>%
         
         dyRangeSelector(dateWindow = c(Sys.Date()-20, Sys.Date()), height = 20)  %>% 
       dyLegend(
         labelsDiv = "DyTSlegendDivRainId",
         labelsSeparateLines = TRUE
       )   %>% 
       
       dyCallbacks(
         clickCallback = "function(e, x, points) {
          Shiny.setInputValue('chart_clicked_time', x);
          }")
     }
   })
   
   
   output$wgtHTMLBomStation <- renderUI({
     req(RV$CurrentSiteData)
     tags$div(style="font-weight: bold;font-size: 10px !important;font-weight: bold;  line-height: 1.2;", 
              HTML(paste('<BR>BoM Station : ', RV$CurrentSiteData$BoM_Station_Name, '<BR>Distance from Probe : ', RV$CurrentSiteData$Station_Distance, '<BR><BR>')))
   })

   
####  ^^^ Date Selection ####
    output$wgtSelectedDate <- renderUI({
     if(is.null(input$chart_clicked_time)){
       dt = 'None'
     }else{
       seconds_epoch <- input$chart_clicked_time / 1000
       posix_time <- as.POSIXct(seconds_epoch, origin = "1970-01-01", tz = "UTC")
       dt <-  format(posix_time, "%Y-%m-%d")
       RV$CurrentDate <- dt
     }
     tags$div(style="font-weight: 700;font-size: 16px;
              display: grid; place-content: center; height: 50px; color: #140A5E;" ,  HTML(paste('Selected date : ', dt)))
   })

   ### ^^ Render bucket plot  ####
    output$wgt_SMmain_volProfile <- renderPlot({
      req(  RV$CurrentSiteData$TS$volumetric_mmPermm, RV$CurrentDate)
       plotPAWProfile2(sitedata = RV$CurrentSiteData, date = RV$CurrentDate)
    })
   
   ### ^^ Render bucket colour gradient plot  ####
   output$wgt_SMmain_volProfile_colGradient <- renderPlot({
      req(  RV$CurrentSiteData$TS$volumetric_mmPermm)
        plotPAWProfileGradientColours2(sitedata = RV$CurrentSiteData, date = RV$CurrentDate, alwaysImage = T)
   })
   
   ### ^^ Render Gauge  ####
   output$wgt_SMmain_guage <- renderC3({
       req( RV$CurrentSiteData$TS$volumetric_mmPermm)
      plotGauge2(sitedata = RV$CurrentSiteData, date = RV$CurrentDate, alwaysImage = T)
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
     
     ###  TO BE FIXED
     
     # req(input$wgt_SMmain_Network, input$wgt_SMmain_SiteName)
     # df <- BE$Shared$Queries$doSelectQuery(paste0("select  photoNum, Network, SiteName, imageName, imgFormat, theDate, width, height, Description, Type from images where Network='", input$wgt_SMmain_Network,"' and SiteName = '", input$wgt_SMmain_SiteName,"'"))
     # updateSelectInput(session, inputId = 'wgt_SiteInformation_Network_PhotoType', choices=unique(df$Type))
     # RV$CurrentSitePhotos <- df
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
     # sql <- paste0("Select SiteName from Sites where Network = '", input$wgt_Admin_SensorSummaryTable_FilterNetwork, "' order by SiteName ASC")
     # chcs <- BE$Shared$Queries$doSelectQuery(sql)
     
     site <- glbSites[glbSites$network==input$wgt_Admin_SensorSummaryTable_FilterNetwork,]
     
     updateSelectInput(session = session, inputId = 'wgt_Admin_SensorSummaryTable_SiteName', choices = c('All', site ))
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
   
  # s <- BE$Admin$generateDBSummary()
   
  # jsonedit(s)
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
         tbl <- SensorSummary %>% group_by(network, siteName) %>% summarise(Latitude = first(latitude), Longitude = first(longitude), Depth=max(lowerdepth))
         pcol<-colorNumeric(palette = "viridis", tbl$depth)
     
     }else if(mt=='Network'){
         param <- 'Network'
         legTitle = 'Network'
         tbl <- SensorSummary %>% group_by(network, sitename) %>% summarise(Latitude = first(latitude), Longitude = first(longitude))
         pcol<-colorFactor(palette = "viridis", tbl$network)
     
     }else if(mt=='Length of Record'){
         param <- 'Length'
         legTitle = 'Duration'
         tbl <- SensorSummary %>% group_by(network, sitename) %>% summarise(Latitude = first(latitude), Longitude = first(longitude), Length=max(count))
         pcol <-colorNumeric(palette = "plasma", tbl$Length)
   
     }else if(mt=='Latest Surface Moisture'){
          param = 'SoilMoisture'
          legTitle = 'Surface Soil Moisture'
          tbl <- glbCurrentSurfaceSoilMoisture
          pcol <-colorNumeric(palette = 'Blues', tbl$soilmoisture)
   
    }else if(mt=='All Active'){
         param <- 'allActive'
         legTitle = 'All Active'
         sites <- unique(SensorSummary[c('network', 'sitename')])
         odf <- data.frame()
           for (i in 1:nrow(sites)) {
             rec <- sites[i,]
             s <- rec$SiteName   
             n <- rec$Network 
             sensors <- SensorSummary[SensorSummary$network==n & SensorSummary$sitename==s,]
             allActive =  !any(sensors$IsActive==F)
             rdf <- data.frame(Network=n, SiteName=s, Latitude=sensors$latitude[1], Longitude=sensors$longitude[1], allActive=allactive )
             rdf$allactive <- ifelse(rdf$allactive, "Yes", "No")
             odf <- rbind(odf, rdf)
           }
         tbl<-odf
         pcol <- colorFactor(palette = c('green', 'red'), levels = c('Yes', 'No'))
       
     }else if(mt=='Quality Index'){
         param = 'QualityIndex'
         legTitle = 'Quality Index'
         tbl <- SensorSummary %>% group_by(network, sitename) %>% summarise(Latitude = first(latitude), Longitude = first(longitude),  QualityIndex=mean(qualityindex))
         pcol <-colorNumeric(palette = 'RdYlGn', tbl$qualityindex)
   }
   
   pts <- st_as_sf(tbl, coords = c("longitude", "latitude"), crs = 4326)
   

   labs <- lapply(seq(nrow(pts)), function(i) {
     paste0( '<li>Network : ', pts[i, ]$network, '</li>
                  <li>Site Name : ', pts[i, ]$sitename, '</li>
                   <li>', param, ' : ', pts[i, ][[param]], '</li>')})

   
   proxy <- leafletProxy("wgt_Admin_ProbeLocationsMap")
   proxy %>% clearMarkers()
   proxy %>%  clearControls()
   proxy %>%  addCircleMarkers(data=pts, radius=6, color = pcol(tbl[[param]]), stroke=FALSE, fillOpacity=1, group="locations", layerId = ~paste0(network, '!', sitename), label=lapply(labs, HTML)) 

  # proxy %>%  leaflet::addLegend(title=legTitle, pal = pcol, values = tbl[[param]],  position = "bottomright")

   
   
 })
 
 observeEvent(input$wgt_Admin_ProbeLocationsMap_marker_click, { 
   p <- input$wgt_Admin_ProbeLocationsMap_marker_click  # typo was on this line
   bits <- str_split(p$id, '!')
  sdf <- BE$Shared$Reporting$SensorSummary(filter =  paste0(" networks.network = '", bits[[1]][1], "' and sites.sitename = '", bits[[1]][2], "'"))
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
