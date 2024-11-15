library(raster)
library(stringr)
library(shiny)
library(leaflet)
library(XML)
library(xml2)
library(dygraphs)
library(xts)
library(mapview)
library(leaflet.extras)
library(htmltools)
library(RCurl)
library(jsonlite)
library(shinyBS)
library(htmltools)
library(rhandsontable)
library(RColorBrewer)
library(zip)
library(shinycssloaders)
library(shinyalert)
library(shinyjs)
library(lubridate)
library(raster)
library(httr)
library(shinyMobile)
library(shinyWidgets)
library(leafem)



SHOW_WELCOME_MESSAGE=F

source("Utils.R")
source("driller.R")

# prodDirs <- c('totalbucket', 'SMindex','totalbucket', 'SMindex')
# prodFnames <- c('totalbucket_mm', 'smi_perc','totalbucket_mm', 'smi_perc')
# prodNames <- c('Total Available Soil Water (mm)', 'Proportion Full (%)')
# version <- c('v1.0','v1.0','v1.1.exp','v1.1.exp')

prodDirs <- c('totalbucket', 'SMindex')
prodFnames <- c('totalbucket_mm', 'smi_perc')
prodNames <- c('Total Available Soil Water (mm)', 'Proportion Full (%)')
version <- c('v1.0','v1.0')

AvailableMapProducts <- data.frame(prodDirs, prodNames, prodFnames,version)


AvailableMapProducts <- data.frame(prodDirs, prodNames, prodFnames,version)

smipsRoot <- '/datasets/lw-sm-forecast_work_processed_delivery/SMIPS'


# warning('Getting Dates')
# 
 maxDate <- getMaxDate(paste0(smipsRoot, '/v1.0'))
 minDate <- getMinDate(paste0(smipsRoot, '/v1.0'))
# print(minDate)
# print(maxDate)

#maxDate<-as.Date('2015-12-15')
#minDate<-as.Date('2015-10-15')


sessionStartTime <- as.character(Sys.time())
trans <- 1
TsNumDays <- 30

defExt <- raster::extent(112,154,-45,-10 )
ALWAYS_DRILL=T
SHOW_GPS_BUTTON=F
ENABLE_SITES=F



server <- function(input, output,session) {
  
  
  RV <- reactiveValues()
  RV$currentSiteInfo <- NULL
  RV$currentDownloadTS <- NULL
  RV$RecreateSlider <- TRUE
  RV$sessionStartTime <- as.character(Sys.time())
  
  

  SMIPSDrillbtn <- if (isTRUE(ALWAYS_DRILL)) reactiveVal('SmipsDrillOn') else reactiveVal('SmipsDrillOff')

  
  ################### MAP FUNCTIONS  ################# 
  ################### Render the Map  #################
  
  
  observe({
    req(input$SMIPSDrillbtn)
    if (!is.null(input$SMIPSDrillbtn)) {
      SMIPSDrillbtn(input$SMIPSDrillbtn)
    } })
  
  output$moistureMap <- renderLeaflet({
    
  
    #shinyalert(maxDate)
    # req(input$ProductType)
    
    lf <- leaflet() %>% clearMarkers() %>%
      addTiles(group = "Map") %>%
      #addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satelite Image") %>%
      setView(lng = 135, lat = -28, zoom = 4)
    
    # if (isTRUE(SHOW_GPS_BUTTON)) {
    #   lf <- addControlGPS(lf, options = gpsOptions(setView=T, autoCenter=T, maxZoom=10))
    # }
    sbo <- scaleBarOptions(maxWidth = 200, metric = TRUE, imperial = FALSE, updateWhenIdle = TRUE)
    lf <- addLayersControl(lf,
                          # baseGroups = c("Map"),
                           overlayGroups =  c("Moisture Maps"),
                           options = layersControlOptions(collapsed = FALSE)
    ) %>%
      addScaleBar(position = "bottomleft", options = sbo) %>%
      addMouseCoordinates()  %>%
      #addHomeButton(ext=defExt)

      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to full extent",
        onClick=JS("function(btn, map){ map.setView({lon: 135, lat: -28}, 4); }")))
        #onClick=JS("function(btn, map){ map.setZoom(1); }")))
    # addWMSLegend(uri = paste0(wmsServer, '.nc?VERSION=1.3.0&layer=', prod, '&REQUEST=GetLegendGraphic&FORMAT=image/png'),  position =  "bottomright")
    
    
    lf <- if (isTRUE(ALWAYS_DRILL)) lf else addEasyButtonBar(lf, position = "topright" ,
                                                             easyButton( position = "topright" ,
                                                                         states = list(
                                                                           easyButtonState(
                                                                             stateName="sOff",
                                                                             #icon="fa-check",
                                                                             #icon=htmltools::span(class = "star", htmltools::HTML('<img src="Buttons/drill.png"></font>')),
                                                                             icon=htmltools::span(class = "star", htmltools::HTML('<font color="gray">Get&nbsp;Timeseries&nbsp;<b>Off</b></font>')),
                                                                             title="Click to turn on drilling of a pixel on the map to return a timeseries of soil moisture values",
                                                                             onClick = JS("
              function(btn, map) {
                btn.state('sOn');
                Shiny.onInputChange('SMIPSDrillbtn', 'SmipsDrillOn');
               $('.leaflet-container').css('cursor','crosshair');
              }")
                                                                           ),
                                                                           easyButtonState(
                                                                             stateName="sOn",
                                                                             icon=htmltools::span(class = "star", htmltools::HTML('<font color="green">Get&nbsp;Timeseries&nbsp;<b>On</b></font>')),
                                                                             title="Click to turn off drilling of a pixel on the map to return a timeseries of soil moisture values",
                                                                             onClick = JS("
              function(btn, map) {
                btn.state('sOff');
                Shiny.onInputChange('SMIPSDrillbtn', 'SmipsDrillOff');
                $('.leaflet-container').css('cursor','');
              }")
                                                                           )
                                                                         )
                                                             ))
    lf <- if (isTRUE(ALWAYS_DRILL))
      htmlwidgets::onRender(lf, "
          function(el, x) {
            var moistureMap = this;
            window.moistureMap = moistureMap;
            $('.leaflet-container').css('cursor','crosshair');
          }")
    else
      htmlwidgets::onRender(lf, "
          function(el, x) {
            var moistureMap = this;
            window.moistureMap = moistureMap;
          }")
    
    
  })
  
  
  
 
  
  
  
  observe({
    
    if(is.null(input$moistureMap_bounds))
      return()
    #req(input$map_bounds)
    
    
    #######  Ash's stuff
    if (is.null(trans)) { trans <- 1.0 } # Trans defaults to NULL if the slider is not created yet
    proxy <- leafletProxy("moistureMap")  %>%
      clearGroup("Moisture Maps")
    
    input$moistureMapTrans
    
    if (RV$RecreateSlider == TRUE) {
      sliderCtrl <- div(id = "slider_move_catcher", sliderInput("moistureMapTrans", width=220, label = HTML('<div style="width: 180px; max-height: 10px; position: absolute; margin: 0; left: 70px; top: 0px; text-shadow: 0px 0px 2px white; color: #444; font-size: 13px;">Overlay Opacity</div>'), ticks = F, min = 0.0,  max = 1.0, value = trans, animate = FALSE),
                        onmouseover = "window.moistureMap.dragging.disable();Shiny.onInputChange('SMIPSDrillbtn', 'SmipsDrillOff'); $('.leaflet-container').css('cursor','');", onmouseout = "window.moistureMap.dragging.enable();Shiny.onInputChange('SMIPSDrillbtn', 'SmipsDrillOn');$('.leaflet-container').css('cursor','crosshair');")
      proxy <- addControl(proxy, position = "bottomright", layerId = "overlay_transparency_slider", className = "info", sliderCtrl)
      RV$RecreateSlider <- FALSE
    }
    
    #######
    
    
    mDate <- input$moistureMapDate
    mapyear <- substr(mDate, 1, 4)
    mapMonth <- substr(mDate, 6, 7) 
    mapDay <- substr(mDate, 9, 10) 
    
    rec <- AvailableMapProducts[AvailableMapProducts$prodNames==input$ProductType,]
    print(rec)
   
    isolate({
   
   # proxy <- leafletProxy("moistureMap")
    bds <- input$moistureMap_bounds
    
    tmpfl = tempfile(fileext = ".tif")
    
    zl <- max(input$moistureMap_zoom, 1) *3
    
    print(rec$prodDirs)
    #c('totalbucket', 'bucket1', 'bucket2', 'deepD', 'runoff')
    if(rec$prodDirs =='totalbucket'){
      pal <- colorNumeric(c("#f7fbff", "#6baed6", "#08306b"), c(0,2,5,10,15,30,50,75,100,150), na.color = "transparent")
      clampVal<-150
    }else if(rec$prodDirs =='SMindex'){
      pal <- colorNumeric(c("#f7fbff", "#6baed6", "#08306b"), c(0, 0.5, 1), na.color = "transparent")
      clampVal <- 1
    }
    
    print('GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG')
    
    # else if(rec$prodDirs =='bucket1'){
    #   pal <- colorNumeric(c("#f7fbff", "#6baed6", "#08306b"), c(0, 1, 3, 6, 10, 15, 20 ), na.color = "transparent")
    #   clampVal<-120
    # }else if(rec$prodDirs =='bucket2'){
    #   pal <- colorNumeric(c("#f7fbff", "#6baed6", "#08306b"), c(0,2,5,10,15,30,50,75,100,150), na.color = "transparent")
    #   clampVal<-150
    # }else if(rec$prodDirs =='deepD'){
    #   pal <- colorNumeric(c("#fff5f0" , "#fb6a4a", "#a50f15"), c(0, 1, 3, 6, 10, 15, 20 ), na.color = "transparent")
    #   clampVal<-20
    # }else if(rec$prodDirs =='runoff'){
    #   pal <- colorNumeric(c("#efedf5", "#807dba", "#3f007d" ),c(0, 1, 3, 6, 10, 15, 20 ), na.color = "transparent")
    #   clampVal<-20
    # }
    
    we <- max(bds$west, defExt@xmin)
    ee <- min(bds$east, defExt@xmax)
    se <-  max(bds$south, defExt@ymin)
    ne <- min(bds$north, defExt@ymax)
    
    #fname <- paste0(smipsRoot, '/', rec$prodDirs, '/', mapyear, '/smips_', rec$prodDirs, '_', mapyear, mapMonth, mapDay, '.tif' )
    fname <- paste0(smipsRoot, '/', rec$version, '/', rec$prodDirs, '/', mapyear, '/smips_', rec$prodFnames, '_', mapyear, mapMonth, mapDay, '.tif' )

    cmd <-paste0('gdal_translate ', fname, ' ', tmpfl, ' -r nearest -q -outsize ', zl,'% ', zl,'% -projwin ', we, ' ', ne, ' ', ee, ' ', se)
    print(cmd)
    op <- system(cmd, intern = T)
    
    r <- raster(tmpfl)
    r <- clamp(r, lower=0, upper=clampVal, useValues=T)
    crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
   

   suppressWarnings(proxy %>% leaflet::addRasterImage(r, group = "Moisture Maps", colors = pal,opacity = input$moistureMapTrans))
    
    unlink(tmpfl)
     })
    
    if (RV$RecreateSlider == TRUE) {
      sliderCtrl <- div(id = "slider_move_catcher", sliderInput("moistureMapTrans", width=220, label = HTML('<div style="width: 180px; max-height: 10px; position: absolute; margin: 0; left: 70px; top: 20px; text-shadow: 0px 0px 2px white; color: #444; font-size: 13px;">Overlay Opacity</div>'), ticks = F, min = 0.0,  max = 1.0, value = trans, animate = FALSE),
                        onmouseover = "window.moistureMap.dragging.disable();Shiny.onInputChange('SMIPSDrillbtn', 'SmipsDrillOff'); $('.leaflet-container').css('cursor','');", onmouseout = "window.moistureMap.dragging.enable();Shiny.onInputChange('SMIPSDrillbtn', 'SmipsDrillOn');$('.leaflet-container').css('cursor','crosshair');")
      proxy <- addControl(proxy, position = "bottomright", layerId = "overlay_transparency_slider", className = "info", sliderCtrl)
      RV$RecreateSlider <- FALSE
    }
  })
  
  
   output$wmsLegend <- renderUI({
     rec <- AvailableMapProducts[AvailableMapProducts$prodNames==input$ProductType,]
       imageLoc <- paste0('./Legends/', rec$prodDirs, '_legend.png')
       div_style <- paste0("box-sizing: content-box; border: 1px solid black; width: 150px; height: 260px; background-image: url(\'", imageLoc, "\');  background-repeat: no-repeat;")
       div(style=div_style)
   })
 
  
  ##############      MAP QUERYING FUNCTIONS  ##############################
  ################## Render the Chart from a map drill  ##################
  output$dummyDygraph <- renderDygraph({})
  output$moisitureChart1 <- renderDygraph({
    
    
    if(!is.null(RV$currentTS)){
      
      isolate({
        
        maxVal <- max(RV$currentTS)
        
        dygraph(RV$currentTS ,  main=paste0(input$ProductType), ylab=RV$currentSiteInfo$DataType, width=50, height=100) %>%
          dyAxis("y", label = RV$currentSiteInfo$DataType, valueRange = c(0, maxVal)) %>%
          dyOptions(axisLineWidth = 1.5, fillGraph = F, drawGrid = T, titleHeight = 25) %>%
          dyRangeSelector()
        
      })
    }
  })
  
  
  output$showDyGraph <- reactive({ifelse(is.null(RV$currentTS), "false", "true")})

  
  
  
  
  ################  Get data from drilling a map pixel ##########
  observeEvent(input$moistureMap_click, {
    
    click <- input$moistureMap_click
    
    if(is.null(click))
      return()
    
    req(SMIPSDrillbtn)
    if(SMIPSDrillbtn() == 'SmipsDrillOn')
      
      if(input$ProductType != 'None'){

        minDateShiny <- input$moistureDateRange[[1]][1]
        maxDateShiny  <- input$moistureDateRange[[2]][1]
        sd <- str_split(minDateShiny, '-')
        minDate <- paste0(sd[[1]][3], '-', sd[[1]][2], '-', sd[[1]][1])
        ed <- str_split(maxDateShiny, '-')
        maxDate <- paste0(ed[[1]][3], '-', ed[[1]][2], '-', ed[[1]][1])
        
        shinyBS::createAlert(session, "progressAlert", "drillingAlertInstance", title = "", content = paste0('<img src=wait.gif>&nbsp;&nbsp;Retrieving timeseries data from <font color=green>', input$ProductType, '</font> at location ', sprintf(click$lng, fmt = '%#.4f'), " ", sprintf(click$lat, fmt = '%#.4f') ), append = FALSE)
        rec <- AvailableMapProducts[AvailableMapProducts$prodNames==input$ProductType,]
        vPath <- paste0(smipsRoot, '/', rec$version)
         ts <- drillSMIPS(click$lng,click$lat, product = input$ProductType, smipsRoot=vPath, sdate = minDate, edate = maxDate, AvailableMapProducts=AvailableMapProducts)
        
        validTS <- !all(ts[,1] < -100) 
        
         if(validTS){

           sli <- list()
           sli$Longitude <- click$lng
           sli$Latitude  <- click$lat
           sli$EndDate <- maxDate
           sli$StartDate <- minDate

           RV$currentTS <- ts
           RV$currentSiteInfo <- sli
          
         }else{
           
           shinyalert(
             title = "Oops",
             text = "There is no data for this location.",
             #size = "xs", 
             closeOnEsc = TRUE,
             closeOnClickOutside = FALSE,
             html = FALSE,
             type = "info",
             showConfirmButton = TRUE,
             showCancelButton = FALSE,
             confirmButtonText = "OK",
             confirmButtonCol = "#305234",
             timer = 0,
             imageUrl = "",
             animation = TRUE
           )
           RV$currentSiteInfo <- NULL
           RV$currentTS <- NULL
         }
         
        shinyBS::closeAlert(session, "drillingAlertInstance")
      }
    
    
  })
  
  
  ############################    DYNAMIC IU FUNCTIONS   ##############
  ################ Update List Choices   ###############################
  # update the MapProduct List choices depending on login  
  observe({
    updateSelectInput(session, "ProductType", choices =  AvailableMapProducts$prodNames, selected = AvailableMapProducts$prodNames[2] )
    #updateSelectInput(session, "MapProductsListForDownload", choices =  RV$AvailableMapProducts[-1] )
  })
 
  observe({
    
    if(is.null(RV$currentTS)){
      chs <- c("Full Map", "Current View Extent")
    }else{
      chs <- c("Full Map", "Current View Extent", "Current TimeSeries")
    }
    updateSelectInput(session, "DownloadType", choices = chs )
   
  })
  
  
  
  
  output$helpMessage <- renderText({"
<p>Click anywhere on the map to display a modelled soil moisture timeseries chart for the selected modelled product. The timeseries will be displayed below the map.</p>
"})
  
  

  
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      
      mDate <- input$moistureMapDate
      mapyear <- substr(mDate, 1, 4)
      mapMonth <- substr(mDate, 6, 7) 
      mapDay <- substr(mDate, 9, 10) 
      rec <- AvailableMapProducts[AvailableMapProducts$prodNames==input$ProductType,]
      
      if(input$DownloadType == "Full Map"){
        fname <- paste0('smips_', rec$prodFnames, '_mm_', mapyear, mapMonth, mapDay, '.tif' )
        paste0(fname)
      }else if(input$DownloadType == "Current View Extent"){
        fname <- paste0('smips_', rec$prodFnames, '_mm_', mapyear, mapMonth, mapDay, '_Clipped.tif' )
        paste0(fname)
      }else if(input$DownloadType == "Current TimeSeries"){
        lon <- sprintf(RV$currentSiteInfo$Longitude, fmt = '%#.4f')
        lat <- sprintf(RV$currentSiteInfo$Latitude, fmt = '%#.4f')
        tsname <- paste0('SMIPS_Timeseries_', lon, '_', lat,'.csv')
        paste0(tsname)
      }else{
        
      }

    },
    content = function(file) {
      
      mDate <- input$moistureMapDate
      mapyear <- substr(mDate, 1, 4)
      mapMonth <- substr(mDate, 6, 7) 
      mapDay <- substr(mDate, 9, 10) 
      rec <- AvailableMapProducts[AvailableMapProducts$prodNames==input$ProductType,]
      
      if(input$DownloadType == "Full Map"){
        
        fullname <- paste0(smipsRoot, '/',rec$version, '/', rec$prodDirs, '/', mapyear, '/smips_', rec$prodFnames, '_', mapyear, mapMonth, mapDay, '.tif' )
        fs <- file.info(fullname)$size
        logDownload(dateTime = as.character(Sys.time()), product=paste0('smips_', rec$prodFnames, '_mm_', mapyear, mapMonth, mapDay),	
                    type='Raster',	minX=defExt@xmin,	minY=defExt@ymin,	maxX=defExt@xmax,	maxY=defExt@ymax,	size=fs)
        file.copy(fullname, file)
        
      }else if(input$DownloadType == "Current View Extent"){
        
        bds <- input$moistureMap_bounds
        
        we <- max(bds$west, defExt@xmin)
        ee <- min(bds$east, defExt@xmax)
        se <-  max(bds$south, defExt@ymin)
        ne <- min(bds$north, defExt@ymax)
        
        tmpfl = tempfile(fileext = ".tif")
        fname <- paste0(smipsRoot, '/',rec$version, '/', rec$prodDirs, '/', mapyear, '/smips_', rec$prodFnames, '_', mapyear, mapMonth, mapDay, '.tif' )
        cmd <-paste0('gdal_translate ', fname, ' ', tmpfl, ' -q -projwin ', we, ' ', ne, ' ', ee, ' ', se)
        print(cmd)
        
        op <- system(cmd, intern = T)
        fs <- file.info(tmpfl)$size
        logDownload(dateTime = as.character(Sys.time()), product=paste0('smips_', rec$prodFnames, '_mm_', mapyear, mapMonth, mapDay),	
                    type='Raster',	minX=we,	minY=se,	maxX=ee,	maxY=ne,	size=fs)
        
        file.copy(tmpfl, file)
        unlink(tmpfl)
  
      }else if(input$DownloadType == "Current TimeSeries"){
        df <- data.frame(date=index(RV$currentTS), coredata(RV$currentTS))
        colnames(df) <- c('Date', 'Value')
        logDownload(dateTime = as.character(Sys.time()), product=paste0('smips_', rec$prodFnames, '_mm_', mapyear, mapMonth, mapDay),	
                    type='TimeSeries',	minX= RV$currentSiteInfo$Longitude,	minY= RV$currentSiteInfo$Latitude,	maxX= RV$currentSiteInfo$StartDate,	maxY= RV$currentSiteInfo$EndDate,	size='')
        write.csv( df, file, row.names = FALSE)
        
      }else{
        
      }
      
    }
  )
  
  
  
  session$onSessionEnded(function() {
    
   
    logSession(startDateTime=sessionStartTime, endDateTime=as.character(Sys.time()))
    
    cat("Session Ended\n")
  })
  
}




ui <- navbarPage(windowTitle = 'SMIPS', theme = 'bootstrap.css', fluid = FALSE, inverse = TRUE,
                 title = HTML('<a href="https://www.csiro.au/" class="navbar-left" target=_blank>&nbsp;<img src="Logos/CSIRO-logo-inverted.svg" width="60" height="60"></a>&nbsp;&nbsp;<a href="https://www.tern.org.au/" class="navbar-left" target=_blank><img src="Logos/TERN-allwhite.png" height="45"></img></a><span>&nbsp;&nbsp;The National Soil Moisture Information Processing System</span>'),
                
                 header = tagList(
                   tags$head( # Start <head>
                     tags$link(rel = "stylesheet", type = "text/css", href = "Styles/csiro.css"),
                     # tags$style(HTML("label {  font-size: 12px; }")),
                     #tags$head(tags$script(src = "message-handler.js")),
                     useShinyjs(),
                    # useShinyalert(),
                     tags$link(rel = "icon", href = "favicon.ico"),
                    # tags$style(appCSS),
                     includeHTML(("GA.html")),
                     tags$style(' #crosshair-button {
position: relative;
top: 10px;
left: 70px;
}')
                   ), 
                   
                   tags$style( )
                   
                   # End </head>
                   # Start <body>
                   # Header above panes (but below the nav)
                   
                 ), # End header taglist
                 # Start tab panel list:
                 tabPanel("Map View",
                          sidebarLayout(fluid = FALSE,  
                                        sidebarPanel(width = 4, height='850px', 
                                                     div(class = 'container-fluid',
                                                         
                                                         fluidRow(htmlOutput("helpMessage")),
                                                         fluidRow( HTML('&nbsp;<br>')),
                                                         fluidRow(selectInput("ProductType", "Map Product Type ", choices = NULL, width='100%')),
                                                         fluidRow(dateInput('moistureMapDate', label = 'Map Date', format = 'dd-mm-yyyy', value = maxDate, width = 130, max = maxDate)),
                                                         
                                                         
                                                        # fluidRow( column(12, actionLink("init", "Download Map Data", icon = icon("download")))),
                                                         fluidRow( HTML('&nbsp;<br>')),
                                                        # fluidRow( HTML('&nbsp;<br>')),
                                                         fluidRow(column(12, dateRangeInput('moistureDateRange',label = 'Timeseries Date Range',start = maxDate-TsNumDays, end = maxDate, max = maxDate, min=minDate),)),
                                                         
                                                         # conditionalPanel('output.showTSLink == "true"',
                                                         #                  fluidRow( column(12, actionLink("initTS", "Download Pixel Drill Data", icon = icon("download"))))
                                                         # ),
                                                        fluidRow( HTML('&nbsp;<br>')),
                                                        fluidRow(selectInput("DownloadType", "Data to Download ", choices = NULL, width='50%'),
                                                        downloadBttn(outputId = "downloadData", style = "jelly", color = "success"))

                                                     )
                                        ),
                                        
                                        
                                        mainPanel( width = 8,
                                                   
                                                   
                                                   fluidRow( column(12,
                                                                    
                                                                    tags$style(' .leaflet-bar button,
.leaflet-bar button:hover {
background-color: #fff;
border: none;
border-bottom: 1px solid #ccc;
width: 30px;
height: 30px;
line-height: 30px;
display: block;
text-align: center;
text-decoration: none;
color: black;
}

.leaflet-bar button {
background-position: 50% 50%;
background-repeat: no-repeat;
overflow: hidden;
display: block;
}

.leaflet-bar button:hover {
background-color: #f4f4f4;
}

.leaflet-bar button:first-of-type {
border-top-left-radius: 1px;
border-top-right-radius: 1px;
}

.leaflet-bar button:last-of-type {
border-bottom-left-radius: 1px;
border-bottom-right-radius: 1px;
border-bottom: none;
}

.leaflet-bar.disabled,
.leaflet-bar button.disabled {
cursor: default;
pointer-events: none;
opacity: .4;
}

.easy-button-button .button-state{
display: block;
width: 100%;
height: 100%;
position: relative;
}

.bttn-jelly.bttn-success {
    background: #003b29;
    color: #fff;
    bottom: 12px;
    left: 20px
}

.bttn-jelly.bttn-md {
    FONT-WEIGHT: 100;
    font-size: 13px;
    font-family: inherit;
    padding: 5px 12px;
}

.shiny-input-container:not(.shiny-input-container-inline) {
    display: inline-block;
    width: 300px;
    max-width: 70%;
}


.leaflet-touch .leaflet-bar button {
width: 30px;
height: 30px;
line-height: 30px;

.leaflet-container.crosshair-cursor-enabled {
cursor:crosshair;
}

}

.well {

  height: 530px;
}

.shiny-input-container:not(.shiny-input-container-inline) {
    display: inline-block;
    width: 300px;
    max-width: 100%;
}


.info {
    padding: 6px 8px;
    font: 14px/16px Arial, Helvetica, sans-serif;
    background: white;
    background: rgba(255,255,255,0.8);
    box-shadow: 0 0 15px rgb(0 0 0 / 20%);
    border-radius: 5px;
    left: -320px;
    bottom: -20px;
    height: 60px;
}
 '                                                                   ),
                                                                    
                                                                    if (isTRUE(SHOW_WELCOME_MESSAGE)) div( class = "panel panel-default", div(class = "panel-heading", HTML("Welcome to SMIPS")), div( class = "panel-body", htmlOutput("welcomeMessage"), style=paste0('color:', 'green', '; width: 850px;'))) else div()
                                                   )),
                                                   
                                                   fluidRow(
                                                     column(12,  leafletOutput("moistureMap", height = 830, width=1000)),
                                                     absolutePanel(htmlOutput("wmsLegend"), style=" top: 1px; right:-150px; height:250px;")) ,
                                                   fluidRow(column(12, bsAlert("progressAlert"))),
                                                   dygraphOutput("dummyDygraph", height = "0px"), #Dummy dygraph to ensure loading of dygraph libraries at boot time, even when none are shown
                                                   div(style="visibility:hidden;",verbatimTextOutput("showDyGraph")), # Dummy invisible output of showDygraph to ensure its included in the conditional render
                                                   
                                                   conditionalPanel('output.showDyGraph == "true"',
                                                                    fluidRow( column(12, dygraphOutput("moisitureChart1", height = "150px"))),
                                                                    #fluidRow( HTML("<BR><BR>")),
                                                                    #fluidRow( column(12, rHandsontableOutput("SensorDrillInfoTable")))
                                                   ),
                                                   div(style="visibility:hidden;",verbatimTextOutput("showTSLink")), 
                                        ))),
                 
                 tabPanel("About", "",
                          includeHTML(paste0( "www/StaticPages/About.html"))
                 ),
                 footer = div(class = 'footer'
                   #           hr(),
                   #            fluidRow( column(12,
                   #                             HTML('<div style="margin: 0px auto 0px auto; width:450px;"><img src="Logos/TERN-NCRIS-digital-Primary.jpg" width="50%"></img></div>')
                   #            )),
                   #            #Trick to escape the fixed-size container for the full-width lower-footer
                   #            HTML('</div><!--close footer--></div><!--close row--></div><!--close container-->
                   # <div class="container-fluid"><div class="row lower-footer"><div class="col-sm-12"><p>&nbsp;<br/>&nbsp;<br/></p>'
                   #                 )
                 )
) 





shinyApp(ui = ui, server = server)


