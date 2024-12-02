library(shiny)
library(stringr)

library(leaflet)
# library(XML)
# library(xml2)
library(dygraphs)
library(xts)
#library(mapview)
library(leaflet.extras)
#library(htmltools)
#library(RCurl)
library(jsonlite)
library(shinyBS)
#library(htmltools)
# library(rhandsontable)
# library(RColorBrewer)
# library(zip)
# library(shinycssloaders)
# library(shinyalert)
 library(shinyjs)
library(lubridate)
#library(raster)
library(httr)

library(shinyWidgets)
library(leafem)


machineName <- as.character(Sys.info()['nodename'])
if (machineName=='soils-discover2') {
 
}else{
 
}



sessionStartTime <- as.character(Sys.time())
trans <- 1
TsNumDays <- 30




server <- function(input, output,session) {
  
  
  RV <- reactiveValues()
  
  
  
  session$onSessionEnded(function() {
    #logSession(startDateTime=sessionStartTime, endDateTime=as.character(Sys.time()))
    cat("Session Ended\n")
  })
  
}




ui <- navbarPage(windowTitle = 'SMIPS', theme = 'bootstrap.css', fluid = FALSE, inverse = TRUE,
                 title = HTML('<a href="https://www.csiro.au/" class="navbar-left" target=_blank>&nbsp;
                              <img src="Logos/CSIRO-logo-inverted.svg" width="60" height="60"></a>
                              &nbsp;&nbsp;<a href="https://www.tern.org.au/" class="navbar-left" target=_blank>
                              <img src="Logos/TERN-allwhite.png" height="45"></img></a>
                              <span>&nbsp;&nbsp;The National Soil Moisture Information System</span>'),
                
                 header = tagList(
                   tags$head( # Start <head>
                     #tags$link(rel = "stylesheet", type = "text/css", href = "Styles/csiro.css"),
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
                                                         
                                                        #  fluidRow(htmlOutput("helpMessage")),
                                                        #  fluidRow( HTML('&nbsp;<br>')),
                                                        #  fluidRow(selectInput("ProductType", "Map Product Type ", choices = NULL, width='100%')),
                                                        #  fluidRow(dateInput('moistureMapDate', label = 'Map Date', format = 'dd-mm-yyyy', value = maxDate, width = 130, max = maxDate)),
                                                        #  
                                                        #  
                                                        # # fluidRow( column(12, actionLink("init", "Download Map Data", icon = icon("download")))),
                                                        #  fluidRow( HTML('&nbsp;<br>')),
                                                        # # fluidRow( HTML('&nbsp;<br>')),
                                                        #  fluidRow(column(12, dateRangeInput('moistureDateRange',label = 'Timeseries Date Range',start = maxDate-TsNumDays, end = maxDate, max = maxDate, min=minDate),)),
                                                        #  
                                                        #  # conditionalPanel('output.showTSLink == "true"',
                                                        #  #                  fluidRow( column(12, actionLink("initTS", "Download Pixel Drill Data", icon = icon("download"))))
                                                        #  # ),
                                                        # fluidRow( HTML('&nbsp;<br>')),
                                                        # fluidRow(selectInput("DownloadType", "Data to Download ", choices = NULL, width='50%'),
                                                        # downloadBttn(outputId = "downloadData", style = "jelly", color = "success"))

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
                                                                    
                                                                   # if (isTRUE(SHOW_WELCOME_MESSAGE)) div( class = "panel panel-default", div(class = "panel-heading", HTML("Welcome to SMIPS")), div( class = "panel-body", htmlOutput("welcomeMessage"), style=paste0('color:', 'green', '; width: 850px;'))) else div()
                                                   )),
                                                   
                                                   # fluidRow(
                                                   #   column(12,  leafletOutput("moistureMap", height = 830, width=1000)),
                                                   #   absolutePanel(htmlOutput("wmsLegend"), style=" top: 1px; right:-150px; height:250px;")) ,
                                                   # fluidRow(column(12, bsAlert("progressAlert"))),
                                                   # dygraphOutput("dummyDygraph", height = "0px"), #Dummy dygraph to ensure loading of dygraph libraries at boot time, even when none are shown
                                                   # div(style="visibility:hidden;",verbatimTextOutput("showDyGraph")), # Dummy invisible output of showDygraph to ensure its included in the conditional render
                                                   # 
                                                   # conditionalPanel('output.showDyGraph == "true"',
                                                   #                  fluidRow( column(12, dygraphOutput("moisitureChart1", height = "150px"))),
                                                   #                  #fluidRow( HTML("<BR><BR>")),
                                                   #                  #fluidRow( column(12, rHandsontableOutput("SensorDrillInfoTable")))
                                                   # ),
                                                   # div(style="visibility:hidden;",verbatimTextOutput("showTSLink")), 
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


