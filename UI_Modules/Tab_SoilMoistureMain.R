
##################################################################
#####       Author : Ross Searle                             #####
#####       Date :  Sat Nov 23 11:13:35 2024                 #####
#####       Purpose :  Shiny UI for the first tab            #####
#####       Comments :                                       #####
##################################################################


Tab1_UI <- function() {
  
 ns <- NS('Tab1')

    tabPanel("Moisture Probes",  icon = icon("bore-hole", tags$style()),
             HTML('<div style="margin-left: 20px; ">'),
             
             sidebarLayout(
               sidebarPanel( width = 3,
                             HTML('Click on a location in the map to view soil moisture data'),
                             leafletOutput("wgt_SMmain_ProbeLocationeMap", width = 400, height = 400),
                             
                             HTML('<BR><BR>'),
                             selectInput("wgt_SMmain_Network", "Network", choices=glbNetworks$Network, width = 350, selected='NSMM'),
                             selectInput("wgt_SMmain_SiteName", "Site", choices=NULL, width = 350),
                           #  helpText("Select map date below"), 
                          #   sliderInput("wgt_SMmain_datePker", "Dates:", min = as.Date("2017-01-01","%Y-%m-%d"), max = as.Date("2021-09-01","%Y-%m-%d"), value=as.Date("2021-07-01"), timeFormat="%Y-%m-%d",
                          #               animate = animationOptions(interval = 300, loop = TRUE)),
                            # sliderInput("tempCor", "Temp Correction:", min = 0, max = 0.5, value=0.3),
                             
                             HTML('<BR><BR><BR><BR><BR><BR>')
                             
                             
               ),
               mainPanel(
                 
                 fluidRow(  dygraphOutput("wgt_SMmain_volTSChart", width = "800px", height = "250px"), HTML(''), 
                            dygraphOutput("wgt_SMmain_rawTSChart", width = "800px", height = "150px"),
                            dygraphOutput("wgt_SMmain_rainTSChart", width = "800px", height = "150px"),
                            dygraphOutput("wgt_testddd", width = "800px", height = "0px"),
                 ),
                 
                 fluidRow( column(6, plotOutput("wgt_SMmain_volProfile", width = "450px", height = "450px")), 
                           column(6, 
                                  fluidRow( htmlOutput('vtext') )),
                           fluidRow(plotOutput("wgt_SMmain_dial", width = "450px", height = "450px")),
                           fluidRow(dygraphOutput("wgt_SMmain_seasons", width = "800px", height = "250px")),
                           fluidRow(HTML('<br><br><br><br><br><br><br><br><br><br><br>')),
                           
                 ))
             ),
             
             HTML('</div>') 
)}