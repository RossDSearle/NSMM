
##################################################################
#####       Author : Ross Searle                             #####
#####       Date :  Sat Nov 23 11:13:35 2024                 #####
#####       Purpose :  Shiny UI for the first tab            #####
#####       Comments :                                       #####
##################################################################


mainChartWidth=9
mainLegendWidth=3

Tab_UI_SoilMoisture <- function() {
  
 
  
  
 #ns <- NS('Tab1')

    tabPanel("Moisture Probes",  icon = icon("bore-hole", tags$style()),
          #   HTML('<div style="margin-left: 20px; ">'),
             
             
             # actionLink("toggleBtn", "User Settings"),
             # 
             # # Sliding Panel for managing persistant settings
             # div(id = "slidePanel",
             #     h4("User Settings"),
             #     hr(),
             #     
             #     br(),
             #     # Default Sensor Site
             #     selectInput("wgt_settings_default_Network", "Default Network :", choices = paste0(glbSites$network)),
             #     selectInput("wgt_settings_default_site", "Default Site :", choices = NULL),
             #     br(),
             #     # Email Input
             #     textInput("wgt_settings_user_email", "Email Address:", placeholder = "name@example.com"),
             #     p(tags$div(style="font-size: 11px;",'Your email address is required by SILO to supply the weather data. 
             #       It is not saved on our systems but rather stored as a cookie on your device. 
             #       If your chose not to supply your email address to SILO you will not have access to weather data in this website.')),
             #     
             #     # Trim And fill TS
             #     br(),
             #     checkboxInput("wgt_settings_tidyTS", "Tidy up timeseries"),
             #     p(tags$div(style="font-size: 11px;",'Trim NA data from the start and end of timeseries data as well as fill missing data (linear interpolation) within a timeseries.')),
             #     br(),
             #     actionButton("wgt_settings_save", "Save", class = "btn-primary"),
             #     actionButton("wgt_settings_cancel", "Cancel", class = "btn-primary")
             # ),
             
             
             # sidebarLayout(
             #   sidebarPanel( width = 3,
             #                 HTML('Click on a location in the map to view soil moisture data'),
             #                 leafletOutput("wgt_SMmain_ProbeLocationeMap", width = '100%', height = 400),
             #                 
             #                 HTML('<BR>'),
             #                 selectInput("wgt_SMmain_Network", "Network", choices=glbNetworks$network[2], width = 350, selected='NSMM'),
             #                 selectInput("wgt_SMmain_SiteName", "Site", choices=NULL, width = 350),
             #                 
             #                 # actionLink('wgt_SaveCookie', 'Save as default site'),
             #                 # HTML('<BR><BR>'),
             #                 
             #                # helpText("Select date below"), 
             #                 # sliderInput("datePker", "Dates:", min = as.Date("2025-01-01","%Y-%m-%d"), max = Sys.Date()-5, value=Sys.Date()-5, timeFormat="%Y-%m-%d",
             #                 #             animate = animationOptions(interval = 300, loop = TRUE)),
             #                
             #                 HTML('<BR><BR><BR><BR><BR><BR>')
             #                 
             #                 
             #   ),
             #   mainPanel(
             #     
                 
                 
                 
                 fluidRow( column(12, HTML('<BR>Click on the charts below to select a date to display daily soil moisture diagram for.<BR><BR>'))),
                 fluidRow( column(mainChartWidth, uiOutput('wgtProbeQuality'))),
                 fluidRow( column(mainChartWidth,  uiOutput("graph_or_message_VolTS") ), 
                           column(mainLegendWidth,  tags$div(style="font-weight: bold;font-size: 13px; display: flex; justify-content: flex-start;", HTML('Volumetric Soil<BR>Water (mm).')), 
                                  tags$div(id = "DyTSlegendDivVolId"))  ), 
                 fluidRow( column(mainChartWidth,   uiOutput("graph_or_message_rawProbe") ), 
                           column(mainLegendWidth,  tags$div(style="font-weight: bold;font-size: 13px;", HTML('Raw Probe<BR>Values.')), 
                                  tags$div(id = "DyTSlegendDivRawId"))  ), 
                 fluidRow( column(mainChartWidth,  style = "text-align: right;",   dygraphOutput("wgt_SMmain_rainTSChart", width = "800px", height = "150px")),
                           column(mainLegendWidth,  tags$div(style="font-weight: bold;font-size: 13px; align: left", HTML(paste0('Rainfall (mm).'))), 
                                  htmlOutput(outputId='wgtHTMLBomStation', inline = FALSE), 
                                  tags$div(id = "DyTSlegendDivRainId")) ), 
                 
                 fluidRow( column(mainChartWidth, htmlOutput(outputId='wgtSelectedDate', inline = F))),
                 
                 
                 
                 fluidRow( column(4, plotOutput("wgt_SMmain_volProfile", width = "350px", height = "450px")), 
                           column(3, plotOutput("wgt_SMmain_volProfile_colGradient", width = "300px", height = "450px")),
                           column(5, 
                                 htmlOutput('vtext'),
                                  c3Output("wgt_SMmain_guage", width = "400px", height = "250px")
                                  ),
                          
                        ),
                           
 
                           fluidRow(HTML('<br><br><br><br><br><br><br><br><br><br><br>')),
                           
              #   )
             # ),
             # 
             # HTML('</div>') 
    )}
