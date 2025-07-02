##################################################################
#####       Author : Ross Searle                             #####
#####       Date :  Sat Nov 23 11:28:22 2024                 #####
#####       Purpose : Shiny UI for the Admin tab             #####
#####       Comments :                                       #####
##################################################################


Tab_Admin_UI <- function() {
  
  ns <- NS('Tab3')
  
  tabPanel("Admin",  icon = icon("toolbox", tags$style()),
           HTML('<div style="margin-left: 20px; ">'),
  
          tabsetPanel(

             tabPanel("Sensor Summary Table", 
                      
                      fluidRow(HTML('<H4> Filter sensor summary by</h4>')),
                      fluidRow(
                        
                        
                        HTML(paste0('<div style="min-width:80px;float:left;display:inline;text-align:right;margin-right:10px;font-weight: bold;">
                                    <span style="vertical-align: -8px;">Network</span></div>')),
                        HTML(paste0('<div style="max-width:200px;  float:left; display: inline;"><span>', 
                                    selectInput(inputId = 'wgt_Admin_SensorSummaryTable_FilterNetwork', label=NULL, choices = c('All', glbNetworks$Network)),
                                    '</span></div>')),
                        
                        
                        HTML(paste0('<div style="min-width:80px;float:left;display:inline;text-align:right;margin-right:10px;font-weight: bold;">
                                    <span style="vertical-align: -8px;">Site</span></div>')),
                        HTML(paste0('<div style="max-width:200px;  float:left; display: inline;"><span>', 
                                    selectInput(inputId = 'wgt_Admin_SensorSummaryTable_SiteName', label=NULL, choices = NULL),
                                    '</span></div>')),
                        
                        
                        
                        HTML(paste0('<div style="min-width:80px;float:left;display:inline;text-align:right;margin-right:10px;font-weight: bold;">
                                    <span style="vertical-align: -8px;">Datatype</span></div>')),
                        HTML(paste0('<div style="max-width:200px;  float:left; display: inline;"><span> ', 
                             selectInput(inputId = 'wgt_Admin_SensorSummaryTable_FilterDatatype', label=NULL, choices = c('All', BE$Shared$Constants$knownFeatures)),
                                  '</span></div>')),
                        
                       
                        
                       
                        HTML(paste0('<div style="min-width:200px;float:left;display:inline;text-align:right;margin-right:10px;font-weight: bold;">
                                    <span style="vertical-align: -8px;">', actionButton(inputId='wgt_Admin_SensorSummaryTable_ShowButton', label='Show Sensor Info'),
                                    '</span></div>')), 
                        
                        HTML(paste0('<div style="min-width:200px;float:left;display:inline;text-align:right;margin-right:10px;font-weight: bold;">
                                    <span style="vertical-align: -8px;">', shinyjs::hidden(actionButton(inputId='wgtButton_Admin_SensorSummaryTable_ShowTimeseries', label='Show Sensor Timeseries')),
                                    '</span></div>')), 
                        
                      ),
                      
                      #fluidRow( DTOutput('wgt_Admin_SensorSummaryTable'))
                      #fluidRow(  rHandsontableOutput('wgt_Admin_SensorSummaryTable', width = 1500, height = 500))
                      
                      
                      HTML("<H3 style='color:orange;font-weight:700'>Sensors</H3>"), 
                      reactableOutput("wgtAdminNetworksTable")
                      
                      #fluidRow( column(12, dygraphOutput("wgtAdmin_SensorChart", height = "200px"))
                                
                    #)
                     
                      
                      
             ),    
             tabPanel("Sensor Status - TreeView", listviewer::jsoneditOutput('wgtNetworkSummaryListView') ),
             tabPanel("Harvest Logs", reactableOutput("wgtAdminHarvestLogTable") ),
             tabPanel("Site Map - Sensor Status", 
                      selectInput(inputId =  'wgtAdminMapViewType',label = 'ViewType',choices = c('Network', 'Depth', 'Length of Record', 'Latest Surface Moisture', 'All Active', 'Quality Index'), selected = 'Length of Record'),
                      leafletOutput(outputId = 'wgt_Admin_ProbeLocationsMap', height = 700, width = 1000))
           ),
 
           
           HTML('</div>') 
  )}
