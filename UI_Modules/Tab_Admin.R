##################################################################
#####       Author : Ross Searle                             #####
#####       Date :  Sat Nov 23 11:28:22 2024                 #####
#####       Purpose : Shiny UI for the Admin tab             #####
#####       Comments :                                       #####
##################################################################


Tab_Admin_UI <- function() {
  
  ns <- NS('Tab3')
  
hidden(tabPanel("Admin",  icon = icon("toolbox", tags$style()), 
           HTML('<div style="margin-left: 20px; ">'),
  
          tabsetPanel(

            tabPanel("Sensor Sites Map", 
                     fluidRow(
                          HTML(paste0('<div style=" padding-top: 10px;">',
                         
                           HTML(paste0('<div style="min-width:80px;float:left;display:inline;text-align:right;margin-right:10px;font-weight: bold;">
                                          <span style="vertical-align: -8px;">ViewType</span></div>')),
                           HTML(paste0('<div style="max-width:200px;  float:left; display: inline;"><span>', 
                           selectInput(inputId =  'wgtAdminMapViewType',label = NULL, ,choices = c('Latest Surface Moisture','Network', 'Depth', 'Length of Record',  'All Active', 'Quality Index'), selected = ''),
                           '</span></div>')),
                           
                           HTML(paste0('<div style="max-width:1000px; color: #ba4a00; padding-left: 40px; float:left; display: inline;"><span style="vertical-align: -8px;">
                                       Click on the map markers to view detailed site information.</span></div>')),
                           '</div>'
                           ))
                        ),
                         
                         leafletOutput(outputId = 'wgt_Admin_ProbeLocationsMap', height = 700, width = 1000),

                     ),
                     
            
             tabPanel("Sensors Summary Table", 
                      
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
                             selectInput(inputId = 'wgt_Admin_SensorSummaryTable_FilterDatatype', label=NULL, choices = c('All', glbSensorDataTypes)),
                                  '</span></div>')),
                        
                       
                        
                       
                        HTML(paste0('<div style="min-width:200px;float:left;display:inline;text-align:right;margin-right:10px;font-weight: bold;">
                                    <span style="vertical-align: -8px;">', actionButton(inputId='wgt_Admin_SensorSummaryTable_ShowButton', label='Show Sensor Info'),
                                    '</span></div>')), 
                        
                        HTML(paste0('<div style="min-width:200px;float:left;display:inline;text-align:right;margin-right:10px;font-weight: bold;">
                                    <span style="vertical-align: -8px;">', shinyjs::hidden(actionButton(inputId='wgtButton_Admin_SensorSummaryTable_ShowTimeseries', label='Show Sensor Timeseries')),
                                    '</span></div>')), 
                        
                      ),
                      HTML("<H3 style='color:orange;font-weight:700'>Sensors</H3>"), 
                      reactableOutput("wgtAdminNetworksTable")
                      
             ),    
             tabPanel("Sensors TreeView", listviewer::jsoneditOutput('wgtNetworkSummaryListView') ),
             
            
             tabPanel("Harvest Logs", 
                      selectInput(inputId =  'wgtAdminHarvestLogDates',label = 'Harvest Log Dates', choices = glbHarvestDates$DT, selected = glbHarvestDates[1,1]),
                      htmlOutput('wgtHarvestSummaryHTML'),
                      reactableOutput("wgtAdminHarvestLogTable") )
           ),
 
           
           HTML('</div>') 
          
)
  )}
