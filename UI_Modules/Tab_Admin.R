##################################################################
#####       Author : Ross Searle                             #####
#####       Date :  Sat Nov 23 11:28:22 2024                 #####
#####       Purpose : Shiny UI for the Admin tab             #####
#####       Comments :                                       #####
##################################################################


Tab_Admin_UI <- function() {
  
  ns <- NS('Tab3')
  
  tabPanel("Admin",  icon = icon("file-lines", tags$style()),
           HTML('<div style="margin-left: 20px; ">'),
  
          tabsetPanel(

             tabPanel("Sensor Summary Table", 
                      
                      fluidRow(HTML('<H4> Filter sensor summary by</h4>')),
                      fluidRow(
                        HTML(paste0('<div style="min-width:80px;float:left;display:inline;text-align:right;margin-right:10px;font-weight: bold;">
                                    <span style="vertical-align: -8px;">Datatype</span></div>')),
                        HTML(paste0('<div style="max-width:200px;  float:left; display: inline;"><span> ', 
                             selectInput(inputId = 'wgt_Admin_SensorSummaryTable_FilterDatatype', label=NULL, choices = c('None', BE$Shared$Constants$knownFeatures)),
                                  '</span></div>')),
                        
                        HTML(paste0('<div style="min-width:80px;float:left;display:inline;text-align:right;margin-right:10px;font-weight: bold;">
                                    <span style="vertical-align: -8px;">Network</span></div>')),
                        HTML(paste0('<div style="max-width:200px;  float:left; display: inline;"><span>', 
                                    selectInput(inputId = 'wgt_Admin_SensorSummaryTable_FilterNetwork', label=NULL, choices = c('None', glbNetworks)),
                                    '</span></div>')),
                        
                        HTML(paste0('<div style="min-width:80px;float:left;display:inline;text-align:right;margin-right:10px;font-weight: bold;">
                                    <span style="vertical-align: -8px;">Is Active</span></div>')),
                        HTML(paste0('<div style="max-width:200px;  float:left; display: inline;"><span>', 
                                    selectInput(inputId = 'wgt_Admin_SensorSummaryTable_FilterIsActive', label=NULL, choices = c('None', 'TRUE', 'FALSE')),
                                    '</span></div>')),
                        
                        HTML(paste0('<div style="min-width:200px;float:left;display:inline;text-align:right;margin-right:10px;font-weight: bold;">
                                    <span style="vertical-align: -8px;">', actionButton(inputId='wgt_Admin_SensorSummaryTable_ShowButton', label='Show Sensor Summary'),
                                    '</span></div>')), 
                        
                        HTML(paste0('<div style="min-width:200px;float:left;display:inline;text-align:right;margin-right:10px;font-weight: bold;">
                                    <span style="vertical-align: -8px;">', actionButton(inputId='wgt_Admin_SensorSummaryTable_ShowTimeseries', label='Show Sensor Timeseries'),
                                    '</span></div>')), 
                        
                      ),
                      
                      #fluidRow( DTOutput('wgt_Admin_SensorSummaryTable'))
                      fluidRow(  rHandsontableOutput('wgt_Admin_SensorSummaryTable', width = 1500, height = 500))
             ),    

             tabPanel("Site Map - Sensor Status", tableOutput("table"))
           ),
 
           
           HTML('</div>') 
  )}
