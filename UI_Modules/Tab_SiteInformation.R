##################################################################
#####       Author : Ross Searle                             #####
#####       Date :  Sat Nov 23 11:28:22 2024                 #####
#####       Purpose : Shiny UI for the second tab            #####
#####       Comments :                                       #####
##################################################################


Tab2_UI <- function() {
  
#  ns <- NS('Tab2')
  
  tabPanel("Site Information",  icon = icon("file-lines", tags$style()),
            HTML('<div style="margin-left: 20px; ">'),

           
           
           fluidRow(
             
             column(2,
                    actionButton('wgtGETDATA', 'Get Soil Data'),
                    plotOutput('wgtProfile')
                    
             ),
             column(10,  tabsetPanel(type = "tabs",
                                     ##### Site Description Tab  ####                            
                                     tabPanel("Soil Description", 
                                              htmlOutput('wgtSiteDescription'),
                                              
                                              HTML('<BR><BR>'),
                                              htmlOutput("wgtLabResultsLabel"),
                                              #rHandsontableOutput('labResultsTable' )),
                                              uiOutput('labResultsTable')),
                                             
                                     
                                     # ##### Site Data Sheet View Tab  ####   
                                     # tabPanel("Soil Test Data",
                                     #          htmlOutput("wgtLabResultsLabel"),
                                     #          #rHandsontableOutput('labResultsTable' )),
                                     # uiOutput('labResultsTable')),
 
                                     ##### Lab Data Plots Tab  #### 
                                     tabPanel("Lab Data Plots", fluidRow(uiOutput('plots'))),
                                     
                                     tabPanel('Photos',  uiOutput('wgtPhotoViewer') %>% withSpinner(color="#0dc5c1"))
                                     
             )  
             )
           )
           
           , HTML('</div>') 
           
           
           
           
          #  
          #  htmlOutput('wgtSiteDescription'),
          #  hidden(htmlOutput("wgtLabResultsLabel2")),
          #  rHandsontableOutput('labResultsTable2' ),
          #  
          #  htmlOutput("WgtSiteInfoTab_SoilSiteImage"),
          # # htmlOutput("WgtSiteInfoTab_SoilSiteImages")
          #  
          #   HTML('</div>') 
           
           )}
