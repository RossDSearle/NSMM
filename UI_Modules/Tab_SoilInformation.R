##################################################################
#####       Author : Ross Searle                             #####
#####       Date :  Sat Nov 23 11:28:22 2024                 #####
#####       Purpose : Shiny UI for the second tab            #####
#####       Comments :                                       #####
##################################################################

Tab_UI_SoilInformation <- function() {
  
  tabPanel("Soil Information",  icon = icon("file-lines"),
            HTML('<div style="margin-left: 20px; ">'),

           fluidRow(
             column(2,
                   # actionButton('wgtGETDATA', 'Get Soil Data'),
                    plotOutput('wgtProfile')
             ),
             column(10,  tabsetPanel(type = "tabs",
                     ##### Site Description Tab  ####                            
                     tabPanel("Soil Description", htmlOutput('wgtSiteDescription'),
                     HTML('<BR><BR>'),
                     htmlOutput("wgtLabResultsLabel"),
                     uiOutput('labResultsTable')),
 
                     ##### Lab Data Plots Tab  #### 
                     tabPanel("Lab Data Plots", fluidRow(uiOutput('plots'))),
             )  
             )
           )
           , HTML('</div>') 
)}
