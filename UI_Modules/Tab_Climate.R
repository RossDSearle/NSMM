Tab_UI_Climate <- function() {
  
  
  tabPanel("Climate",  icon = icon("circle-info", tags$style()),
           HTML('<div style="margin-left: 20px; ">'),
           
           tabPanel("Climate", "",
                    fluidRow(uiOutput('wgtClimateInfoPageHTML'))
           ),
           
           
           HTML('</div>') 
           
           
           
  )}