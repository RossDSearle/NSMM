Tab_UI_Climate <- function() {
  
  
  tabPanel("Weather/Climate",  icon = icon("cloud-sun-rain", tags$style()),
           HTML('<div style="margin-left: 20px; ">'),
           
           tabPanel("Climate", "",
                    fluidRow(uiOutput('wgtClimateInfoPageHTML'))
           ),
           
           
           HTML('</div>') 
           
           
           
  )}