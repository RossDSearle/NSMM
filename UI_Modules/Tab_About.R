##################################################################
#####       Author : Ross Searle                             #####
#####       Date :  Sat Nov 23 11:28:22 2024                 #####
#####       Purpose : Shiny UI for the second tab            #####
#####       Comments :                                       #####
##################################################################


includeHTML2 <- function (path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  return(HTML(lines))
}



Tab3_UI <- function() {
  
  ns <- NS('Tab3')
  
  tabPanel("About",  icon = icon("circle-info", tags$style()),
           HTML('<div style="margin-left: 20px; ">'),
           
           tabPanel("About", "",
                    includeHTML2(paste0( "www/StaticPages/About.html"))
           ),
 
           
           HTML('</div>') 
           
           
           
  )}
