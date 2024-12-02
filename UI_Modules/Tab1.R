
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
             
             HTML('HEgkg iuc igckdglkvgK WDG LUK'),
             selectInput('id', 'test', choices = NULL),
             actionButton('id2', 'test'),
             HTML('</div>') 
)}