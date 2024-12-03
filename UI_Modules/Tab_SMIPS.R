##################################################################
#####       Author : Ross Searle                             #####
#####       Date :  Sat Nov 23 11:28:22 2024                 #####
#####       Purpose : Shiny UI for the second tab            #####
#####       Comments :                                       #####
##################################################################


Tab4_UI <- function() {
  
  ns <- NS('Tab4')
  
  tabPanel("Modelled National SM Map",  icon = icon("map", tags$style()),
           HTML('<div style="margin-left: 20px; ">'),
           
           HTML('<H1>This page could show the current SMIPS Website - just a suggestion. Not convinced we would want to</H>
                <p><img src="Images/SMIPSDemo.png" alt="Dummy soil morphology screen" width="1250" height="650"></p>'),
           HTML('</div>') 
  )}