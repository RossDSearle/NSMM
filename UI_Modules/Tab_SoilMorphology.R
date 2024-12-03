##################################################################
#####       Author : Ross Searle                             #####
#####       Date :  Sat Nov 23 11:28:22 2024                 #####
#####       Purpose : Shiny UI for the second tab            #####
#####       Comments :                                       #####
##################################################################


Tab2_UI <- function() {
  
  ns <- NS('Tab2')
  
  tabPanel("Soil Description",  icon = icon("file-lines", tags$style()),
           HTML('<div style="margin-left: 20px; ">'),
           
           
           HTML('<H1>This page will show the soil description for the site and it would look something like below. </H>
                <p><img src="Images/SoilMorhology.png" width="750" height="550"></p>'),
 
           
           HTML('</div>') 
           
           
           
  )}