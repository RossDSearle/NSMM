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
           
<<<<<<< HEAD
           HTML('<H1>This page will show the soil description for the site and it would look something like below</H>
                <p><img src="./Images/SoilMorhology.png" alt="Dummy soil morphology screen" width="750" height="550"></p>'),
=======
           
           HTML('<H1>This page will show the soil description for the site and it would look something like below. </H>
                <p><img src="Images/SoilMorhology.png" width="750" height="550"></p>'),
 
           
>>>>>>> 9d418fd647b35f1e09731a9e0f2efd6b49f5a12b
           HTML('</div>') 
  )}