Tab_UI_Downloads <- function() {
  
  
  tabPanel("Data Downloads",  icon = icon("download"),
           HTML('<div style="margin-left: 20px; ">'),
           
           HTML('<H1>Data Downloads</H1><BR><BR>'),
           
           HTML('<P>Click on the links below to download the various data sets for this site.</P>'),
           
           HTML('<P>These data sets are also available via an API if you would like to access them programatically - 
                <a href="https://soils.ternlandscapes.net.au/TERNLandscapes/SoilMoistureProbesAPI/__docs__/#" target="_blank"> HERE </a>.</P>'),
           
           HTML('<P>The full suite of climate data is available via the 
                <a href="https://www.longpaddock.qld.gov.au/silo/"  target="_blank">QLD Government SILO website.</a></P>'),
           
           HTML('<BR><BR>'),
           
           HTML('<P>Click here to download Volumetric Soil Moisture estimates - '), downloadLink("wgtDownloadData_VolSM", "Download"), HTML('<P>'),
           HTML('<P>Click here to download Raw SM Probe data - '), downloadLink("wgtDownloadData_RawProbe", "Download"), HTML('<P>'),
           HTML('<P>Click here to download Rainfall data - '), downloadLink("wgtDownloadData_Rainfall", "Download"), HTML('<P>'),
           HTML('<P>Click here to download Soil Test data - '), downloadLink("wgtDownloadData_SoilTestData", "Download"), HTML('<P>'),
           
           HTML('</div>') 
           
           
           
  )}