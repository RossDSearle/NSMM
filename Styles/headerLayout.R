

PageHeader_UI <- function() {
  ns <- NS('PageHeaderBar')
  
 
  tagList(
    
    tags$head(
      tags$style(HTML("
      .leaflet-left .leaflet-control-zoom {
        visibility: hidden;
        height: 0px;
        vertical-align: top
      }
    ")),
      
    #  tags$style(".leaflet-control-zoom { display: none; }"),
      
      tags$style(HTML("
          .leaflet .legend {
            line-height: 10px; /* Adjust line height for label spacing */
            font-size: 10px;   /* Adjust font size of legend labels */
            padding: 1px;     /* Adjust padding around the legend content */
          }
          .leaflet .legend i {
            width: 8px;       /* Adjust width of color boxes/symbols */
            height: 8px;      /* Adjust height of color boxes/symbols */
          }
        "))
   
    
    ),
    
    tags$style(HTML('.container-fluid { margin-right: auto; margin-left: 0px; padding-left: 0px; padding-right: 0px }') ),
    
    tags$link(rel = "icon", href = "favicon.ico"),
    tags$title("NSMM"),
    HTML('<div style="background-color:#00303b;color:white;padding:20px; font-size: 35px;">
           
           <a href="https://www.csiro.au/" class="navbar-left" target=_blank>&nbsp;
         <img src="Logos/CSIRO-logo-inverted.svg" width="40" height="40"></a>
           <div style="min-width:20px;  float:left;"><span>&nbsp;</span></div>
           <a href="https://www.tern.org.au/" class="navbar-left" target=_blank>
           <img src="./Logos/tern1-inverted.png" height="45"></img></a>
           <span style="font-weight:bolder;">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
         National Soil Moisture Monitoring Network</span>
           
           </div>'
  ),
  ###  Adjust the width of the modal dialog
  tags$style(HTML(".modal-dialog{ width:1200px}"))
  )
}