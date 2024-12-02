

PageHeader_UI <- function() {
  ns <- NS('PageHeaderBar')
  tagList(
    
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
  )
  )
}