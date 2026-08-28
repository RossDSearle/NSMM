Tab_UI_Photos <- function() {
  
  tabPanel("Photos",  icon = icon("circle-info"),uiOutput('wgtPhotoViewer') %>% withSpinner(color="#0dc5c1") )

  }
