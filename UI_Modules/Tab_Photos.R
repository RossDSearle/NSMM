Tab_UI_Photos <- function() {
  
  tabPanel("Photos",  icon = icon("camera"),uiOutput('wgtPhotoViewer') %>% withSpinner(color="#0dc5c1") )

  }
