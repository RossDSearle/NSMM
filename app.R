library(shiny)
library(bslib)
library(DT)
library(rhandsontable)

source('Styles/headerLayout.R')
source('UI_Modules/Tab1.R')
source('UI_Modules/Tab2.R')
source('UI_Modules/Tab_Admin.R')

source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/TernLandscapes/APIs/SensorFederator2/Backends/ObjectStore.R')

dfn <- BE$Shared$Queries$doSelectQuery("select * from Networks")

glbNetworks <<- dfn$Network

ui <- fluidPage(

  PageHeader_UI(),

  navbarPage("", id = "mainTabset", 

            theme = shinythemes::shinytheme("flatly"),
             
            Tab1_UI(),
            Tab2_UI(),
            Tab_Admin_UI(),
  )
                      
)

server <- function(input, output, session) {
  
####   Instantiate Reactive Valuse  ######  
  RV <- reactiveValues()
  RV$User <- NULL
  RV$ShowReporting <- T
  RV$CurrentSensorSummaryTable <- NULL
  RV$Admin_SensorSummaryTable_SelectedRow <- NULL
  
####  Read URL Parameters  #######  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['usr']])) {
      RV$User <- query[['usr']]
      print(RV$User)
    }
  })

#####  Show/Hide UI Components   #######

  observe({
    req(RV$User)
    if(RV$User=='Admin'){
        RV$ShowReporting <- T
    }
  })
  
  observe({
    if(RV$ShowReporting){
      showTab(inputId = "mainTabset", target = "Admin")
    }else{
      hideTab(inputId = "mainTabset", target = "Admin")
    }
  })
  
  
  observeEvent(input$wgt_Admin_SensorSummaryTable_ShowButton, {
    # RV$CurrentSensorSummaryTable <- BE$Shared$Reporting$SensorSummaryFromLiveData()
    
    r <- list()
    if(input$wgt_Admin_SensorSummaryTable_FilterDatatype != 'None'){
      r$DataType = input$wgt_Admin_SensorSummaryTable_FilterDatatype
    }
    if(input$wgt_Admin_SensorSummaryTable_FilterNetwork != 'None'){
      r$Network = input$wgt_Admin_SensorSummaryTable_FilterNetwork
    }
    if(input$wgt_Admin_SensorSummaryTable_FilterIsActive!= 'None'){
      r$IsActive = input$wgt_Admin_SensorSummaryTable_FilterIsActive
    }

    filt <- NULL
    if(length(r) > 0){
     filt <- paste0( names(r)[1], " = '", r[[1]], "' " )
    }
    if(length(r) > 1){
      for (z in 2:length(r)) {
        filt <- paste0(filt, ' and ', names(r)[z], " = '", r[[z]], "' " )
      }
     
    }
    
    RV$CurrentSensorSummaryTable <- BE$Shared$Reporting$SensorSummary(filter = filt)
   
  })
  
  
  output$wgt_Admin_SensorSummaryTable = renderRHandsontable({
    req( RV$CurrentSensorSummaryTable)
    df <- RV$CurrentSensorSummaryTable
    
    row_highlight <- which(df$IsActive=='FALSE')
         rhandsontable( df, selectCallback = TRUE,  manualColumnResize = T, readOnly = TRUE, rowHeaders = F,
                        
                        afterDeselect = htmlwidgets::JS(
                          "function() {Shiny.onInputChange('wgtDisplayTimeTable_select', null);
                   }"
                        )) %>%
           hot_cols(columnSorting = TRUE)
  })
  
  observe({
    req( input$wgt_Admin_SensorSummaryTable_select$select$r)
    sid <- input$wgt_Admin_SensorSummaryTable_select$select$r
   print(sid)
   rec <- RV$CurrentSensorSummaryTable[sid, ]
   print(rec)
   RV$Admin_SensorSummaryTable_SelectedRow <- rec
  })
  
  
  observeEvent(input$wgt_Admin_SensorSummaryTable_ShowTimeseries, {
    
    rec <- RV$Admin_SensorSummaryTable_SelectedRow
    
    if(is.null(rec)){
      showModal(modalDialog(
        title = "Timeseries Viewer",
        "Please select sensor record in the table to view"
      ))
    }
    
    showModal(modalDialog(
      tags$h2(rec$SensorName),
      plotOutput(outputId = 'wgt_Admin_SensorDetails_SensorTimeseries'),
      footer=tagList(
        modalButton('Close')
      )
    ))
    
    
    
  })
  
  
  output$wgt_Admin_SensorDetails_SensorTimeseries <- renderPlot({
    
    df <- data.frame(a=c(1,2,3,4), b=c(5,6,7,8))
    plot(df)
    
  })
  
}

shinyApp(ui = ui, server = server)
