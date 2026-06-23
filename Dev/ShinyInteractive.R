if(interactive()){
  
  library(shiny)
  library(dygraphs)
  library(xts)
  library(lubridate)
  
  # Generate sample time-series data
  set.seed(123)
  dates <- seq(as.Date("2026-01-01"), as.Date("2026-01-30"), by = "day")
  data_ts <- xts(rnorm(length(dates), mean = 50, sd = 10), order.by = dates)
  
  ui <- fluidPage(
    titlePanel("Select Date by Clicking Dygraph"),
    
    fluidRow(
      column(12,
             # The output ID used here matches the JavaScript callback string below
             dygraphOutput("my_chart"),
             br(),
             wellPanel(
               h4("Selection Result:"),
               textOutput("selected_date_text")
             )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    # Render the interactive dygraph
    output$my_chart <- renderDygraph({
      dygraph(data_ts, main = "Click Anywhere on the Chart") %>%
        dyOptions(drawPoints = TRUE, pointSize = 4) %>%
        # Use JavaScript callback to send clicked date back to Shiny
        dyCallbacks(
          clickCallback = "function(e, x, points) {
          Shiny.setInputValue('chart_clicked_time', x);
        }"
        )
    })
    
    # Process the clicked time sent from Javascript
    reactive_date <- reactive({
      # Ensure an input exists before running conversions
      req(input$chart_clicked_time)
      
      # JavaScript sends milliseconds since Epoch; convert to POSIXct
      seconds_epoch <- input$chart_clicked_time / 1000
      posix_time <- as.POSIXct(seconds_epoch, origin = "1970-01-01", tz = "UTC")
      
      # Format cleanly as a Date string
      format(posix_time, "%Y-%m-%d")
    })
    
    # Display the text output
    output$selected_date_text <- renderText({
      if (is.null(input$chart_clicked_time)) {
        "No date selected yet. Click on the chart canvas above."
      } else {
        paste("You clicked on date:", reactive_date())
      }
    })
  }
  
  shinyApp(ui = ui, server = server)
  
}
