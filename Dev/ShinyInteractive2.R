if(interactive()){

  library(shiny)
  library(shinyjs)
  
  ui <- fluidPage(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
      #slidePanel {
        position: fixed;
        width: 300px;
        height: 100%;
        top: 0;
        left: -300px;
        background-color: #f8f9fa;
        padding: 20px;
        box-shadow: 2px 0 5px rgba(0,0,0,0.3);
        transition: left 0.4s ease-in-out;
        z-index: 1050;
      }
      #slidePanel.open {
        left: 0;
      }
    "))
    ),
    
    actionButton("toggleBtn", "Settings"),
    
    # Sliding Panel
    div(id = "slidePanel",
        h3("User Settings"),
        br(),
        # Email Input
        textInput("user_email", "Email Address:", placeholder = "name@example.com"),
        
        # Checkbox Input
        checkboxInput("subscribe", "Subscribe to newsletter", value = FALSE),
        br(),
        
        # Dynamic Target Element (wgtBOB)
        selectInput("wgtBOB", "Select Option (wgtBOB):", choices = c("Default 1", "Default 2")),
        br(),
        
        actionButton("save", "Save", class = "btn-primary")
    )
  )
  
  server <- function(input, output, session) {
    # Open panel
    observeEvent(input$toggleBtn, {
      shinyjs::addClass("slidePanel", "open")
    })
    
    # Close panel on save
    observeEvent(input$save, {
      shinyjs::removeClass("slidePanel", "open")
    })
    
    # NEW: Update wgtBOB choices based on the checkbox state
    observeEvent(input$subscribe, {
      if (input$subscribe) {
        # Choices to display when checked
        new_choices <- c("Premium Content A", "Premium Content B", "Monthly Digest")
      } else {
        # Choices to display when unchecked
        new_choices <- c("Default 1", "Default 2")
      }
      
      # Update the input element dynamically
      updateSelectInput(session, "wgtBOB", choices = new_choices)
      
      # Note: If wgtBOB is radioButtons, use: updateRadioButtons()
      # Note: If wgtBOB is checkboxGroupInput, use: updateCheckboxGroupInput()
    })
  }
  
  shinyApp(ui, server)
  
  
}
