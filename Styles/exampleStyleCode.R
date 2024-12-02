tagList(
  # CSS for navbar elements
  tags$style(
    
    HTML('.container-fluid {
    margin-right: auto;
    margin-left: 0px;
    padding-left: 0px;
    padding-right: 0px
}')
    #  
    #     '.navbar {font-size: 17px; background-color:#00303b;}',
    #        '.navbar-default .navbar-brand {color: red; font-weight: bold}',
    #        '.navbar-default .navbar-brand:hover {color: #262626;}',
    #        '.navbar-default .navbar-nav > li > a {color: #262626;}')
    # )
    # CSS for fonts
    # tags$style('h3 {font-weight: normal;}',
    #            'h4 {font-weight: normal;}',
    #            '* {font-family: Ubuntu;}'),
    # CSS for all buttons (class .btn)
    # tags$style('.btn {color: #333333; 
    #                   background-color: #eeeeee; 
    #                   border-color: #cccccc;}
    #             .btn:hover {background-color: #e1e1e1;}'), 
    # CSS for errors and validation messages
    # tags$style('.shiny-output-error-validation {
    #              color: #e35300;
    #              font-weight: bold;}'),
    # CSS for individual elements: note the '#id' syntax
    # tags$style('#map-readme {color: #333333;
    #                          background-color: #ff770050;
    #                          border-color: #c84407;}
    #             #map-readme:hover {background-color: #e36a0075;}
    #             #trend-readme {color: #333333;
    #                            background-color: #ff770050;
    #                            border-color: #c84407;}
    #             #trend-readme:hover {background-color: #e36a0075;}')
  ), # end of style block
),