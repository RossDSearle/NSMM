

Admin_HarvestLogTable <- function(dt){
  
  df <- BE$Shared$Queries$HarvestLogTable(dt)
  ot <-  reactable(df)
  
}


Ticks <- function(online) {

    if(online){
    ico <- tagAppendAttributes(shiny::icon("check", "fa-xl"),
                        style = paste("color:green"),
                        "aria-hidden" = "true"
    )
    }else{
      ico <- tagAppendAttributes(shiny::icon("xmark", "fa-xl"),
                                 style = paste("color:red"),
                                 "aria-hidden" = "true"
      )  
    }
    return(ico)
  }



rating_stars <- function(rating, max_rating = 5) {
  star_icon <- function(empty = FALSE) {
    tagAppendAttributes(shiny::icon("star"),
                        style = paste("color:", if (empty) "#edf0f2" else "orange"),
                        "aria-hidden" = "true"
    )
  }
  rounded_rating <- floor(rating*5 + 0.5)  # always round up
  stars <- lapply(seq_len(max_rating), function(i) {
    if (i <= rounded_rating) star_icon() else star_icon(empty = TRUE)
  })
  label <- sprintf("%s out of %s stars", rating*5, max_rating)
  div(title = label, role = "img", stars)
}




Admin_SensorsTable <- function(df){

   req( df)

  lastPing <- (Sys.time() - as.POSIXct.Date( df$MaxDate))/(60*24)
  df$Online <- lastPing<=5 
  df$Stars <- df$QualityIndex
  sumDf <- df[, c(39, 40, 1,2,3,4,5,6,7,8,9, 10, 13, 14)]
  subDf <- df[, c(3, 11:37)]
  
 
  colDef <- function(..., wrap = FALSE, style = NULL, headerStyle = NULL) {
    if (wrap) {
      style <- c(style, list(whiteSpace = "nowrap", textOverflow = "ellipsis"))
      headerStyle <- c(headerStyle, list(whiteSpace = "nowrap", textOverflow = "ellipsis"))
    }
    reactable::colDef(..., style = style, headerStyle = headerStyle)
  }
  
  
   colList <- list(
#     .selection = colDef(headerStyle = list(pointerEvents = "none")),
     
     # Online = colDef(width=100, cell = function(value) {
     #   if (!value) "\u274c Error" else paste0("\u2714\ufe0f")
     # }),
     Online = colDef(width=60, cell = function(value) {
       Ticks(value)
     }),
     
     Stars = colDef(width=120, cell = function(value) rating_stars(value)),
     Network = colDef(minWidth=150),
     SiteName = colDef(minWidth=150),
     SensorName = colDef(minWidth=300, wrap = T),
     Count = colDef(maxWidth=60),
     DataType = colDef(minWidth=200),
     
     MinDate = colDef(),
     MaxDate = colDef(),
     MinVal =  colDef(format=colFormat(digits = 2)),
     MaxVal =  colDef(format=colFormat(digits = 2))
   )
   
   
   colListSub <- list(
     
     UpperDepth = colDef(minWidth=120, name='UpperDepth'),
     LowerDepth = colDef(minWidth=120, name='LowerDepth'),
     Units = colDef(minWidth=200),
     HarvestDate = colDef(minWidth=130, name='HarvestDate'),
     QualityIndex = colDef(minWidth=130, name='QualityIndex', format=colFormat(digits = 2)),
     
     LowestIndexValue = colDef(minWidth=80, name='LI_Value', format=colFormat(digits = 2)),
     LowestIndexName = colDef(minWidth=200, name='LowestIndexName'),
     IinRange = colDef(minWidth=100, name='inRange'),
     IvalidProp = colDef(minWidth=100, name='validProp', format=colFormat(digits = 2)),
     InumGaps = colDef(minWidth=100, name='numGaps', format=colFormat(digits = 2)),
     ItotalGapsDays = colDef(minWidth=150, name='gapsDaysProp', format=colFormat(digits = 2)),
     IdesiredNumDays = colDef(minWidth=150, name='desiredNumDays', format=colFormat(digits = 2)),
     TotalNumDays = colDef(minWidth=90, name='NumDays'),
     TotalValidDays = colDef(minWidth=90, name='validDays'),
     ValidProportion = colDef(minWidth=120, name='validDaysProp', format=colFormat(digits = 2)),
     NumGaps = colDef(minWidth=100, name='NumGaps'),
     NumGapDays = colDef(minWidth=120, name='NumInRange'),
    TotalValidDays = colDef(minWidth=10, name='validDays'),
    # NumDays = colDef(minWidth=100, name='NumDays'),
     NumInRange = colDef(minWidth=130, name='validProp'),

   MinimumValue = colDef(minWidth=100, name='MinVal', format=colFormat(digits = 2)),
   MaximumValue = colDef(minWidth=100, name='MaxVal', format=colFormat(digits = 2)),
   MeanValue = colDef(minWidth=100, name='MeanVal', format=colFormat(digits = 2)),
   StandardDeviation = colDef(minWidth=100, name='StdDev', format=colFormat(digits = 2))

   )
   
   
  
  
ot <-  reactable(
  sumDf,
  compact = TRUE, defaultPageSize = 20,showPagination = T, showPageInfo = T, sortable = T,  filterable = TRUE, resizable = TRUE, 
#  selection = "multiple",  
  showPageSizeOptions = TRUE,
  pageSizeOptions = c(10, 20, 50, 100, 100000),
  showSortIcon = TRUE,
  defaultColDef = colDef(headerClass = "sort-header"),
#  onClick = "select",
  striped = TRUE,
  highlight = TRUE,
  bordered = TRUE,
  style = list(maxWidth = 10000),
  columns = colList,
  theme = reactableTheme(
    headerStyle = list("& input[type='checkbox']" = list(display = "none")),
    borderColor = "#dfe2e5",
    stripedColor = "#f6f8fa",
    highlightColor = "#aed6f1",
    cellPadding = "0px 0px",
    style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif")
  ),


details = function(index) {
  details <- filter(subDf, SensorNum  == df$SensorNum[index]) %>% select(-SensorNum )
  tbl <- reactable(details, outlined = TRUE, highlight = TRUE, fullWidth = FALSE,
                   columns = colListSub,
                   )
  htmltools::div(style = list(margin = "12px 45px"), tbl)
},
onClick = "expand",
rowStyle = list(cursor = "pointer")



)

ot
}

