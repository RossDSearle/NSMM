
Admin_NetworksTable <- function(){

  df <- BE$Shared$Queries$SensorInfo()
  lastPing <- Sys.time() - df$MaxDate
  df$Online <- lastPing<=5 
  
  
  # colList <- list(
  #   .selection = colDef(headerStyle = list(pointerEvents = "none")),
  #   Network = colDef(minWidth=200),
  #   BackEnd = colDef(minWidth=100),
  #   Server = colDef(minWidth=300),
  #   Organisation = colDef(minWidth=150),
  #   Usr = colDef(minWidth=150),
  #   Pwd = colDef(minWidth=300),
  #   Access = colDef(minWidth=80),
  #   Contact = colDef(minWidth=150),
  #   Email = colDef(minWidth=300),
  #   OrgURL = colDef(minWidth=900),
  #   Comments = colDef(minWidth=10000)
  #   
  # )
  
  
ot <-  reactable(
  df,
  compact = TRUE, defaultPageSize = 20,showPagination = T, showPageInfo = T, sortable = T,  filterable = TRUE, resizable = TRUE, 
  selection = "single", onClick = "select",
  
  striped = TRUE,
  highlight = TRUE,
  bordered = TRUE,
  style = list(maxWidth = 10000),
#  selection = seltype,
#  columns = colList,
  theme = reactableTheme(
    headerStyle = list("& input[type='checkbox']" = list(display = "none")),
    borderColor = "#dfe2e5",
    stripedColor = "#f6f8fa",
    highlightColor = "#aed6f1",
    cellPadding = "0px 0px",
    style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif")
  )
)

return(ot)
}