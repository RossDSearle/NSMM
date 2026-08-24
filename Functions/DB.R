



wd <- getwd()
setwd('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/Apps/NSMPReporting')

source('Secrets/DBServerConfig.R')
source('Secrets/DBconfig.R')
source('Secrets/DBLogins.R')
source('Code/dbHelpers.R')
source('Code/DBNatsoilQueries.R')



OS <<- list()
OS$DB$Config <- get_DBConfig()
OS$DB$Helpers <- get_DBHelpers()
OS$DB$Logins <- get_DBLogins()
OS$DB$NatSoilQueries <- get_NatSoilQueries()


setwd(wd)


getNatSoilProjectsCon <- function(){
  
  sqlServerDrv <- 'ODBC Driver 17 for SQL Server'
  con <- dbConnect(odbc(),
                   Driver = sqlServerDrv,
                   Server = "sql-asris-stage.it.csiro.au",
                   Database = "NatSoilProjects",
                   port = 1433,
                   UID      =  'projects_readwrite',
                   PWD      = 'letmeinplease',
  )
  return(con)
}
