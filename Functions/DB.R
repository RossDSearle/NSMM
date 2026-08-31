



wd <- getwd()
#setwd('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/Apps/NSMPReporting')

source('Secrets/DBServerConfig.R')
source('Secrets/DBconfig.R')
source('Secrets/DBLogins.R')
source('Functions/dbHelpers.R')
#source('Code/DBNatsoilQueries.R')



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


deleteWholeSite <- function(con, verbose=F, agencyCode='994', projCode='NSMP', siteID=1, obsNo=NULL){
  
  appcon <-dbConnect(RSQLite::SQLite(), "C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/Apps/SoilDataEntry/DBs/SoilDataEntryAppConfigs.sqlite")
  tableLevels <- OS$DB$Helpers$doQuery(appcon, "select * from NatSoil_TableLevels")
  dbDisconnect(appcon)
  
  dfd <- tableLevels[rev(order(tableLevels$Level)), ]
  for (i in 1:nrow(dfd)) {
    rec<-dfd[i,]$Table
    if(verbose){  print(paste0('Deleting data from ', rec)) }
    sql <- paste0("DELETE from ", rec, " WHERE 
                         agency_code = '", agencyCode, "' and ",
                  "proj_code = '", projCode, "' and ",
                  "s_id = '", siteID, "'"
    )
    OS$DB$Helpers$doExec(con, sql)
  }
}
