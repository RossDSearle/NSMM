##################################################################### #
#####       Author : Ross Searle                                  ###
#####       Date :  Wed Jan 22 18:48:58 2025                      ###
#####       Purpose : Generates an empty Natsoil shell in sqllite ###
#####       Comments :                                            ###
##################################################################### #


library("DBI")
library("odbc") 
library("readr") 

scriptPath <- 'admin/makeSQLite NatSoil Schema.sql'


makeSQLiteNatSoilDB <- function(con, populateData=T, projCode){
      
      print('Reading SQL make DB script...')
        
        DBpath <- tempfile(pattern = 'soilData_', fileext = '.db')
        #DBpath <- 'c:/temp/portable8.db'
        
        
        if(file.exists(DBpath)){
          unlink(DBpath)
        }
        
        
        portablecon <- dbConnect(RSQLite::SQLite(), paste0(DBpath))
     # dbExecute(portablecon, 'PRAGMA foreign_keys = ON;')
      
      lns <- readr::read_lines(scriptPath)
      
      sqlist <- list()
      
      sql<- ''
      
      for (i in 1:length(lns)) {
        l<-lns[i]
        if(l != ''){
          if (stringr::str_starts(l, pattern = '[/***]')){
            sql<-''
            l=''
          }
          
          sql <- paste0(sql, l)
            if(stringr::str_detect(l, ';')){
            sqlist <- c(sqlist, sql)
          }
        }
      }
      
     
      #### Generate the blank tables in the DB  
      print('Generating the blank tables...')
      for (i in 1:length(sqlist)) {
        dbExecute(portablecon, sqlist[[i]])
      }
      
      
      if(populateData){
      
       conInfo <- OS$DB$Config$getCon(OS$DB$Config$DBNames$NatSoilProjects)
       tcon <- conInfo$Connection
        tables <- c('CODES','STATES','AGENCIES','OFFICERS','LAB_METHOD_TYPES','LAB_METHODS','LAB_PROPERTIES')
        for (i in 1:length(tables)) {
      
          t <- tables[i]
          print(paste0('Inserting data for ', t))
          res <- dbSendQuery(tcon, paste0('select * from ',t))
          recs <- dbFetch(res)
          dbAppendTable(portablecon, t, recs)
          dbClearResult(res)
        }
        
        dbDisconnect(tcon)
        
        
        appcon <- dbConnect(RSQLite::SQLite(), paste0('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/Apps/SoilDataEntry/DBs/SoilDataEntryAppConfigs.sqlite')) 
        tableLevels <- OS$DB$Helpers$doQuery(appcon, "select * from NatSoil_TableLevels order by Level")
        dbDisconnect(appcon)
        
         prj <- data.frame(Table='PROJECTS',  Level=0)
         dfd <- rbind(prj, tableLevels)
        
        for (i in 1:nrow(dfd)) {
       #  for (i in 1:34) {
          rec<-dfd[i,]$Table
          print(paste0('Writing data to ', rec))
          
          
          if(rec!='POINTS' & rec!='project.FIELD_CORES' & rec!='ARCHIVE_SAMPLES' & rec!='project.FIELD_SAMPLES'){
          sql <- paste0('select * from ', rec, " WHERE proj_code = '", projCode, "'")

          ddf <- OS$DB$Helpers$doQuery(con, sql)
         
         

       
           ndf <- OS$DB$Helpers$doQuery(con, paste0('select  top 1 * from ', rec))
           pdf <- OS$DB$Helpers$doQuery(portablecon, paste0('select * from ', rec))
           
           idxs <- which(!colnames(ndf) %in% colnames(pdf))
             if(length(idxs)>0){
                ddf <- ddf[, -idxs]
             }
          
           
             if(nrow(ddf)>0){
               if(rec=='LAB_RESULTS'){
                 
                 # for (z in 1:nrow(ddf)) {
                 #   rec <- ddf[z,]
                 #   print(z)
                 #   sql <- paste0("insert into  LAB_RESULTS (agency_code, proj_code, s_id, o_id, h_no, samp_no, labr_no, labm_code, labr_value) values (", 
                 #               "'",  rec$agency_code, "', '", rec$proj_code, "', '",  rec$s_id, "', '", rec$o_id, "', ", rec$h_no, ", ", rec$samp_no, 
                 #                 ", ", rec$labr_no, ", '",  rec$labm_code, "', ",  rec$labr_value, ")"
                 #   ) 
                 #   print(sql)
                 #   
                 #   OS$DB$Helpers$doInsertUsingRawSQL(portablecon, sql)
                 # }
                 DBI::dbAppendTable(portablecon, rec, ddf)
                
               }else if(rec=='OBSERVATIONS'){
                 
                 ddf$o_date_desc <- as.character(ddf$o_date_desc)
                 
                 DBI::dbAppendTable(portablecon, rec, ddf)
               }
               
               else{
                 DBI::dbAppendTable(portablecon, rec, ddf)
               }
                
               }
             
           }
        }
         
        mdf <- data.frame(extractDate=as.character(Sys.time()))
        DBI::dbWriteTable( portablecon, 'meta', mdf)
        
        #appCon <- OS$DB$Config$getCon(OS$DB$Config$DBNames$AppDB)$Connection
        appCon <- dbConnect(RSQLite::SQLite(), paste0('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/Apps/SoilDataEntry/DBs/SoilDataEntryAppConfigs.sqlite')) 
        labLims <- OS$DB$Helpers$doQuery(appCon, 'Select * from LabLimits')
        colnames(labLims) <- c("labm_code", "Count", "MinValue", "LQ_05", "MeanValue", "UQ_95", "MaxValue")
        DBI::dbDisconnect(appCon)
        DBI::dbWriteTable( portablecon, 'labQuantificationLimits', labLims)
        DBI::dbWriteTable( portablecon, 'tableLevels', tableLevels)
        
        dbDisconnect(portablecon)
      }
      
      return(DBpath)
      
      }
  





