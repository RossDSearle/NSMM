library(openxlsx)
library(DBI)
library(stringr)

source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/NSMM/Functions/DB.R')
source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/NSMM/Admin/DBMakeSQLiteNatSoilDB.R')

labName <- 'C:/Projects/NSMM/LabData/Import_1/CSBP_Lab_260521_Brendan Malone_001_batch_1.xlsx'
getSheetNames(labName)
#lab <- openxlsx::read.xlsx('C:/Projects/NSMM/LabData/Import_1/CSBP_Lab_260521_Brendan Malone_001_batch_1.xlsx', 'Results')
lab <- openxlsx::read.xlsx('C:/Projects/NSMM/LabData/Import_2/CSBP_Lab_260810_Brendan Malone_001_batch_2.xlsx', 'Results')
lab <- lab[-1,]

meths <- read.csv('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/NSMM/Admin/CSBPMethodMaps.csv')



sitenames <- 'C:/Projects/NSMM/LabData/Import_1/TERN_SMPN_Deidentified_Sample_Labels_updated.xlsx'
getSheetNames(sitenames)
names <- openxlsx::read.xlsx(sitenames, 'Site_Lookup_RESTRICTED')
names <- na.omit(names)
depths <- openxlsx::read.xlsx(sitenames, 'Depth_Lookup_RESTRICTED')
bits <- str_split(depths$Depth_Range, '-')
depths$UpperDepth <- as.numeric(sapply(bits, function (x) x[1]))
depths$LowerDepth <- as.numeric(str_remove(sapply(bits, function (x) x[2]), 'cm'))

con <- getNatSoilProjectsCon()

sitesDF <- OS$DB$Helpers$doQuery(con, "Select * from Sites where agency_code='994' and proj_code='TISM'")
sort(sitesDF$s_id)

bits <- str_split(lab$Sample.Name.2, '-')

sitesInLab <- na.omit(unique(sapply(bits, function (x) x[1])))


####  Ingest site lab data  ####

    for (i in 1:length(sitesInLab)) {
      
      site <- sitesInLab[i]
      labNameRec <- names[names$Site_Code==site,]
      
      if(nrow(labNameRec) > 0){
          DBSiteName <- labNameRec$SoilDB_Name
          
          if(!is.na(DBSiteName)){
              print(paste0(i, ' : ', DBSiteName))
              idxs <- which(str_starts(lab$Sample.Name.2, labNameRec$Site_Code))
              sitedata <- lab[idxs,]
              
         
              for (j in 1:nrow(sitedata)) {
                
                datarec <- sitedata[j,]
                dr <- str_split(datarec$Sample.Name.2, '-')[[1]][2]
                ud <- depths[depths$Depth_Code==dr,]$UpperDepth/100
                ld <- depths[depths$Depth_Code==dr,]$LowerDepth/100
                
                natSiteHors <- OS$DB$Helpers$doQuery(con, paste0( "Select * from HORIZONS where agency_code='994' and proj_code='TISM' and s_id = '", DBSiteName, "' order by h_upper_depth"))
                hno = NULL
        
                for (v in 1:nrow(natSiteHors)) {
                  rec <- natSiteHors[v,]
                  if(ud >= rec$h_upper_depth & ud < rec$h_lower_depth ){
                    hno=rec$h_no
                    break
                  }
                }
                
                sqlsamps <- OS$DB$Helpers$doQuery(con, paste0( "Select * from SAMPLES where agency_code='994' and proj_code='TISM' and s_id = '", DBSiteName, "' and h_no = ", hno))
                if(nrow(sqlsamps)>0){
                  sampNo = max(sqlsamps$samp_no) + 1
                }else{
                  sampNo = 1
                }
                sql <- paste0("insert into SAMPLES (agency_code, proj_code, s_id, o_id, h_no, samp_no, samp_upper_depth, samp_lower_depth)   
                                               VALUES('994', 'TISM', '", DBSiteName, "', 1, ", hno, ", ", sampNo, ", ", ud, ", ", ld, " )")
                OS$DB$Helpers$doInsert(con, sql)
                  
                for (k in 7:length(colnames(sitedata))) {
                    att <- colnames(sitedata)[k]
                    methrec <- meths[meths$CSBP_R==att, ]
                   
                   
                      if(!is.na(methrec$LabCode) & methrec$LabCode!=''){
                        labcode <- methrec$LabCode
                        labval <- datarec[, k]
                        if(str_detect( labval, '<')){
                          v1 <- str_trim(str_remove(labval, '<'))
                          labval <- as.numeric(v1)/2
                        }else{
                          labval <- as.numeric(labval)
                        }
                        
                        
                        sql <- paste0("insert into LAB_RESULTS (agency_code, proj_code, s_id, o_id, h_no, samp_no, labr_no, labm_code, labr_value)   
                                               VALUES('994', 'TISM', '", DBSiteName, "', 1, ", hno, ", ", sampNo, ", 1,'", labcode, "', ", labval , " )")
                        OS$DB$Helpers$doInsert(con, sql)
                       }
                }
                sampNo=sampNo+1
              }
          }else{
      print(paste0('There is no site in the Morph DB for ', appNameRec$Site_Name))
          }
      }else{
        print(paste0('We are not sure what the DB Sitename is'))
      }
  }
}




deleteDevData <- function(){
  
 d <- c("CSIRO_FHRS_BackPaddock", "Angaston","Yamba","Cadell", "Nyah", "Thoona", "Taylorville")
 
 for (n in 1:length(d)) {
   s <- d[n]
   sql <- paste0("DELETE FROM [NatSoilProjects].[dbo].LAB_RESULTS   where proj_code='TISM' and s_id = '", s, "';")
   OS$DB$Helpers$doExec(con, sql)
 }
}


###### create the portable DB

outDB <- 'C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/NSMM/DB/TISM.db'
if(file.exists(outDB)){
  tm <- round(as.numeric(Sys.time()))
  file.rename(from=outDB, paste0(outDB, '_', tm))
}
con <- getNatSoilProjectsCon()

DBpath <- makeSQLiteNatSoilDB(con=con, populateData = T, projCode =  'TISM')
if(file.exists(DBpath)){
  file.copy(DBpath, outDB)
  unlink(DBpath)
}
dbDisconnect(con)

#deleteWholeSite(con, projCode = 'TISM', siteID = 'Taylorville')

# sql <- paste0("DELETE FROM [NatSoilProjects].[dbo].LAB_RESULTS   where proj_code='TISM';")
# OS$DB$Helpers$doExec(con, sql)
# 
# sql <- paste0("DELETE FROM [NatSoilProjects].[dbo].[SAMPLES]   where proj_code='TISM';")
# OS$DB$Helpers$doExec(con, sql)



OS$DB$Helpers$doQuery(con, paste0( "Select * from SAMPLES where agency_code='994' and proj_code='TISM' and s_id = '", DBSiteName, "' and h_no = ", 1))
