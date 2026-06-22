library(listviewer)

Network='NSMM'
SiteName='Thoona'

fp <- file.path(tempdir(), paste0('NSMMDataCache'))
unlink(fp, recursive = T)

RV <- list()
RV$CurrentSite$SiteName=SiteName
RV$CurrentSite$Network=Network
RV$Email <- 'ross.searle@gmail.com'

session <- list()
session$token <- '1234567890'

input <- list()
input$wgt_SMmain_Network <- 'NSMM'
input$wgt_SMmain_SiteName <- 'Thoona'
input$datePker <- as.Date('2026-06-11')

soilBucketDataSrc='SLGApedo'

calibs <- BE$Shared$Queries$doSelectQuery(paste0("select * from probeCalibration where Network='", Network, "' and SiteName='", SiteName, "'"))

SensorSummary <<- BE$Shared$Reporting$SensorSummary()
SensorSummary <<- BE$DataStore$Queries$getSensorInfo()
idxs <- which(SensorSummary$network==RV$CurrentSite$Network & SensorSummary$sitename==RV$CurrentSite$SiteName
              & SensorSummary$datatype == BE$Shared$Constants$knownFeatures$SoilMoisture )


mts <- BE$Shared$SensorData$getSensorDataFromDBUsingSensorSummaryRecs(SensorSummary[idxs,])
mtsVol <- BE$Calibrations$getAllLayersVolumeticTS(Network = RV$CurrentSite$Network, SiteName=RV$CurrentSite$SiteName, rawTS=mts, type='Available')
    
ts <- mtsVol[nrow(mtsVol),]  

rawTS <- mts


pbVal = 25
maxProbeValue = 40
minProbeValue = 10
DrainedUpperLimit = 23
LowerLimit = 11

mmperrawprobeval <- soilrange/rawrange

(pbVal-minProbeValue) * mmperrawprobeval + LowerLimit



mperraw <- rawrange/soilrange

pm <- function(x) {
  return((x-minProbeValue)*mperraw + LowerLimit)
}

pm(pbVal)

mmperpv <-  ((calibs$DrainedUpperLimit - calibs$LowerLimit)/100) / pbrange
