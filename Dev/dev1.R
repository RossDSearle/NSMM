Network='NSMM'
SiteName='B Farmers'

RV <- list()
RV$CurrentSite$SiteName='B Farmers'
RV$CurrentSite$Network='NSMM'





soilBucketDataSrc='SLGApedo'

calibs <- BE$Shared$Queries$doSelectQuery(paste0("select * from probeCalibration where Network='", Network, "' and SiteName='", SiteName, "'"))

SensorSummary <<- BE$Shared$Reporting$SensorSummary()
idxs <- which(SensorSummary$Network==RV$CurrentSite$Network & SensorSummary$SiteName==RV$CurrentSite$SiteName
              & SensorSummary$DataType == BE$Shared$Constants$knownFeatures$SoilMoisture )


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
