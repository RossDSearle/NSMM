Network='NSMM'
SiteName='B Farmers'

RV <- list()
RV$CurrentSite$SiteName='B Farmers'
RV$CurrentSite$Network='NSMM'


rawTS <- mts


soilBucketDataSrc='SLGApedo'

calibs <- BE$Shared$Queries$doSelectQuery(paste0("select * from probeCalibration where Network='", Network, "' and SiteName='", SiteName, "'"))


idxs <- which(SensorSummary$Network==RV$CurrentSite$Network & SensorSummary$SiteName==RV$CurrentSite$SiteName
              & SensorSummary$DataType == BE$Shared$Constants$knownFeatures$SoilMoisture )


mts <- BE$Shared$SensorData$getSensorDataFromDBUsingSensorSummaryRecs(SensorSummary[idxs,])
mtsVol <- BE$Calibrations$getAllLayersVolumeticTS(Network = RV$CurrentSite$Network, SiteName=RV$CurrentSite$SiteName, rawTS=mts, type='Available')
    