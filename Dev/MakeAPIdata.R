

glbNetworks <- queryAPI(EP = 'Networks')
glbSites <- queryAPI(EP = 'Sites')
glbHarvestDates <- queryAPI(EP = 'RunSQLQuery?SQL=select distinct dt from HarvestLog order by dt DESC')
SensorSummary <-  queryAPI(EP = 'Sensors?verbose=true')
csm <-  queryAPI(EP = 'LatestSurfaceSoilMoisture?numDays=2&SoilMoistureReadingType=RawSensor')
currentSoilMoisture <- as.numeric(csm$soilmoisture)
glbCurrentSurfaceSoilMoisture <- currentSoilMoisture
glbSensorDataTypes <- queryAPI(EP = 'SensorDataTypes')

save(glbNetworks, glbSites, glbHarvestDates, SensorSummary, glbCurrentSurfaceSoilMoisture, glbSensorDataTypes,  file = "c:/projects/NSMM/api.RData")
