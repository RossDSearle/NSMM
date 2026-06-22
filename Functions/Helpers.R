

queryAPI <- function(EP){
  qry <- paste0(APIUrl, str_replace_all(EP, ' ', '_'))
  print(qry)
  resp <- httr::GET(qry)
  if(resp$status_code==200){
    jsn <- content(resp, type ='text', encoding = 'UTF-8')
    df <- fromJSON(jsn)
    return(df)
  }else{
    stop(paste0('There was a problem with the API query - ', qry))
  }
}


getTimeSeries <- function(df, filter=NULL){
  
  colIdxs <- which(str_starts(colnames(df), filter))
  
  if(length(colIdxs)>0){
    nms <- colnames(df)
    idxDate <- which(colnames(df) == 'Date')
     tdf <- df[, c(idxDate, colIdxs)]
      ts <- xts::xts(tdf[, -1], order.by = tdf$Date)
      names(ts) <- nms[colIdxs]
      return(ts)

  }else{
    return(NULL)
  }
}

getAllSiteData <- function(user_temp_dir, Network=NULL, SiteName=NULL,  Email=NULL, makeTS=T){
  
  ol <- list()
  ol$QueryDate <- Sys.Date()
  ol$Network <- Network
  ol$SiteName <- SiteName 
  
  silo <- getData(user = user_temp_dir, Network = Network, SiteName = SiteName, what='silo', Email = Email)
  ol$BoM_Station_Number <- silo$Station_Number
  ol$BoM_Station_Name <- silo$Station_Name
  ol$Station_Distance <- silo$Distance
  
  ol$DF$SiteSensorData <- getData(user = user_temp_dir, Network = Network, SiteName = SiteName, what='volumetric')
  ol$DF$Silo <- silo$Data
  
  if(makeTS){
    ol$TS$volumetric_mm <- getTimeSeries(df = ol$DF$SiteSensorData, filter = 'Avail-mm-Soil-Moisture')
    ol$TS$rawProbe <- getTimeSeries(ol$DF$SiteSensorData, filter = 'Soil-Moisture')
    
    ol$TS$Rainfall <- getTimeSeries(df=ol$DF$Silo, filter='daily_rain')
  }
  
  return(ol)
}

getGlobalData <- function(){
  
  ol <- list()
  ol$glbNetworks <- queryAPI(EP = 'Networks')
  ol$glbSites <- queryAPI(EP = 'Sites')
  ol$glbHarvestDates <- queryAPI(EP = 'RunSQLQuery?SQL=select distinct dt from HarvestLog order by dt DESC')
  ol$SensorSummary <-  queryAPI(EP = 'Sensors?verbose=true')
  csm <-  queryAPI(EP = 'LatestSurfaceSoilMoisture?numDays=2&SoilMoistureReadingType=RawSensor')
  csm$soilmoisture <- as.numeric(csm$soilmoisture)
  ol$glbCurrentSurfaceSoilMoisture <- csm
  ol$glbSensorDataTypes <- queryAPI(EP = 'SensorDataTypes')
  
  return(ol)
  
}

getData <- function(user=NULL, Network='', SiteName='', what='volumetric', Email=NULL){
  # what options are silo, volumetric, globals
  
  if(is.null(user)){
    datacache <- paste0(tempdir(), '/NSMMDataCache')
    CacheDir <- paste0(datacache, '/', what)
    if(!dir.exists(CacheDir)){dir.create(CacheDir, recursive = T)}
    cachefile <- paste0(CacheDir, '/',  Sys.Date(), '_', what, '_Data.rds')
    
    fls <- list.files(CacheDir, full.names = T,recursive = F)
        if(length(fls)>0){
          idxs <- which(!fls %in% cachefile)
          if(length(idxs)>0){
            unlink(fls)
          }
        }
      
    
  }else{
    datacache <- user_temp_dir
    CacheDir <- paste0(datacache, '/', what)
    if(!dir.exists(CacheDir)){dir.create(CacheDir, recursive = T)}
    cachefile <- paste0(CacheDir, '/', Network, '_', SiteName, '_', Sys.Date(), '_', what, '_Data.rds')
  }
  
  
  
  
  
  if(file.exists(cachefile)){
    data <- readRDS(cachefile)
  }else{
    
    if(what=='silo'){
      url <-paste0('SiloWeatherData?network=', Network,'&sitename=',SiteName, '&email=', Email, '&variables=RXNEJHM')
      data <- queryAPI(EP = url )
      data$Data$date <- as.Date(data$Data$date )
      colnames(data$Data)[1] <- 'Date'
    }else if(what=='volumetric'){
      url <-paste0('VolumetricSoilWater?network=', Network,'&sitename=',SiteName ,'&verbose=true')
      data <- queryAPI(EP = url )
      data$Date <- as.Date(data$Date )
    }else if(what=='globals'){
      data <- getGlobalData()
  }
      
    
    saveRDS(data, cachefile)
  }
  
  return(data)
}

splineVals <- function(indf, atts){
  
  ol <- vector("list", length = length(atts))
  
  for (n in 1:length(atts)) {
    att=atts[n]
    sdf <- data.frame(indf[,c(1:3)], as.numeric(indf[att][[1]]),stringsAsFactors = FALSE)
    colnames(sdf) <- c('SID','UD', 'LD', 'VAL' )
    spline <- mpspline2::mpspline_tidy(obj = sdf, var_name = 'VAL')
    ol[[n]] <- spline
  }
  names(ol) <- names(atts)
  return(ol)
}



