

queryAPI <- function(EP){
  
  EP <- utils::URLencode(EP)
  qry <- paste0(APIUrl, str_replace_all(EP, ' ', '_'))
  print(qry)
  resp <- httr::GET(qry)
  if(resp$status_code==200){
    jsn <- content(resp, type ='text', encoding = 'UTF-8')
    df <- fromJSON(jsn)
    return(df)
  }else{
    return(paste0('Error : There was a problem with the API query - ', qry))
  }
}


getTimeSeries <- function(df, filter=NULL, fill.gaps=T, do.trim=T){
  
  colIdxs <- which(str_starts(colnames(df), filter))
  
  if(length(colIdxs)>0){
    nms <- colnames(df)
    idxDate <- which(colnames(df) == 'Date')
     tdf <- df[, c(idxDate, colIdxs)]
      ts <- xts::xts(tdf[, -1], order.by = tdf$Date)
      names(ts) <- nms[colIdxs]
      
      if(do.trim){
        ts <- zoo::na.trim(ts)
      }
      if(fill.gaps){
        ts <- zoo::na.approx(ts)
      }
      
      return(ts)

  }else{
    return(NULL)
  }
}




getAllSiteData <- function(user_temp_dir=NULL, Network=NULL, SiteName=NULL,  Email=NULL, makeTS=T, fill.gaps=T, do.trim=T){
  
  ol <- list()
  ol$QueryDate <- Sys.Date()
  ol$Network <- Network
  ol$SiteName <- SiteName 
  
  if(!is.null(Email)){
    silo <- getData(user = user_temp_dir, Network = Network, SiteName = SiteName, what='silo', Email = Email)
    
    if(!is.null(silo)){
      ol$BoM_Station_Number <- silo$Station_Number
      ol$BoM_Station_Name <- silo$Station_Name
      ol$Station_Distance <- silo$Distance
      ol$DF$Silo <- silo$Data
    }
  }
  
   
  sensD <-  getData(user = user_temp_dir, Network = Network, SiteName = SiteName, what='volumetric')     
  if(!is.null(sensD)){
        ol$DF$SiteSensorData <- sensD
  }    
    
  
  if(makeTS){
    if(!is.null(ol$DF$SiteSensorData)){
      ol$TS$volumetric_mmPermm <- getTimeSeries(df = ol$DF$SiteSensorData, filter = 'Avail-mm_per_mm-Soil-Moisture', fill.gaps, do.trim)
      ol$TS$volumetric_mm <- getTimeSeries(df = ol$DF$SiteSensorData, filter = 'Avail-mm-Soil-Moisture', fill.gaps, do.trim)
      ol$TS$rawProbe <- getTimeSeries(ol$DF$SiteSensorData, filter = 'Soil-Moisture', fill.gaps=T, do.trim=T)
    }
    if(!is.null(ol$DF$Silo)){
      ol$TS$Rainfall <- getTimeSeries(df=ol$DF$Silo, filter='daily_rain')
    }
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
  ol$CalibrationInfo <-queryAPI(EP = 'BucketInfo')
  
  return(ol)
  
}

getData <- function(user=NULL, Network='', SiteName='', what='volumetric', Email=NULL, fill.gaps=T, do.trim=T){
  # what options are silo, volumetric, globals
  
 # grab data from the daily cache if it exists
  if(is.null(user)){
    datacache <- paste0(tempdir(), '/NSMMDataCache')
    CacheDir <- paste0(datacache, '/', what)
    if(!dir.exists(CacheDir)){dir.create(CacheDir, recursive = T)}
    cachefile <- paste0(CacheDir, '/',  Sys.Date(), '_', Network, '_', SiteName, '_', what, '_Data.rds')
    
    fls <- list.files(CacheDir, full.names = T,recursive = F)
    idxs <- which(!stringr::str_starts(basename(fls), as.character(Sys.Date())))
    if(length(idxs) > 0){
      unlink(fls[idxs])
    }
  }else{
    datacache <- user
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
      if(class(data)!='data.frame'){
        return(NULL)
      }
      data$Data$date <- as.Date(data$Data$date )
      colnames(data$Data)[1] <- 'Date'
    }else if(what=='volumetric'){
      url <-paste0('VolumetricSoilWater?network=', Network,'&sitename=',SiteName ,'&verbose=true')
      data <- queryAPI(EP = url )
      if(class(data)!='data.frame'){
        return(NULL)
      }
      
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


makeDepthIntervals <- function(calibs){
  
  odf <- calibs
  
  odf$IntervalUpper <- NA
  odf$IntervalLower <- NA
  
  for (i in 1:nrow(odf)) {
    rec <- odf[i,]
    
    if(i==1){
      up=0
    }else{
      up=rec$upperdepth
    }
    
    if(i==nrow(odf)){
      low=rec$lowerdepth
    }else{
      
      low=odf[i+1,]$upperdepth
    }
    
    odf[i,]$IntervalUpper <- up
    odf[i,]$IntervalLower <- low
    
  }
  
  return(odf)
} 


