library(tictoc)
library(raster)
library(jsonlite)
library(lubridate)
library(stringr)
library(XML)
library(xts)

# sdate = '04-12-2015'
# edate <- '01-02-2016'
# product <- 'totalbucket'
# lon <- 140.55379 
# lat <- -24.78697



#drillSMIPS(lon, lat, product, smipsRoot=smipsRoot)

drillSMIPS <- function( lon=NULL, lat=NULL, product=NULL, sdate=format(Sys.Date()-120,  "%d-%m-%Y"), edate=format(Sys.Date()-90,  "%d-%m-%Y"), smipsRoot, AvailableMapProducts){

  
  print(product)
  dts <- seq.Date(as.Date(sdate, "%d-%m-%Y"), as.Date(edate, "%d-%m-%Y"), by='day')
  b <- vector(mode = "numeric", length = length(dts))
  ots <- xts(x=b, order.by = dts)
  
  rec <- AvailableMapProducts[AvailableMapProducts$prodNames==product,]
  print(rec)

    for(i in 1:length(dts)){
      print(i)
      yr <- lubridate::year(dts[i])
      mth <- str_pad(lubridate::month(dts[i]), side = 'left', width=2, pad='0')
      dy <- str_pad(lubridate::day(dts[i]), side = 'left', width=2, pad='0')
      
      inR <- paste0(smipsRoot, '/', rec$prodDirs, '/', yr, '/smips_', rec$prodFnames, '_', yr, mth, dy, '.tif')
  
      if(file.exists(inR)){
          
          cmd <- paste0('gdallocationinfo -geoloc -xml ', inR, ' ', lon, ' ', lat)
          s <- system(cmd, intern = T)
          xmlStr <- xmlParse(s)
          
          p <- xpathApply(xmlStr , '/Report/BandReport/Value')
          v <- as.numeric(xmlValue(p))
          ots[dts[i]] <- v
      }
    
    }
  return(ots)
}


