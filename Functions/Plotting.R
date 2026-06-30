

plotNoDataMessage <- function(textColour='red'){
  
  plot( 0, type="n", main=paste(''), 
        xlab='', ylab='',
        yaxs = "i", xaxs = "i", xlim = c(0,2), ylim = c(0,1),
        cex.lab = 1.5,
        axes = FALSE
  )
  text(x = 0.1, y = 0.9, labels = "No data available", col = textColour, cex = 1.8, font = 2, adj = 0)
  text(x = 0.3, y = 0.8, labels = "for this date", col = textColour, cex = 1.8, font = 2, adj = 0)
  
  recorded_plot <- recordPlot()
  return(recorded_plot)
}





plotPAWProfile2 <- function(sitedata=NULL, date, title='', alwaysImage=F){
  
  
  smDayVals <- as.numeric(t(sitedata$TS$volumetric_mmPermm[date]))
  
  if(all(is.na(smDayVals))){
  #if(nrow(smDayVals)==0){
      if(alwaysImage){
        plotNoDataMessage('darkgreen')
      }else{
        return(NULL)
      }
    
  }else{
  
        network <- sitedata$Network
        sitename = sitedata$SiteName
        
        calibs <- glbCalibrationInfo[glbCalibrationInfo$network==network & glbCalibrationInfo$sitename==sitename,]
        calibs[1,]$upperdepth <- 0
        calibs <- makeDepthIntervals(calibs)
        
        smDayVals <- as.numeric(t(sitedata$TS$volumetric_mmPermm[date]))
        sdf <- data.frame(upperD=calibs$IntervalUpper, lowerD=calibs$IntervalLower,LL=calibs$lowerlimit*0.01, DUL=calibs$drainedupperlimit*0.01, availWater=smDayVals)
        
        if(all(is.na(smDayVals))){
          plotNoDataMessage('darkgreen')
        }else{
        
        sbx <- c((c(sdf$LL[1], sdf$LL) * 100), rev(c(sdf$DUL, sdf$DUL[1]) * 100))
        sby <- c(0, calibs$lowerdepth/10, rev(calibs$lowerdepth/10), 0)
        water <- c(c(sdf$LL[1], sdf$LL)*100, (c(rev(sdf$availWater), sdf$availWater[1]) * 100))
        
        plot( 0, type="n", main=paste(''), 
              xlab='Volumeteric Soil Moisture(mm/mm)', ylab='Soil Depth (cm)',
              yaxs = "i", xaxs = "i", xlim = c((min(sdf$LL*100)-2), (max(sdf$DUL*100) + 2)), ylim = rev(range(c(0,sdf$upperD/10))),
              cex.lab = 1.2
        )
        
        polygon(sbx,sby, col=c("navajowhite3"), border=c("navajowhite3"),lwd=1, lty=c("solid"))
        #points(sbx,sby, col=c("red"), pch=0 )
        polygon(water,sby, col=c("lightskyblue"), border=c("lightskyblue"), lwd=1, lty=c("solid"))
        
        }
  }
}

# date=as.Date('2026-06-25')
#  plotPAWProfileGradientColours2(sitedata = sitedata,  date = date, title = 'Ross is cool', alwaysImage=T )
# plotPAWProfile2(sitedata = sitedata,  date = date, title = 'Ross is cool', alwaysImage=T )
# plotGauge2(sitedata = sitedata,  date = date, title = 'Ross is cool', alwaysImage=T )

plotPAWProfileGradientColours2 <- function(sitedata=NULL, date, title='', alwaysImage=F){
  
  smDayVals <- as.numeric(t(sitedata$TS$volumetric_mmPermm[date]))
  
  if(all(is.na(smDayVals))){
      if(alwaysImage){
        plotNoDataMessage('darkgreen')
      }else{
        return(NULL)
      }
    
  }else{

    network <- sitedata$Network
    sitename = sitedata$SiteName
    
    calibs <- glbCalibrationInfo[glbCalibrationInfo$network==network & glbCalibrationInfo$sitename==sitename,]
    calibs[1,]$upperdepth <- 0
    calibs <- makeDepthIntervals(calibs)
    
    sdf <- data.frame(upperD=calibs$IntervalUpper, lowerD=calibs$IntervalLower,LL=calibs$lowerlimit*0.01, DUL=calibs$drainedupperlimit*0.01, availWater=smDayVals)
    volperc <- (sdf$availWater-sdf$LL) / (sdf$DUL-sdf$LL) * 100
    
    sbx <- c((c(sdf$LL[1], sdf$LL) * 100), rev(c(sdf$DUL, sdf$DUL[1]) * 100))
    sby <- c(0, calibs$lowerdepth/10, rev(calibs$lowerdepth/10), 0)
    water <- c(c(sdf$LL[1], sdf$LL)*100, (c(rev(sdf$availWater), sdf$availWater[1]) * 100))
    
      cols <- colorRampPalette(c('brown', '#f5e1c1', '#84d3f5', "#042d5e"))( ceiling(max(volperc))) 
      
      plot( 0, type="n", main=paste(''), 
            xlab='Available Moisture Distribution', 
            ylab='Soil Depth (cm)',
            yaxs = "i", 
            xaxt = "n", 
            xlim = c(0, 1.5), 
            ylim = rev(range(c(0,calibs$upperdepth/10))),
            cex.lab = 1.5
      )
      
      
      col.labels<-c("Wet","Moist","Dry")
      plotrix::color.legend(1.2,20,1.3,150,col.labels, rev(cols),gradient="y", align = 'rb')
      
      wdf <- data.frame(1,calibs$upperdepth/10, calibs$lowerdepth/10, volperc, calibs$lowerlimit, calibs$drainedupperlimit)
      names(wdf) <- c('sid', 'upper', 'lower', 'val', 'LL', 'DUL')
      awcSpline <- splineVals(indf=wdf, atts=c('val', 'LL', 'DUL'))
      
      moistPerc <- awcSpline[[1]]$est_1cm$SPLINED_VALUE
      ll <- awcSpline[[2]]$est_1cm$SPLINED_VALUE
      ul <- awcSpline[[3]]$est_1cm$SPLINED_VALUE
      
      for (i in 1:length(moistPerc)) {
        
        val <- moistPerc[i]
        l <- ll[i]
        u <- ul[i]
        
        c <- as.integer(val)
        x = c(0,1,1,0, 0)
        y = c(i, i,i+1, i+1, i)
        polygon(x,y,col=cols[c],   border=NA)
        
  }
  
  }
  
}


plotGauge2 <- function(sitedata=NULL, date, title='', alwaysImage=F){
  
  smDayVals <- as.numeric(t(sitedata$TS$volumetric_mmPermm[date]))
  
  
  if(all(is.na(smDayVals))){
      if(alwaysImage){
        plotNoDataMessage('darkgreen')
      }else{
        return(NULL)
      }
    
  }else{
      network <- sitedata$Network
      sitename = sitedata$SiteName
      
      calibs <- glbCalibrationInfo[glbCalibrationInfo$network==network & glbCalibrationInfo$sitename==sitename,]
      calibs[1,]$upperdepth <- 0
      calibs <- makeDepthIntervals(calibs)
      bucketSize =  as.integer(sum( (calibs$drainedupperlimit - calibs$lowerlimit)/100 * (calibs$IntervalLower-calibs$IntervalUpper)))
      
      sdf <- data.frame(upperD=calibs$IntervalUpper, lowerD=calibs$IntervalLower,LL=calibs$lowerlimit*0.01, DUL=calibs$drainedupperlimit*0.01, availWater=smDayVals)
      
      totwat <- sum((sdf$lowerD-sdf$upperD) * (sdf$availWater-sdf$LL))
      percWat <- (totwat/bucketSize) * 100
      
      data.frame('Available Moisture'=percWat) %>%
    c3::c3() %>% c3::c3_gauge(title = 'Colors', label = 'Available Moisture', pattern = c('brown', '#f5e1c1', '#84d3f5', "#042d5e"))
      
  }
}


