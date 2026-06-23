

plotNoDataMessage <- function(textColour='red'){
  
  plot( 0, type="n", main=paste(''), 
        xlab='', ylab='',
        yaxs = "i", xaxs = "i", xlim = c(0,2), ylim = c(0,1),
        cex.lab = 1.5,
        axes = FALSE
  )
  text(x = 0.1, y = 0.9, labels = "No data available", col = textColour, cex = 1.8, font = 2, adj = 0)
  text(x = 0.1, y = 0.8, labels = "for this date", col = textColour, cex = 1.8, font = 2, adj = 0)
}




plotPAWProfile <- function(ts, calibs, title, bktSize=null){
  calibs[1,]$upperdepth <- 0
  
  sbx <- c(calibs$lowerlimit, rev(calibs$drainedupperlimit))
  sby <- c(calibs$upperdepth/10, rev(calibs$upperdepth/10))
  water <- c(calibs$lowerlimit, rev((calibs$lowerlimit)+as.numeric(ts)))
  
  plot( 0, type="n", main=paste(''), 
        xlab='Volumeteric Soil Moisture', ylab='Soil Depth (cm)',
        yaxs = "i", xaxs = "i", xlim = c((min(calibs$lowerlimit))-2, (max(calibs$drainedupperlimit)) + 2), ylim = rev(range(c(0,calibs$upperdepth/10))),
        cex.lab = 1.5
  )
  
  polygon(sbx,sby,
          col=c("navajowhite3"),
          border=c("navajowhite3"),
          lwd=1, lty=c("solid"))
  
  polygon(water,sby,
          col=c("lightskyblue"),
          border=c("lightskyblue"),
          lwd=1, lty=c("solid"))
}



plotPAWProfileGradientColours <- function(ts, calibs, title, bktSize=null){
  
  calibs[1,]$upperdepth <- 0
  
  mm <- calibs$lowerdepth - calibs$upperdepth
  volperc <- (ts/mm) * 100
  uperc <- (calibs$drainedupperlimit - calibs$lowerlimit)
  upperperc <- calibs$drainedupperlimit - calibs$lowerlimit
  
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
 
  
  wdf <- data.frame(1,calibs$upperdepth/10, calibs$lowerdepth/10, t(volperc), calibs$lowerlimit, calibs$drainedupperlimit)
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
    polygon(x,y,col=cols[c],   border=cols[c])
    
  }

}

