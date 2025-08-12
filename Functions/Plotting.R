

plotPAWProfile <- function(ts, calibs, title, bktSize=null){
  calibs[1,]$UpperDepth <- 0
  
  sbx <- c(calibs$LowerLimit, rev(calibs$DrainedUpperLimit))
  sby <- c(calibs$UpperDepth/10, rev(calibs$UpperDepth/10))
  water <- c(calibs$LowerLimit, rev((calibs$LowerLimit)+as.numeric(ts)))
  
  plot( 0, type="n", main=paste(''), 
        xlab='Volumeteric Soil Moisture', ylab='Soil Depth (cm)',
        yaxs = "i", xaxs = "i", xlim = c((min(calibs$LowerLimit))-2, (max(calibs$DrainedUpperLimit)) + 2), ylim = rev(range(c(0,calibs$UpperDepth/10))),
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
  
  calibs[1,]$UpperDepth <- 0
  
  cols <- colorRampPalette(c('brown', '#f5e1c1', '#84d3f5', "#042d5e"))( 100 )
  
  plot( 0, type="n", main=paste(''), 
        xlab='Available Moisture Distribution', 
       ylab='Soil Depth (cm)',
        yaxs = "i", 
       xaxt = "n", 
        xlim = c(0, 1.5), 
        ylim = rev(range(c(0,calibs$UpperDepth/10))),
        cex.lab = 1.5
  )
  
  
  col.labels<-c("Wet","Moist","Dry")
 plotrix::color.legend(1.2,20,1.3,150,col.labels, rev(cols),gradient="y", align = 'rb')
 
  
  wdf <- data.frame(1,calibs$UpperDepth/10, calibs$LowerDepth/10, t(ts), calibs$LowerLimit, calibs$DrainedUpperLimit)
  names(wdf) <- c('sid', 'upper', 'lower', 'val', 'LL', 'DUL')
  awcSpline <- splineVals(indf=wdf, atts=c('val', 'LL', 'DUL'))
  
  
  
  
  wc <- awcSpline[[1]]$est_1cm
  ll <- awcSpline[[2]]$est_1cm
  ul <- awcSpline[[3]]$est_1cm
  
  for (i in 1:nrow(wc)) {
    
    val <- wc[i,]
    l <- ll[i,]
    u <- ul[i,]

    c <- as.integer(val$SPLINED_VALUE / (u$SPLINED_VALUE - l$SPLINED_VALUE) *100)
    x = c(0,1,1,0, 0)
    y = c(l$UD, l$UD,l$LD, l$LD, l$UD)
    polygon(x,y,col=cols[c],   border=cols[c])
    
  }

}

