

plotPAWProfile <- function(ts, calibs, title, bktSize=null){
  
  sbx <- c(calibs$LowerLimit, rev(calibs$DrainedUpperLimit))
  sby <- c(calibs$UpperDepth/10, rev(calibs$UpperDepth/10))
  water <- c(calibs$LowerLimit, rev((calibs$LowerLimit)+as.numeric(ts)))
  
  plot( 0, type="n", main=paste(''), 
        xlab='Volumeteric Soil Moisture', ylab='Soil Depth (cm)',
        yaxs = "i", xaxs = "i", xlim = c((min(calibs$LowerLimit))-2, (max(calibs$DrainedUpperLimit)) + 2), ylim = rev(range(c(0,110))),
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
  
  # if(!is.null(bktSize)){
  #     text(0.5,0.4, paste0("Total Bucket Size = ", round(bktSize), ' mm' ), font=2)
  # }
}
