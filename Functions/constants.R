
  Constants <<- list()
  
  
  Constants$knownFeatures <- as.list( c(SoilMoisture='Soil-Moisture', SoilTemperature='Soil-Temperature', Rainfall='Rainfall', SoilEC='Soil-EC', Humidity='Humidity', 
                                    Temperature='Temperature', WindDirection='Wind-Direction', WindSpeed='Wind-Speed', 
                                    AtmosphericPressure='Atmospheric-Pressure', VapourPressure='Vapour-Pressure', DewPoint='Dew-Point', 
                                    DeltaT='Delta T', SoilSuction='Soil-Suction', SolarRadiation='Solar-Radiation', PAW='PAW', 
                                    Salinity='Salinity', DielectricConstant='Dielectric-Constant'))
  
  Constants$Units <- as.list(c(mm='mm', Percent='Percent', Degrees = 'Degrees', DegreesCelcius= 'Degrees-Celcius', kmPerHr='km/hr', 
                           MetresPerSec='m/s', SoilEC='dS/m', DielectricConstant='None'))
  

  

  Constants$NetworkColour <- as.list(c(NSMM='skyblue', Boorowa_Research_Farm='yellow', TERN_Cosmoz = 'pink'))
  
  bl <- list()
  bl$AustXMax = 154.0
  bl$AustXMin = 112.0
  bl$AustYMax = -10.0
  bl$AustYMin = -44.0
  
  Constants$AustExtent <- bl
  


