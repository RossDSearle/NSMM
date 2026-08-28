


makeClimatePageHTML <- function(CurrentSiteData=NULL){
  
  imagePath <- tempfile(fileext = '.png')
  seasonsDF <- data.frame(code=c('DJF',  'MAM', 'JJA', 'SON'), season=c( 'Summer', 'Autumn', 'Winter', 'Spring'))
  
  #siloDF <- read.csv('c:/temp/nhill.csv')
  #siloDF <- CurrentSiteData$TS$Rainfall
  rain_xts <- CurrentSiteData$TS$Rainfall
  colnames(rain_xts) <- 'Rainfall'
  
  seasonal_totals <- AllSeasonalRainfallTotals(rain_xts)
  fourseasons <- tail(seasonal_totals,4)
  seasDeciles <- Decile_AllSeasons(rain_xts, wide=F)
  fourseasonsDeciles <- tail(seasDeciles,4)
  
  last_12_months <- last(rain_xts, "12 months")
  
  
  
  seastxt <- 'Rainfall totals for the previous 4 seasons : <ul>'
  for (i in nrow(fourseasons):1) {
    rec <- fourseasons[i,]
    decrec <- fourseasonsDeciles[i,]
    season <- seasonsDF[seasonsDF$code == rec$Season,]$season
    seastxt <- paste0(seastxt, "<li>", season, " : <span style=' font-weight: bold;'>", round(rec$TotalRain), "</span> mm (Decile ", decrec$Decile, ")</li>" )
  }
  seastxt <- paste0(seastxt, '</ul><BR>')
  
  html <- '<BR><BR>'
  
  locationTxt <- paste0('<P>This climate information was generated using the data from the BoM station at ', CurrentSiteData$BoM_Station_Name, 
                        ' (ID:', CurrentSiteData$BoM_Station_Number, ') ', CurrentSiteData$Station_Distance , 's 
                        from this soil moisture probe site using data sourced from the QLD Govt. 
                        <a href="https://www.longpaddock.qld.gov.au/silo/" target="_blank">SILO</a> database.</P>.<BR>')
  
  ytdRainfall <- paste0('<P>Year to date rainfall is <span style="color: blue; font-weight: bold;">', tail(AnnualRainfall(rain_xts),1), '</span> mm </P>')
  
  ol <- Decile_YearToDate(rain_xts)
  dectxt <- sprintf('The year-to-date rainfall (Jan 1 to Day %d) for %s (%s mm) falls into <span style="color: blue; font-weight: bold;">Decile %d</span>.\n', 
                    ol$current_ytd_day, ol$current_year_label, ol$current_year_total, ol$current_decile)
  
  ytdDecile <- paste0('<P>', dectxt, '</P>')
  twelvemthsRainfall <- paste0('<P>Rainfall total for the last 12 months is <span style="color: blue; font-weight: bold;">', sum(last_12_months$Rainfall) , '</span> mm </P>')
  
  html <- paste0(html, locationTxt)
  html <- paste0(html, ytdRainfall)
  html <- paste0(html, ytdDecile)
  
  html <- paste0(html, twelvemthsRainfall)
  
  html <- paste0(html, seastxt)
  
  b64Monthly <- plotMonthlyTotalComparisons(rain_xts, imagePath)
  b64CumAnn <- plotCummulativeDailyRainfallPerYear(rain_xts, imagePath)
  b64heatMap <- plotSeasonalHeatMap(rain_xts, imagePath)
  
  html <- paste0(html, '<table>')
  
  html <- paste0(html, '<tr><td><img src="data:image/png;base64,', b64Monthly, '" 
          width="', 800, '" 
          height="', 400, '"></td></tr>')
  
  html <- paste0(html, '<tr><td><img src="data:image/png;base64,', b64CumAnn, '" 
          width="', 800, '" 
          height="', 400, '"></td></tr>')
  html <- paste0(html, '<tr><td><img src="data:image/png;base64,', b64heatMap, '" 
          width="', 400, '" 
          height="', 800, '"></td></tr>')
 
  
   html <- paste0(html, '</table>')
  
  html
  
  
  
  
  
  
  
  
  
  
  
  # np <- ncol(wt)-3
  # 
  # cnt=0
  # for (i in 1:np) {
  #   cnt=cnt+1
  #   b64 <-  plotLabDataDB(df=wt,i)
  #   html <- paste0(html, '<td><img src="data:image/png;base64,', b64, '" 
  #         width="', 250, '" 
  #         height="', 300, '"></td>')
  #   
  #   if(cnt==4){
  #     html <- paste0(html, '</tr><tr>')
  #     cnt=0
  #   }
  # }
  # 
  # html <- paste0(html, '</tr></table')

  return(html)
  
}

makeRainTSfromSILOData <- function(siloDF){
  df <-siloDF[, c(4,5)]
  colnames(df) <- c("Date", "daily_rain")
  rain_xts <- xts::xts(df[, -1], order.by = as.Date(df[,1]))
  return(rain_xts)
}

AnnualRainfall <- function(rain_xts){
      annual_rain <- apply.yearly(rain_xts, sum, na.rm = TRUE)
      return(annual_rain)
}


AnnualRainfallDeciles <- function(rain_xts){
      annual_deciles <- quantile(coredata(annual_rain), probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE)
      return(annual_deciles)
}


Decile_WholeYears<- function(rain_xts){

      # 1. Calculate annual totals
      annual_rain <- apply.yearly(rain_xts, sum, na.rm = TRUE)
      
      # 2. Compute historical decile thresholds (0% to 100% to create 10 bins)
      decile_thresholds <- quantile(coredata(annual_rain), probs = seq(0, 1, by = 0.1), na.rm = TRUE)
      
      # 3. Extract the rainfall total for the current (most recent) year
      current_year_total <- as.numeric(tail(annual_rain, 1))
      current_year_label <- format(index(tail(annual_rain, 1)), "%Y")
      
      # 4. Assign the current year to its decile rank (returns a value from 1 to 10)
      current_decile <- findInterval(current_year_total, decile_thresholds, all.inside = TRUE)
      
      # View the result
      dectext <- sprintf("The rainfall for %s (%s mm) falls into Decile %d.\n", 
                  current_year_label, round(current_year_total, 1), current_decile)
      
      return(dectext)
}


Decile_YearToDate <- function(rain_xts){

      # 1. Identify the current day-of-year index (e.g., today's date progress)
      # tail(index(rain_xts), 1) ensures it uses the latest daily timestamp in your data
      latest_date <- tail(index(rain_xts), 1)
      current_ytd_day <- as.integer(format(latest_date, "%j"))
      
      # 2. Filter daily data to include only days <= current day-of-year across ALL years
      # format(index(x), "%j") extracts the day number (001-366) for every record
      ytd_daily_rain <- rain_xts[as.integer(format(index(rain_xts), "%j")) <= current_ytd_day]
      
      # 3. Calculate the YTD annual totals for every year using the filtered daily data
      ytd_annual_rain <- apply.yearly(ytd_daily_rain, sum, na.rm = TRUE)
      
      # 4. Compute historical YTD decile thresholds (0% to 100%)
      decile_thresholds_ytd <- quantile(coredata(ytd_annual_rain), probs = seq(0, 1, by = 0.1), na.rm = TRUE)
      
      # 5. Extract the YTD rainfall total and label for the current year
      current_year_total <- as.numeric(tail(ytd_annual_rain, 1))
      current_year_label <- format(index(tail(ytd_annual_rain, 1)), "%Y")
      
      # 6. Assign the current year's YTD total to its YTD decile rank
      current_decile <- findInterval(current_year_total, decile_thresholds_ytd, all.inside = TRUE)
      
      # View the adjusted output
      
      ol <- list()
      ol$current_ytd_day <- current_ytd_day
      ol$current_year_label <- current_year_label
      ol$current_year_total <- round(current_year_total, 1)
      ol$current_decile <- current_decile
      return(ol)
      
      # dectxt <- sprintf("The year-to-date rainfall (Jan 1 to Day %d) for %s (%s mm) falls into Decile %d.\n", 
      #                       current_ytd_day, current_year_label, round(current_year_total, 1), current_decile)
      # 
      # return(dectxt)

}



Decile_Season <- function(rain_xts){
      # 1. Convert the daily xts object to a data.table
      rain_dt <- data.table(
        Date = index(rain_xts),
        Rain = as.numeric(rain_xts)
      )
      rain_dt[is.na(Rain), Rain := 0]
      
      # 2. Extract calendar attributes
      rain_dt[, Month := as.integer(format(Date, "%m"))]
      rain_dt[, CalYear := as.integer(format(Date, "%Y"))]
      
      # 3. Assign months to meteorological seasons
      rain_dt[Month %in% c(12, 1, 2), Season := "DJF"] # Summer
      rain_dt[Month %in% c(3, 4, 5),  Season := "MAM"] # Autumn
      rain_dt[Month %in% c(6, 7, 8),  Season := "JJA"] # Winter
      rain_dt[Month %in% c(9, 10, 11), Season := "SON"] # Spring
      
      # 4. Critical Step: Shift December to the next year's season group 
      # (e.g., Dec 2025 + Jan 2026 + Feb 2026 combine into the "2026" Summer Season)
      rain_dt[, SeasonYear := CalYear]
      rain_dt[Month == 12, SeasonYear := CalYear + 1]
      
      # 5. Aggregate total rainfall by Season and SeasonYear
      seasonal_totals <- rain_dt[, .(TotalRain = sum(Rain)), by = .(SeasonYear, Season)]
      
      # 6. Extract the most recent completed or active season in your data
      current_season_record <- tail(seasonal_totals, 1)
      curr_year   <- current_season_record$SeasonYear
      curr_season <- current_season_record$Season
      curr_total  <- current_season_record$TotalRain
      
      # 7. Isolate the historical totals for *only* that specific season type to build thresholds
      historical_season_totals <- seasonal_totals[Season == curr_season, TotalRain]
      
      # 8. Compute decile thresholds and assign the current season
      decile_thresholds <- quantile(historical_season_totals, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
      current_decile <- findInterval(curr_total, decile_thresholds, all.inside = TRUE)
      
      # View the result
      return(sprintf("The %s %s season rainfall (%s mm) falls into Decile %d for that season.\n", 
                  curr_season, curr_year, round(curr_total, 1), current_decile))

}


AllSeasonalRainfallTotals <- function(rain_xts){
  
  # 1. Convert daily xts object to data.table
  rain_dt <- data.table(
    Date = index(rain_xts),
    Rain = as.numeric(rain_xts)
  )
  rain_dt[is.na(Rain), Rain := 0]
  
  # 2. Assign dates to meteorological seasons
  rain_dt[, Month := as.integer(format(Date, "%m"))]
  rain_dt[, CalYear := as.integer(format(Date, "%Y"))]
  
  rain_dt[Month %in% c(12, 1, 2), Season := "DJF"] # Summer
  rain_dt[Month %in% c(3, 4, 5),  Season := "MAM"] # Autumn
  rain_dt[Month %in% c(6, 7, 8),  Season := "JJA"] # Winter
  rain_dt[Month %in% c(9, 10, 11), Season := "SON"] # Spring
  
  # 3. Shift December to align with the following calendar year's summer group
  rain_dt[, SeasonYear := CalYear]
  rain_dt[Month == 12, SeasonYear := CalYear + 1]
  
  # 4. Calculate total rainfall for each season per year
  seasonal_totals <- rain_dt[, .(TotalRain = sum(Rain)), by = .(SeasonYear, Season)]
  seasonal_totals
}





Decile_AllSeasons <- function(rain_xts, wide=F){

# 1. Convert daily xts object to data.table
rain_dt <- data.table(
  Date = index(rain_xts),
  Rain = as.numeric(rain_xts)
)
rain_dt[is.na(Rain), Rain := 0]

# 2. Assign dates to meteorological seasons
rain_dt[, Month := as.integer(format(Date, "%m"))]
rain_dt[, CalYear := as.integer(format(Date, "%Y"))]

rain_dt[Month %in% c(12, 1, 2), Season := "DJF"] # Summer
rain_dt[Month %in% c(3, 4, 5),  Season := "MAM"] # Autumn
rain_dt[Month %in% c(6, 7, 8),  Season := "JJA"] # Winter
rain_dt[Month %in% c(9, 10, 11), Season := "SON"] # Spring

# 3. Shift December to align with the following calendar year's summer group
rain_dt[, SeasonYear := CalYear]
rain_dt[Month == 12, SeasonYear := CalYear + 1]

# 4. Calculate total rainfall for each season per year
seasonal_totals <- rain_dt[, .(TotalRain = sum(Rain)), by = .(SeasonYear, Season)]

# 5. Calculate and assign deciles INDIVIDUALLY for each season type
# This ensures Summer totals are only ranked against historical Summers, etc.
seasonal_totals[, Decile := {
  thresholds <- quantile(TotalRain, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
  findInterval(TotalRain, thresholds, all.inside = TRUE)
}, by = Season]

if(!wide){
  return(seasonal_totals)
}else{
  # 6. Reshape the data into a wide matrix (Years as rows, Seasons as columns)
  historical_matrix <- dcast(
    seasonal_totals, 
    SeasonYear ~ Season, 
    value.var = "Decile"
  )
  
  # 7. Reorder columns to follow chronological calendar order (DJF, MAM, JJA, SON)
  setcolorder(historical_matrix, c("SeasonYear", "DJF", "MAM", "JJA", "SON"))
  setnames(historical_matrix, "SeasonYear", "Year")
  
  # View the full historical decile matrix
  return(historical_matrix)
}



}


plotSeasonalHeatMap <- function(rain_xts, imagePath=NULL){

  df_daily <- data.frame(date = index(rain_xts), value = coredata(rain_xts), row.names = NULL)
  colnames(df_daily) <- c('date', 'value')
  
  # 2. Data Processing & Seasonal Aggregation
  df_seasonal <- df_daily %>%
    mutate(
      date = as.Date(date),
      month = month(date),
      calendar_year = year(date),
      # Assign standard meteorological seasons
      season = case_when(
        month %in% c(12, 1, 2) ~ "Summer (DJF)",
        month %in% c(3, 4, 5)  ~ "Autumn (MAM)",
        month %in% c(6, 7, 8)  ~ "Winter (JJA)",
        month %in% c(9, 10, 11) ~ "Spring (SON)"
      ),
      # Adjust year for Summer (December belongs to the next year's summer season)
      season_year = if_else(month == 12, calendar_year + 1, calendar_year)
    ) %>%
    # Filter out incomplete seasons (e.g., Summer 1960 missing Dec 1959, or late 2026)
    group_by(season_year, season) %>%
    summarise(
      seasonal_total = sum(value, na.rm = TRUE),
      days_counted = n(),
      .groups = "drop"
    ) %>%
    # Filter for seasons that have roughly a full complement of days (~90 days)
    filter(days_counted >= 80)
  
  # 3. Calculate Deciles (1 to 10) Relative to Each Season's History
  df_deciles <- df_seasonal %>%
    group_by(season) %>%
    mutate(
      # ntile splits the data into 10 equally sized groups (deciles)
      decile = ntile(seasonal_total, 10)
    ) %>%
    ungroup()
  
  # Set season as a factor to ensure correct chronological order on the plot
  df_deciles$season <- factor(df_deciles$season, 
                              levels = c("Summer (DJF)", "Autumn (MAM)", "Winter (JJA)", "Spring (SON)"))
  
  # 4. Generate the Heatmap
p <- ggplot(df_deciles, aes(x = season, y = factor(season_year), fill = factor(decile))) +
    geom_tile(color = "white", size = 0.2) +
    scale_fill_brewer(palette = "RdYlBu", direction = 1, name = "Decile") +
    scale_y_discrete(breaks = function(x) x[seq(1, length(x), by = 5)]) + # Show every 5th year for readability
    scale_x_discrete(labels = c("Summer", "Autumn", "Winter", "Spring")) +
    labs(
      title = "Historical Seasonal Rainfall Deciles",
      subtitle = "1960 - 2026",
      x = "Season",
      y = "Year"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "right"
      #,aspect.ratio = 0.5
    )

if(!is.null(imagePath)){
  
  ggsave(imagePath, plot = p, width = 4, height = 8, dpi = 300)
  b64 <- base64enc::base64encode(imagePath)
  unlink(imagePath)
  return(b64)
}else{
  p
}

}




plotMonthlyTotalComparisons <- function(rain_xts, imagePath=NULL){

      # 1. Convert the daily xts object to a data.table
      rain_df <- data.table(
        Date = index(rain_xts),
        Rain = as.numeric(rain_xts)
      )
      rain_df[is.na(Rain), Rain := 0]
      
      # 2. Extract Year and Month attributes
      rain_df[, Year  := format(Date, "%Y")]
      rain_df[, Month := format(Date, "%b")] # e.g., "Jan", "Feb"
      rain_df[, MonthNum := as.integer(format(Date, "%m"))]
      
      # 3. Calculate total rainfall for every specific month in the dataset
      monthly_totals <- rain_df[, .(TotalRain = sum(Rain)), by = .(Year, Month, MonthNum)]
      
      # 4. Separate this year (2026) totals from historical records
      this_year_totals <- monthly_totals[Year == "2026", .(Month, MonthNum, Rain = TotalRain, Type = "2026 Total")]
      
      # 5. Calculate the historical baseline average for each calendar month
      historical_avg <- monthly_totals[Year != "2026", .(Rain = mean(TotalRain)), by = .(Month, MonthNum)]
      historical_avg[, Type := "Historical Average"]
      
      # 6. Combine datasets and sort chronologically by month number
      plot_data <- rbind(this_year_totals, historical_avg)
      plot_data[, Month := reorder(Month, MonthNum)]
      
      # 7. Generate the side-by-side bar chart
      p <- ggplot(plot_data, aes(x = Month, y = Rain, fill = Type)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.7) +
        scale_fill_manual(values = c("2026 Total" = "steelblue", "Historical Average" = "darkgrey")) +
        theme_minimal() +
        labs(
          title = "2026 Monthly Rainfall vs. Historical Baseline Average",
          x = "Month",
          y = "Rainfall Total (mm)",
          fill = "Dataset"
        )
      
      if(!is.null(imagePath)){
      
        ggsave(imagePath, plot = p, width = 8, height = 4, dpi = 300)
        b64 <- base64enc::base64encode(imagePath)
        unlink(imagePath)
        return(b64)
      }else{
        p
      }

}


plotCummulativeDailyRainfallPerYear <- function(rain_xts, imagePath=NULL){
  
  # 1. Convert the daily xts object to a data.table
  rain_df <- data.table(
    Date = index(rain_xts),
    Rain = as.numeric(rain_xts)
  )
  
  # 2. Extract calendar attributes
  rain_df[, Year := format(Date, "%Y")]
  rain_df[is.na(Rain), Rain := 0]
  
  # 3. Calculate cumulative daily rainfall within each year
  rain_df[, CumulativeRain := cumsum(Rain), by = Year]
  
  # 4. Create a fake static date for alignment (e.g., locking all data to the year 2024)
  # This forces ggplot to treat the x-axis as a timeline from January to December
  rain_df[, FakeDate := as.Date(paste0("2024-", format(Date, "%m-%d")))]
  
  # 5. Highlight the current year (2026) vs historical records
  rain_df[, Period := ifelse(Year == "2026", "Current Year (2026)", "Historical")]
  
  # 6. Plot using Date scales for the x-axis
  p <- ggplot(rain_df, aes(x = FakeDate, y = CumulativeRain, group = Year)) +
    geom_line(data = rain_df[Period == "Historical"], aes(color = "Historical"), alpha = 0.4, linewidth = 0.5) +
    geom_line(data = rain_df[Period == "Current Year (2026)"], aes(color = "Current Year (2026)"), linewidth = 1.2) +
    scale_color_manual(name = "Year Type", values = c("Current Year (2026)" = "steelblue", "Historical" = "darkgrey")) +
    # Format x-axis to show short month names (Jan, Feb, Mar...)
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    theme_minimal() +
    labs(
      title = "Historical Cumulative Daily Rainfall Comparison",
      x = "Month",
      y = "Cumulative Rainfall (mm)"
    )
  
  if(!is.null(imagePath)){
    
    ggsave(imagePath, plot = p, width = 8, height = 4, dpi = 300)
    b64 <- base64enc::base64encode(imagePath)
    unlink(imagePath)
    return(b64)
  }else{
    p
  }
  
}







########   Forecast  ########

get_met_forecast <- function(lng, lat){

forecast <- get_metno_forecast(
  latitude = lat,
  longitude = lng,
  format = "complete",
  api_key = 'ross.searle@csiro.au'
)
return(forecast)
}







# find_forecast_towns(longitude = 121.8913, latitude = -33.8614)
# 
# get_precis_forecast(state = "AUS")
# 
# 
# ftp_url <- 'ftp://ftp.bom.gov.au/anon/gen/fwo/WA_PT006.shtml'
# download.file(url = ftp_url, destfile = 'c:/temp/f.xml', mode = "wb")
