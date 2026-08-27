library(xts)
library(data.table)

siloDF <- read.csv('c:/temp/nhill.csv')









Decile_WholeYears(rain_xts)

Decile_YearToDate(rain_xts)


makeRainTSfromSILOData <- function(siloDF){
  df <-siloDF[, c(4,5)]
  colnames(df) <- c("Date", "Rainfall")
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
      
      dectxt <- sprintf("The year-to-date rainfall (Jan 1 to Day %d) for %s (%s mm) falls into Decile %d.\n", 
                            current_ytd_day, current_year_label, round(current_year_total, 1), current_decile)
      
      return(dectxt)

}






########   Seasonal Decile for this season  ############


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
cat(sprintf("The %s %s season rainfall (%s mm) falls into Decile %d for that season.\n", 
            curr_season, curr_year, round(curr_total, 1), current_decile))



#######   All Seasonal Deciles   ########

library(xts)
library(data.table)

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
print(historical_matrix)


#######  seasonal heatmap   #######

library(xts)
library(data.table)
library(ggplot2)

# 1. Generate the seasonal totals and deciles (from previous step)
# [Re-using the 'seasonal_totals' long-format data table before dcast]
seasonal_totals[, Season := factor(Season, levels = c("DJF", "MAM", "JJA", "SON"))]

# 2. Filter out incomplete seasons (e.g., if a season hasn't ended yet)
# This prevents an ongoing season from showing a false low decile
# (Assuming rain_xts ends mid-way through a season)
latest_date <- max(index(rain_xts))
# Optional: remove the absolute final row if it represents an active unfinished season

# 3. Create the visual heatmap
ggplot(seasonal_totals, aes(x = Season, y = as.factor(SeasonYear), fill = Decile)) +
  geom_tile(color = "white", linewidth = 0.2) +
  # Custom diverging color scale (Red = Dry, White = Average, Blue = Wet)
  scale_fill_gradientn(
    colors = c("#d73027", "#f46d43", "#fdae61", "#fee090", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac"),
    breaks = 1:10,
    limits = c(1, 10),
    name = "Decile Rank"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold", size = 11),
    axis.text.y = element_text(size = 9)
  ) +
  labs(
    title = "Historical Seasonal Rainfall Deciles",
    subtitle = "Decile 1 (Red) = Very Dry | Decile 10 (Blue) = Very Wet",
    x = "Meteorological Season",
    y = "Year"
  )







####   Deciles for all years  #######

# 1. Aggregate daily rainfall into annual totals
annual_rain <- apply.yearly(rain_xts, sum, na.rm = TRUE)

# 2. Compute the baseline historical decile thresholds (0% to 100%)
decile_thresholds <- quantile(coredata(annual_rain), probs = seq(0, 1, by = 0.1), na.rm = TRUE)

# 3. Assign every year to a decile rank (1 to 10)
# Convert to numeric vector first to strip the xts time attributes during matching
decile_ranks <- findInterval(as.numeric(annual_rain), decile_thresholds, all.inside = TRUE)

# 4. Combine results into a clean, readable data frame
historical_deciles <- data.frame(
  Year   = format(index(annual_rain), "%Y"),
  Rain_mm = round(as.numeric(annual_rain), 1),
  Decile = decile_ranks
)

# View the full historical table
print(historical_deciles)






########  Cummulative daily rainfall per year  #################


library(xts)
library(data.table)
library(ggplot2)
library(scales)

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
ggplot(rain_df, aes(x = FakeDate, y = CumulativeRain, group = Year)) +
  geom_line(data = rain_df[Period == "Historical"], aes(color = "Historical"), alpha = 0.4, linewidth = 0.5) +
  geom_line(data = rain_df[Period == "Current Year (2026)"], aes(color = "Current Year (2026)"), linewidth = 1.2) +
  scale_color_manual(name = "Year Type", values = c("Current Year (2026)" = "red", "Historical" = "darkgrey")) +
  # Format x-axis to show short month names (Jan, Feb, Mar...)
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal() +
  labs(
    title = "Historical Cumulative Daily Rainfall Comparison",
    x = "Month",
    y = "Cumulative Rainfall (mm)"
  )




######  Monthly total comparisons  #########

library(xts)
library(data.table)
library(ggplot2)

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
ggplot(plot_data, aes(x = Month, y = Rain, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("2026 Total" = "steelblue", "Historical Average" = "darkgrey")) +
  theme_minimal() +
  labs(
    title = "2026 Monthly Rainfall vs. Historical Baseline Average",
    x = "Month",
    y = "Rainfall Total (mm)",
    fill = "Dataset"
  )












########   Forecast  ########


library(weatherOz)
forecast <- get_metno_forecast(
  latitude = -31.95,
  longitude = 115.86,
  format = "complete",
  api_key = 'ross.searle@csiro.au'
)


forecast$data






find_forecast_towns(longitude = 121.8913, latitude = -33.8614)

get_precis_forecast(state = "AUS")


ftp_url <- 'ftp://ftp.bom.gov.au/anon/gen/fwo/WA_PT006.shtml'
download.file(url = ftp_url, destfile = 'c:/temp/f.xml', mode = "wb")
