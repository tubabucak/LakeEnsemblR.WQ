#'@title Calculate Stratified Days

#'@description
#' Calculate the starting date of spring/summer stratification,
#' Duration of stratification
#' Start date of mixing

#' @name cal_strat_date
#' @param temp_data dataframe; The output of the model/observed output. It should include Datetime and depth specific values per each column.
#' @param hemisphere character; N for Northern Hemisphere, S for Southern hemisphere

#' @return dataframe with starting date of stratification, start date of mixing and duration of stratification
#' @importFrom rLakeAnalyzer ts.thermo.depth

#' @import tidyverse
#' @import lubridate

#' @export
cal_strat_date <- function(temp_data, hemisphere = "N") {
  
  # Calculate the average temperature (ignoring the first column since its datetime column by default)
  temp_avg <- rowMeans(temp_data[,-1], na.rm = TRUE)
  
  # Calculate the thermo depth by rlakeAnalyzer's ts.thermo.depth function
  thermo_depth <- ts.thermo.depth(temp_data)
  
  # Merge the average temperature, thermo depth, and datetime
  temp_data_merged <- data.frame(Date = temp_data[,1], temp_avg = temp_avg, thermo.depth = thermo_depth$thermo.depth)
  
  # Extract the year from the Date column
  temp_data_merged$Year <- year(temp_data_merged$Date)
  
  # Extract the month from the Date column
  temp_data_merged$Month <- month(temp_data_merged$Date)
  # Correction for Southern hemisphere. We assume that the year starts in July

  temp_data_merged<- temp_data_merged %>%
  
    mutate(Year_upd = ifelse(Month>6, Year, Year-1))
  # Initialize a list to store results for each year to return
  results_list <- list()
  
  # Get unique years from the dataset
  uniq_year <- unique(temp_data_merged$Year)
  
  # Loop through each unique year
  for (i in 1:length(uniq_year)) {
    # Subset data for the current year and filter for temp_avg > 4 and NOT NaN values (indicating termocline exists)
    if (hemisphere == "N"){
    temp_data_sub <- temp_data_merged %>%
      filter(Year == uniq_year[i]) %>%
      filter(temp_avg >= 4.00)
    }
    else if (hemisphere == "S"){
    temp_data_sub <- temp_data_merged %>%
      filter(Year_upd == uniq_year[i]) %>%
      filter(temp_avg >= 4.00)

    }
    # Extract rows where thermo_depth is NOT NaN
    temp_data_nan <- temp_data_sub %>%
      filter(!is.nan(thermo.depth))
    
    # If NOT NaNs exist, calculate the first day, consecutive NOT NaN days (which are stratified), and the final stratification date
    if (nrow(temp_data_nan) > 0) {
      # Extract the first spring/summer stratification date
      first_nan_date <- min(temp_data_nan$Date)
      
      rle_consecutive <- rle(as.integer(diff(as.Date(temp_data_nan$Date)) == 1))
      consecutive_lengths <- rle_consecutive$lengths[rle_consecutive$values == 1]
      num_consecutive_nans <- ifelse(length(consecutive_lengths) > 0, max(consecutive_lengths) + 1, 1)

      # Identify the last stratified days
      final_nan_date <- max(temp_data_nan$Date)
      
      results_list[[i]] <- list(
        Year = uniq_year[i],
        Strat_Start_Date = first_nan_date,
        Consecutive_Strat_Days = num_consecutive_nans,
        Mixing_Start_Date = final_nan_date + 1
      )
    }
  }
  
  # Combine the results into a data frame
  result_df <- do.call(rbind, lapply(results_list, as.data.frame))
  
  return(result_df)
}


