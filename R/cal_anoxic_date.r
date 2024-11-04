#'@title Calculate Anoxic Days

#'@description
#' Calculates Number of Anoxic Days and Anoxic Factors (Nürnberg, 1995) for each year/season

#' @name cal_anoxic_date
#' @param bathy_file, dataframe; The file includes depth and area relationship
#' @param oxy_data, dataframe; The output of the model/observed output. It should include Datetime and depth specific values per each column.
#' @param threshold, anoxic O2 threshold (mg L-1)
#' @param duration, argument for calculating the anoxic factor and number of anoxic days. If duration = full, the number of all anoxic days will be calculated. If duration = longest, AF for the longest anoxic period will be returned.
#' @return dataframe with number of anoxic days for each year and anoxic factor. Also returns the depth for each day where anoxic layer starts
#' @importFrom rLakeAnalyzer ts.meta.depths


#' @export
cal_anoxic_date <- function(oxy_data, bathy_file, threshold = 1, duration = "full"){

  bathy_file$depths <- as.numeric(bathy_file$depths)
  bathy_file$areas <- as.numeric(bathy_file$areas)
# Create an empty vector to store the results
  anoxic_depths <- rep(NA, nrow(oxy_data))
  area_anoxic <- rep(0, nrow(oxy_data))
  AF_total <- numeric(0)
  num_anoxic_days <- numeric(0)

# Extract surface area of the lake
surface_area <- bathy_file$areas[1] 

for (i in 1:nrow(oxy_data)) {

  # Subset the row and exclude the datetime column
  row_values <- oxy_data[i, -1]
  
  # Find the first depth where the value is below the threshold
  below_threshold <- which(row_values < threshold)
  
  # If there is any value below the threshold, extract the depth
  if (length(below_threshold) > 0) {

    first_depth <- names(row_values)[below_threshold[1]]
    first_depth <- as.numeric(gsub("[^0-9.]", "", first_depth))

    if( first_depth %in% bathy_file$depths){
      area <- bathy_file$areas[which(bathy_file$depths == first_depth)]
      anoxic_depths[[i]] <- first_depth
      area_anoxic[[i]]<- area
    }
    else {
      # Apply piecewise linear interpolation to estimate the area for specific depth
      lower_depth <- max(bathy_file$depths[bathy_file$depths < first_depth], na.rm = TRUE)
      upper_depth <- min(bathy_file$depths[bathy_file$depths > first_depth], na.rm = TRUE)

      # Get the corresponding areas for the specific date
      lower_area <- bathy_file$areas[which(bathy_file$depths == lower_depth)]
      upper_area <- bathy_file$areas[which(bathy_file$depths == upper_depth)]
      interpolated_area <- lower_area + (upper_area - lower_area) * ((first_depth - lower_depth) / (upper_depth - lower_depth))
      anoxic_depths[[i]] <- first_depth
      area_anoxic[[i]]<- interpolated_area
    }
  } 
  else {
    anoxic_depths[[i]] <- NA  
    area_anoxic[[i]]<- 0
  }
}

# Add the results as a new column in the data frame

oxy_data$area_anoxic <- area_anoxic

oxy_data$Year <- year(oxy_data$datetime)
Years <- unique(oxy_data$Year)


for (i in 1:length(Years)) {

    oxy_data_sub <- oxy_data %>%
    filter(Year == Years[i])

    # New column for anoxic days
    oxy_data_sub$is_anoxic <- oxy_data_sub$area_anoxic > 0
    
    # Identifying consecutive anoxic days
    rle_anoxic <- rle(oxy_data_sub$is_anoxic)
    
    if (any(rle_anoxic$values == TRUE)) {
      # Find the longest anoxic period
      longest_anoxic_index <- which.max(rle_anoxic$lengths * rle_anoxic$values)
      longest_anoxic_days <- rle_anoxic$lengths[longest_anoxic_index]
      
      # Calculate the sum of area_anoxic for the longest period
      start_index <- ifelse(longest_anoxic_index > 1, 
                            sum(rle_anoxic$lengths[1:(longest_anoxic_index - 1)]) + 1, 
                            1)
      end_index <- start_index + longest_anoxic_days - 1
      longest_anoxic_area <- sum(oxy_data_sub$area_anoxic[start_index:end_index], na.rm = TRUE)
    } else {
      longest_anoxic_days <- 0
      longest_anoxic_area <- 0
    }

    # Calculate the sum of area for the full period
    area_anoxic_sum <- sum(oxy_data_sub$area_anoxic, na.rm = TRUE)

    if (duration == "full"){ 
    # AF for full period
    AF <- area_anoxic_sum/surface_area
    # Calculate Number of anoxic days
    num_anoxic<- length( which(oxy_data_sub$area_anoxic>0) )
    }
    else if (duration == "longest"){ 
    # AF for the longest period
    AF <- longest_anoxic_area/surface_area
    num_anoxic <- longest_anoxic_days
    }
    else{
      stop("duration argument is missing!")
    }

    AF_total[i]<- AF

    num_anoxic_days[i]<- num_anoxic

    }
    AF_yearly<- data.frame(Years, AF_total)
    num_anoxic_days<- data.frame(Years, num_anoxic_days)
    anoxic_depths <- data.frame(oxy_data$datetime, anoxic_depths )
  
 return(list(AF_yearly = AF_yearly, num_anoxic_days = num_anoxic_days, anoxic_depths = anoxic_depths))

  }

# Example usage   
# anoxic_results <- cal_anoxic_date(oxy_wet, bathy_file, threshold = 1, duration= "longest")
