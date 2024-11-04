#'@title Calculate surface-bottom temperature difference

#'@description
#' Calculate the temperature difference between bottom and surface of the lake.
#' Uses layer.temperature function from rlakeAnalyzer

#' @name cal_bot_surf_temp_dif 
#' @param temp_data dataframe; The output of the model/observed output. It should include Datetime and depth specific values per each column.
#' @return dataframe with Dateframe and bottom-surface difference

#' @examples
#' Example usage:
#' # temp_diff_result <- cal_bot_surf_temp_dif(temp_glm)
#' # print(temp_diff_result)
#' @export
  cal_bot_surf_temp_dif <- function(temp_data) {
    
    # Create a vector to store the differences
    temp_diff <- numeric(nrow(temp_data))
    
    # For each row
    for (i in 1:nrow(temp_data)) {
      # Extract the surface temperature (which is second column)
      surface_temp <- temp_data[i, 2]
      
      # Remove datetime
      temp_values <- temp_data[i, -1]
      
      # Keeping non NA temperatures - because of the WLF, there are NAs in the last column.
      valid_values <- na.omit(as.numeric(temp_values))
      
      # Check if we have at least one valid temperature value (besides surface)
      if (length(valid_values) > 0) {
        # Get the last valid (non-NA) temperature
        last_non_na <- tail(valid_values, 1)
        
        # Calculate the difference between the surface and the last valid bottom temperature
        temp_diff[i] <- surface_temp - last_non_na
      } else {
        # If no valid non-NA bottom value exists, set the difference to NA
        temp_diff[i] <- NA
      }
    }
    
    # Create a new dataframe with datetime and the temperature differences
    result <- data.frame(datetime = temp_data$datetime, temp_diff = temp_diff)
    
    return(result)
  }
  
