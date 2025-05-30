#'@title Calculate Epilimnion Thickness

#'@description
#' Calculate the epilimnion thickness (in meter) from the temperature data

#' @name cal_epi_depth

#' @param temp_data dataframe; The output of the model/observed temperature data. It should include Datetime and depth-specific temperature values in each column.
#' @param folder character; Optional folder path for saving results (default is '.').
#' 
#' @return A dataframe with Datetime and epilimnion thickness.
#' @importFrom rLakeAnalyzer ts.meta.depths
#' @importFrom rLakeAnalyzer get.offsets
#' @importFrom dplyr mutate


#' @export

  cal_epi_depth <- function(temp_data, folder = '.') {
    

    max_depth <- tail(get.offsets(temp_data),)
    # calculation the top and bottom metalimnion depths
    meta <- ts.meta.depths(temp_data)

    # Replace the NaN"s with maximum depth
    meta<- meta %>%
    mutate_at(vars(top), ~ifelse(is.nan(.), max_depth, .))


    epi_mixed_depth <- abs(0-meta$top)

    # Create a new dataframe with datetime and the epilimnion thickness
    result <- data.frame(datetime = temp_data$datetime, epi_thickness= epi_mixed_depth)
    
    return(result)
  }
  
