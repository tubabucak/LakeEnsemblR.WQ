#'@title Calculate Metalimnion Thickness

#'@description
#' This function calculate the metalimnion thickness using temperature data


#' @name cal_meta_depth
#' @param temp_data dataframe; The output of the model/observed output. It should include Datetime and depth specific values per each column.
#' @return dataframe with Dateframe and metalimnion thickness
#' @importFrom rLakeAnalyzer ts.meta.depths
#' @importFrom dplyr mutate_at vars
#' 

#' @export
  cal_meta_depth <- function(temp_data) {
    
    # Calculate the metalimnion depths usin rLakeAnalyzer ts.meta.depths
    meta <- ts.meta.depths(temp_data)

    # Replace the NaN"s with maximum depth
    meta<- meta %>%
    mutate_all(~ifelse(is.nan(.), 0, .))

    # Calculate the metalimnion thickness
    meta_depth <- abs(meta$top- meta$bottom)
    # Create a new dataframe with datetime and the metalimnion thickness
    result <- data.frame(datetime = temp_data$datetime, meta_thickness = meta_depth)
    
    return(result)
  }
  
