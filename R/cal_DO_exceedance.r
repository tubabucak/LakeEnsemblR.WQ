#'@title Calculate DO exceedance

#'@description
#' Calculates the percentage time exceeded curve for O2 (similar approach to Flow Duration Curve)

#' @name cal_DO_exceedance
#' @param oxy_data {data.frame} A dataframe containing oxygen concentrations. 
#' The first column should be `datetime`, and the remaining columns should represent 
#' oxygen values at different depths.
#' @param depth {numeric} The depth at which to extract data. Must match a column name 
#' in `oxy_data` (e.g., 1.0, 1.5, etc.).

#' @return a data.frame
#' @importFrom stats na.omit



#' @export
cal_DO_exceedance <- function(oxy_data, depth = 0) {
  depth_col <- paste0("Depth_", depth)
  
  # Check if the depth exists in the data
  if (!(depth_col %in% names(oxy_data))) {
    stop("The depth column is not found in the data.")
  }
  
  # Select and clean the data
  oxy_data_sub <- na.omit(oxy_data[[depth_col]])
  
  # Sort in descending order
  oxy_data_sub <- sort(oxy_data_sub, decreasing = TRUE)
  
  # Calculate exceedance
  oxy_exceedance <- data.frame(
    exceedance = 100 * (1:length(oxy_data_sub)) / length(oxy_data_sub),
    DO = oxy_data_sub
  )
  
  return(oxy_exceedance)
}

