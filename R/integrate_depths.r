#'@title Integrate Model Outputs Across Specified Depths

#'@description
#' The aim of the function is to integrate the model outputs across specified depth intervals since observed water sample data are often 
#' collected as integrated samples over multiple depths. 
#' There are two options in this function:
#' 1)The user either specify an exact depth
#' 2)The user give provides a depth range as a vector (e.g., `c(lower, upper)`) 
#' 3)The user gives dataframe with dynamic lower-upper bounds for certain period. The dataframe should have columns datetime, lower, upper 
#' the function integrate the average values within these depts.

#' @name integrate_depths
#' @param output List: A list containing model output data for different models at different depths.
#' @param depth numeric or vector or dataframe: 
#' 1)if exact depth is being extracted, provide a simple numeric value,
#' 2)if integrating across depths for all time series, provide a vector c(lower, upper)
#' 3) if you want to integrate different depth intervals for different dates, a dataframe with datetime, lower, upper values should be given
#' @param variable character: variable name to be extracted
#' @param bathy_path character: variable name to be extracted
#' @param obs_data list: list of observed data
#' @param folder character: main filepath; where all the output is stored
#' @param hypso_avg logical: if TRUE= hypsometric curve (depth-area relationship) will be used, otherwise average of 
#' selected columns will be calculated
#' @param dz numeric: interpolation depth interval. The default is 0.1

#' @return A list of integrated variables for the specified depth or depth range.
#' 
#' @import dplyr
#' @importFrom zoo na.approx
#' @importFrom readr read_csv
#' @importFrom utils str
#' @export 
#' 
#' 


integrate_depths <- function(output, depth, variable, bathy_path, obs_data = NULL, folder = NULL, hypso_avg = TRUE, dz = 0.1) {
  
  # Load bathymetry file
  hyps <- read_csv(bathy_path)

 # Extract variable of interest
  metric_output <- output[[variable]]

  # Find the depth columns in the data and convert to numeric values
  depth_cols <- grep("^Depth_", colnames(metric_output[[1]]), value = TRUE)
  depth_numeric <- as.numeric(gsub("Depth_", "", depth_cols))  
  # depth_numeric <- seq(0, max(depth_numeric), dz)
  dz = mean(diff(depth_numeric))
  
  # Interpolate bathymetric areas for depths
  hyps_area <- approx(x = hyps$`Bathymetry Depths`,
                      y = hyps$`Bathymetry Areas`,
                      xout = depth_numeric, 
                      rule = 2)$y  

  # Single depth case
  if(is.numeric(depth) && length(depth) == 1) {
    exact_depth <- paste0("Depth_", depth)
    
    # Extract the column corresponding to the exact depth 
    if (exact_depth %in% colnames(metric_output[[1]])) {
      result <- lapply(metric_output, function(output) output[, c("datetime", exact_depth), drop = FALSE])
    } else {
      stop("Specified depth does not exist in the dataset.")
    }
    
    # If observation data is given
    if (!is.null(obs_data) && variable %in% names(obs_data)) {
    obs_df <- obs_data[[variable]]

      # Extract the exact depth column
      if (exact_depth %in% colnames(obs_df)) {
              
        obs_result<- obs_df[, c("datetime", exact_depth)]
 
        # Append observations to result
        result <- append(result, list(Observations = obs_result))
        print(str(result))
      }
      else {
        message(paste("No observation data available for", exact_depth))
      }
    }
    
  # Integrated column case (lower-upper bounds should be given)
  } else if (is.numeric(depth) && length(depth) == 2) {
    lower <- depth[1]
    upper <- depth[2]
    
    # Select columns within depth range
    selected_cols <- depth_cols[depth_numeric >= lower & depth_numeric <= upper]
    selected_depths <- depth_numeric[depth_numeric >= lower & depth_numeric <= upper]
    selected_areas <- hyps_area[depth_numeric >= lower & depth_numeric <= upper]
    
    if (length(selected_cols) == 0) {
      stop("No depth columns found within the specified range.")
    }
    if (hypso_avg == TRUE){
    # Depth integration with bathymetric info
    result <- lapply(metric_output, function(output) {
      output_subset <- output[, selected_cols, drop = FALSE]  # Extract relevant depths
      
      # Interpolate missing values in each row
      output_interpolated <- apply(output_subset, 1, function(row) {
        na.approx(row, x = selected_depths, rule = 2, na.rm = FALSE)  
      })
      
      # Compute the depth-integrated value using bathymetry
      integrated_values <- colSums(output_interpolated * selected_areas, na.rm = TRUE) / sum(selected_areas, na.rm = TRUE)

      
      # Return new dataframe with integrated values
      data.frame(datetime = output$datetime, Integrated = integrated_values)
    })
}


if (hypso_avg == FALSE){
    # Depth integration with bathymetric info
    result <- lapply(metric_output, function(output) {
      output_subset <- output[, selected_cols, drop = FALSE]  # Extract relevant depths
      
      # Interpolate missing values in each row
      output_interpolated <- apply(output_subset, 1, function(row) {
        na.approx(row, x = selected_depths, rule = 2, na.rm = FALSE)  
      })
      
      # Compute the depth-integrated value using bathymetry
      integrated_values <- colMeans(output_interpolated * selected_areas, na.rm = TRUE)
      
      # Return new dataframe with integrated values
      data.frame(datetime = output$datetime, Integrated = integrated_values)
    })
}
  if (!is.null(obs_data) && variable %in% names(obs_data)) {
    obs_df <- obs_data[[variable]]
    
    # Remove full NA columns
    obs_df <- obs_df[, colSums(is.na(obs_df)) < nrow(obs_df), drop = FALSE]

    obs_depth_cols <- grep("^Depth_", colnames(obs_df), value = TRUE)

    # Extract numeric depth values and also sort 
    obs_depth_numeric <- as.numeric(gsub("Depth_", "", obs_depth_cols))  
    obs_depth_cols <- obs_depth_cols[order(obs_depth_numeric)]  
    obs_depth_numeric <- sort(obs_depth_numeric) 

    # Select data of observation depths
    selected_obs_cols <- obs_depth_cols[obs_depth_numeric >= lower & obs_depth_numeric <= upper]
    selected_obs_depths <- obs_depth_numeric[obs_depth_numeric >= lower & obs_depth_numeric <= upper]
 
    if (length(selected_obs_cols) > 0) {
      obs_subset <- obs_df[, selected_obs_cols, drop = FALSE]
      
      # Interpolate missing values
      obs_interpolated <- apply(obs_subset, 1, function(row) {
        na.approx(row, x = selected_obs_depths, rule = 2, na.rm = FALSE)  
      })
      
    selected_obs_areas <- approx(x = hyps$`Bathymetry Depths`,
        y = hyps$`Bathymetry Areas`,
        xout = selected_obs_depths,
        rule = 2)$y 


      # Compute the depth-integrated value using bathymetry relationship
      obs_integrated_values <- colSums(obs_interpolated * selected_obs_areas, na.rm = TRUE) / sum(selected_obs_areas, na.rm = TRUE)
      
      # Create a data frame
      obs_result <- data.frame(datetime = obs_df$datetime, Integrated = obs_integrated_values)
      
      # Append observations to the result list
      result <- append(result, list(Observations = obs_result))
    print(str(result))
   }
  } 

  } 
# Data.frame case
else if (is.data.frame(depth) && all(c("datetime", "lower", "upper") %in% colnames(depth))) {

  result <- lapply(metric_output, function(output) {
    model_results <- list()

    for (i in 1:nrow(depth)) {
      dt <- as.POSIXct(depth$datetime[i], tz = "GMT")
      lower <- depth$lower[i]
      upper <- depth$upper[i]   

      output_dt <- output[output$datetime == dt, , drop = FALSE]
      if (nrow(output_dt) == 0) next  

      selected_cols <- depth_cols[depth_numeric >= lower & depth_numeric <= upper]
      selected_depths <- depth_numeric[depth_numeric >= lower & depth_numeric <= upper]
      selected_areas <- hyps_area[depth_numeric >= lower & depth_numeric <= upper]

      if (length(selected_cols) == 0) next  

      output_subset <- output_dt[, selected_cols, drop = FALSE]

      output_interpolated <- apply(output_subset, 1, function(row) {
        na.approx(row, x = selected_depths, rule = 2, na.rm = FALSE)  
      })

      integrated_values <- if (hypso_avg) {
        colSums(output_interpolated * selected_areas, na.rm = TRUE) / sum(selected_areas, na.rm = TRUE)
      } else {
        colMeans(output_interpolated, na.rm = TRUE)
      }

      model_results[[i]] <- data.frame(datetime = dt, Integrated = integrated_values)
    }

    do.call(rbind, model_results)
  })

  # Observation Data
  if (!is.null(obs_data) && variable %in% names(obs_data)) {
    obs_df <- obs_data[[variable]]
    
    # Remove full NA columns
    obs_df <- obs_df[, colSums(is.na(obs_df)) < nrow(obs_df), drop = FALSE]

    obs_depth_cols <- grep("^Depth_", colnames(obs_df), value = TRUE)
    obs_depth_numeric <- as.numeric(gsub("Depth_", "", obs_depth_cols))  
    obs_depth_cols <- obs_depth_cols[order(obs_depth_numeric)]  
    obs_depth_numeric <- sort(obs_depth_numeric)

    obs_results <- list()

    for (i in 1:nrow(depth)) {
      dt <- as.POSIXct(depth$datetime[i], tz = "GMT")
      lower <- depth$lower[i]
      upper <- depth$upper[i]   

      # **Filter observation data correctly**
      obs_df_dt <- obs_df[as.POSIXct(obs_df$datetime, tz = "GMT") == dt, , drop = FALSE]
      if (nrow(obs_df_dt) == 0) next  

      selected_obs_cols <- obs_depth_cols[obs_depth_numeric >= lower & obs_depth_numeric <= upper]
      selected_obs_depths <- obs_depth_numeric[obs_depth_numeric >= lower & obs_depth_numeric <= upper]

      if (length(selected_obs_cols) > 0) {
        obs_subset <- obs_df_dt[, selected_obs_cols, drop = FALSE]

        # **Interpolate missing values**
        obs_interpolated <- apply(obs_subset, 1, function(row) {
          na.approx(row, x = selected_obs_depths, rule = 2, na.rm = FALSE)  
        })

        selected_obs_areas <- approx(x = hyps$`Bathymetry Depths`,
                                     y = hyps$`Bathymetry Areas`,
                                     xout = selected_obs_depths,
                                     rule = 2)$y  

        # Compute depth-integrated values
        obs_integrated_values <- if (hypso_avg) {
          colSums(obs_interpolated * selected_obs_areas, na.rm = TRUE) / sum(selected_obs_areas, na.rm = TRUE)
        } else {
          colMeans(obs_interpolated * selected_obs_areas, na.rm = TRUE)
        }

        obs_results[[i]] <- data.frame(datetime = dt, Integrated = obs_integrated_values)
      }
    }

    if (length(obs_results) > 0) {
      obs_results <- do.call(rbind, obs_results)
      result <- append(result, list(Observations = obs_results))  # **Append Observations to Result**
    }
   print(str(result))
  }

}

   else {
    stop("Invalid depth input. Please provide a numeric value, a vector c(lower, upper), or a dataframe with 'datetime', 'lower', and 'upper' columns.")
  }

     # Convert list to dataframe -first add Model Column
    result_add <- lapply(names(result), function(model) {
    df <- result[[model]]
    df$Model <- model
    return(df)  
    })
   # Convert to dataframe for easy plotting
    result_df <- do.call(rbind, result_add)
    
    return(result_df)
}








