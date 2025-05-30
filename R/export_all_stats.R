#'@title Calculate & export performance metrics for integrated samples - so far only for specific depths
#'@description
#' This function integrates a selected variable (e.g., DO, temperature) across specified depths for each model output,
#' compares it to observed data, and returns summary statistics.
#' @name integrate_depths
#' @param output List: A list containing model output data for different models at different depths.
#' @param variable Character: Variable name to be extracted (e.g., "DO_mgL", "temp").
#' @param depths Numeric vector: One or more depths to extract or integrate over. But so far it just works with single vector output
#' @param bathy_path Character: File path to the bathymetry data.
#' @param obs_data Data frame: Observed data to compare model output against.

#' @return A data frame of calculated statistics (e.g., RMSE, KGE) for each model, depth, and variable.
#'
#' @import dplyr
#' @importFrom zoo na.approx
#' @importFrom hydroGOF KGE
#' @export

export_all_stats <- function(output, variable, depths, bathy_path, obs_data) {
  
  results_list <- list()
  
  # For now it just works with single depth option - will be updated
  for (d in depths) {
    

    merged_df <- integrate_depths(
      output = output,
      depth = d,
      variable = variable,
      bathy_path = bathy_path,
      obs_data = obs_data,
      hypso_avg= FALSE
    )
    

    obs_series <- merged_df %>% 
      filter(Model == "Observations") %>%
      arrange(datetime)
    
    # Skip if there is no observed data for this depth
    if (nrow(obs_series) == 0 || all(is.na(obs_series[, 2]))) {
      message("No observed data at depth ", d, " for variable ", variable, ". Skipping.")
      next  # skip
    }
    
    
    model_names <- unique(merged_df$Model)
    model_names <- model_names[model_names != "Observations"]
    
    for (model in model_names) {
     # print(model)
      mod_series <- merged_df %>% 
        filter(Model == model) %>%
        arrange(datetime)
      
      # Matching dates obs vs simulated
      common_dates <- intersect(obs_series$datetime, mod_series$datetime)
      obs_vals <- obs_series %>%
        filter(datetime %in% common_dates) %>%
        pull(2)
    #  print(length(obs_vals))
      mod_vals <- mod_series %>% 
        filter(datetime %in% common_dates) %>%
        pull(2)
    #  print(length(mod_vals))
      # Calculate stats
      stat <- cal_stats(obs_vals, mod_vals)
      
      # remove residuals
      stat_filtered <- stat[setdiff(names(stat), "residual")]
      
      stat_df <- data.frame(
        Variable = variable,
        Depth = d,
        Model = model,
        Metric = names(stat_filtered),
        Value = unlist(stat_filtered),
        Count = sum(!is.na(obs_vals)) 
      )
      
      
      results_list[[length(results_list) + 1]] <- stat_df
    }
  }
  
  # Combine all results
  final_df <- bind_rows(results_list)
  return(final_df)
}


