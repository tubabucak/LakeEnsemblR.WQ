#'@title Comparing observed/predicted data and calculate the stats

#'@description
#' The aim is to plot observed vs predicted and calculate statistical measures for each model. So far we have GLM, WET and SELMAPROTBAS

#' @name compare_plot
#' @param data_glm, dataframe; Timeseries output of GLM model for a certain variable where each depth should be in different columns (starting with "Depth_0", "Depth_0.5" etc) and the first column should be datetime
#' @param data_wet, dataframe; Timeseries output of WET model for a certain variable where each depth should be in different columns (starting with "Depth_0", "Depth_0.5" etc) and the first column should be datetime
#' @param data_selma, dataframe; Timeseries output of SELMAPROTBAS model for a certain variable where each depth should be in different columns (starting with "Depth_0", "Depth_0.5" etc) and the first column should be datetime
#' @param data_OBS, dataframe; Timeseries output of OBSERVED data for a certain variable where each depth should be in different columns (starting with "Depth_0", "Depth_0.5" etc) and the first column should be datetime
#' @param depth, numeric; Certain depth to be compared

#' @param y_title, character; Name of the variable to be shown in the plot
#'
#' @return A list with comparison plot with statistical metrics. order: plot, stats_glm, stats_wet, stats_selma
#'
#' @importFrom ggplot2 ggplot
#' @importFrom dplyr filter select rename mutate bind_rows
#' @examples
#' Example usage:
#' cal_stats(Observed, Predicted)


#' @export


# Function to create the data for each depth and plot
compare_plot <- function(data_glm, data_wet, data_selma, data_obs, depth, y_title) {

  # Select the relevant depth column dynamically
  GLM_data <- data_glm %>%
    select(datetime, !!sym(paste0("Depth_", depth))) %>%
    rename(Value = !!sym(paste0("Depth_", depth))) %>%
    mutate(Model = "GLM")

  WET_data <- data_wet %>%
    select(datetime, !!sym(paste0("Depth_", depth))) %>%
    rename(Value = !!sym(paste0("Depth_", depth))) %>%
    mutate(Model = "WET")

  Selma_data <- data_selma %>%
    select(datetime, !!sym(paste0("Depth_", depth))) %>%
    rename(Value = !!sym(paste0("Depth_", depth))) %>%
    mutate(Model = "SELMAPROTBAS")

  Obs_data <- data_obs %>%
    select(datetime, !!sym(paste0("Depth_", depth))) %>%
    rename(Value = !!sym(paste0("Depth_", depth))) %>%
    mutate(Model = "OBSERVED")

  # Combine all data
  data_all <- bind_rows(GLM_data, WET_data, Selma_data, Obs_data)


  # Extract the points for statistics
  observed_dates <- data_all %>%
  filter(Model == "OBSERVED") %>%
  pull(datetime) # Extract observed dates

  data_all_obsdates <- data_all %>%
  filter(datetime %in% observed_dates)


  data_all_obsdates_wide <- spread(data_all_obsdates, key = Model, value = Value)

 stats_glm <-  cal_stats(data_all_obsdates_wide$OBSERVED, data_all_obsdates_wide$GLM)
 stats_wet <-  cal_stats(data_all_obsdates_wide$OBSERVED, data_all_obsdates_wide$WET)
 stats_selma <-   cal_stats(data_all_obsdates_wide$OBSERVED, data_all_obsdates_wide$SELMAPROTBAS)

  # Create plot
  data_plot <- ggplot(data_all, aes(x = datetime, y = Value, color = Model, group = Model)) +
    geom_line(data = data_all[data_all$Model != "OBSERVED",], aes(linetype = Model)) +  # Line plot for GLM, WET, and SELMA
    geom_point(data = data_all[data_all$Model == "OBSERVED",], aes(shape = Model), col = "black") +  # Points for observed data
    labs(title = paste0("Depth: ", depth, " m"),
         x = "Date",
         y = y_title) +
    theme(legend.title = element_blank())

  return(list(data_plot, stats_glm, stats_wet, stats_selma))
}
