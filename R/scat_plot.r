#'@title Scatter plot modeled vs observed
#'@description
#' This function plots the observed versus predicted across depths and annotate KGE and RMSE on the graph
#' @name scat_plot
#' @param temp_glm Data frame: Modeled values from the GLM model in wide format with columns named "datetime" and "Depth_*".
#' @param temp_wet Data frame: Modeled values from the WET model in wide format with columns named "datetime" and "Depth_*".
#' @param temp_selma Data frame: Modeled values from the SELMAPROTBAS model in wide format with columns named "datetime" and "Depth_*".
#' @param temp_avg Data frame: Averaged modeled values across models in wide format with columns named "datetime" and "Depth_*".
#' @param temp_avg_pareto Data frame: Pareto-optimized averaged modeled values in wide format with columns named "datetime" and "Depth_*".
#' @param temp_obs Data frame: Observed values in wide format with columns named "datetime" and "Depth_*".
#' @param y_title Character: A string to label the y-axis (e.g., "Temperature (°C)", "DO (mg/L)").
#'
#' @return A list containing:
#' \itemize{
#'   \item A ggplot2 object with the observed vs predicted scatter plot and model statistics annotated.
#'   \item A named list of statistics (RMSE and KGE) for each model.
#' }
#'
#' @import ggplot2
#' @import dplyr
#' @import purrr
#' @importFrom tidyr pivot_longer
#' @importFrom hydroGOF KGE
#' @importFrom ggtext element_markdown
#' @export
#' 
#' 
scat_plot <- function(temp_glm, temp_wet, temp_selma, temp_avg, temp_avg_pareto, temp_obs, y_title) {
  
  # Helper: convert data to long format with Model label
  get_model_long <- function(data, model_name) {
    data %>%
      pivot_longer(cols = starts_with("Depth_"), names_to = "Depth", values_to = "Value") %>%
      mutate(Depth = gsub("Depth_", "", Depth),
             Model = model_name)
  }
  
  # Convert each dataset to long format
  GLM_data <- get_model_long(temp_glm, "GLM")
  WET_data <- get_model_long(temp_wet, "WET")
  Selma_data <- get_model_long(temp_selma, "SELMAPROTBAS")
  Avg_data <- get_model_long(temp_avg, "AVERAGE")
  Avg_pareto_data <- get_model_long(temp_avg_pareto, "AVG_PARETO")
  Obs_data <- get_model_long(temp_obs, "OBSERVED")
  
  # Join observed with model data
  join_obs <- function(model_data) {
    inner_join(model_data, Obs_data, by = c("datetime", "Depth"), suffix = c("_pred", "_obs")) %>%
      rename(Predicted = Value_pred, Observed = Value_obs)
  }
  
  df_glm <- join_obs(GLM_data) %>% mutate(Model = "GLM")
  df_wet <- join_obs(WET_data) %>% mutate(Model = "WET")
  df_selma <- join_obs(Selma_data) %>% mutate(Model = "SELMAPROTBAS")
  df_avg <- join_obs(Avg_data) %>% mutate(Model = "AVERAGE")
  df_avg_pareto <- join_obs(Avg_pareto_data) %>% mutate(Model = "AVG_PARETO")
  
  all_data <- bind_rows(df_glm, df_wet, df_selma, df_avg, df_avg_pareto)
  
  # Compute stats
  stats_list <- list(
    GLM = cal_stats(df_glm$Observed, df_glm$Predicted),
    WET = cal_stats(df_wet$Observed, df_wet$Predicted),
    SELMAPROTBAS = cal_stats(df_selma$Observed, df_selma$Predicted),
    AVERAGE = cal_stats(df_avg$Observed, df_avg$Predicted),
    AVG_PARETO = cal_stats(df_avg_pareto$Observed, df_avg_pareto$Predicted)
  )
  
  
  stat_chunks <- purrr::imap_chr(stats_list, function(stats, model) {
    paste0("**", model, "**: KGE=", round(stats$KGE, 2), ", RMSE=", round(stats$RMSE, 2))
  })
  
  # Split into two lines (e.g. first 3 models on line 1, rest on line 2)
  stat_line <- paste(
    paste(stat_chunks[1:3], collapse = " &nbsp; | &nbsp; "),
    paste(stat_chunks[4:5], collapse = " &nbsp; | &nbsp; "),
    sep = "<br>"
  )
  
  # Final scatter plot
  scatter_plot <- ggplot(all_data, aes(x = Observed, y = Predicted, color = Model)) +
    geom_point(alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = "Observed vs Predicted (All Depths)",
      subtitle = stat_line,
      x = paste("Observed", y_title),
      y = paste("Predicted", y_title)
    ) +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      plot.title = element_text(size = 11, face = "bold", hjust = 0),
      plot.subtitle = ggtext::element_markdown(size = 8, color = "gray30", hjust = 0)
    )
  
  return(list(scatter_plot, stats_list))
}




# all_plots <- purrr::imap(output_filtered, function(model_data, var_name) {
  
  
#   temp_glm <- model_data$GLM
#   temp_wet <- model_data$WET
#   temp_selma <- model_data$SELMAPROTBAS
#   temp_avg <- model_data$Average
#   temp_pareto <- model_data$Avg_pareto
  
  
#   temp_obs <- obs_data[[var_name]]
  
  
#   scat_plot(temp_glm, temp_wet, temp_selma, temp_avg, temp_pareto, temp_obs, var_name)
# })



# purrr::iwalk(all_plots, function(plot_obj, var_name) {
#   ggsave(
#     filename = file.path(getwd(), paste0(var_name, ".jpeg")),
#     plot = plot_obj[[1]]  # the ggplot object
#   #  width = 800, height = 600, units = "px", dpi = 300
#   )
# })




















# 
# 
# 
# 
# 
# 
# 
# 
#   output_filtered <- output_filtered[names(output_filtered) != "pH"]
# 
#   all_plots <- purrr::imap(output_filtered, function(model_data, var_name) {
#     
#    
#     temp_glm <- model_data$GLM
#     temp_wet <- model_data$WET
#     temp_selma <- model_data$SELMAPROTBAS
#     temp_avg <- model_data$Average
#     temp_pareto <- model_data$Avg_pareto
#     
#     
#     temp_obs <- obs_data[[var_name]]
#     
#    
#     scat_plot(temp_glm, temp_wet, temp_selma, temp_avg, temp_pareto, temp_obs, var_name)
#   })