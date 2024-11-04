#' @title Check and Extract Selected Metrics from output.YAML
#'
#' @description
#' This function reads a YAML file containing metrics and extracts the relevant metric details based on a provided dictionary. Firstly, it checks whether each metric in the YAML file exists in the dictionary and, then extracts the necessary information needed for calculating the metric.
#'
#' @param metric_yaml_file Character: Name of the YAML file containing the list of metrics.
#' @param folder Character: The main filepath where the YAML file is located.
#' @param dict_file Character: Full path of the metric dictionary
#'

#' @return
#' A data frame containing the extracted metric details which will be used to extract the data from netcdf files (using get_output_wq & create_netcdf_wq)
#'
#' @importFrom yaml yaml.load_file
#' @importFrom dplyr filter select
#' @export
# check_the_metrics <- function(metric_yaml_file, folder, dict_file) {
  
#   # Import the dictionary file (both comma and semicolon should work)
#   dict <- tryCatch({
#     read.csv(dict_file, sep = ifelse(grepl(";", readLines(dict_file, n = 1)), ";", ","))
#   }, error = function(e) {
#     stop("Error reading the dictionary CSV file: ", dict_file)
#   })
#   # Load the YAML file
#   yaml_path <- file.path(folder, metric_yaml_file)
  
#   config <- tryCatch({
#     yaml::yaml.load_file(yaml_path)
#   }, error = function(e) {
#     stop("Error reading YAML file: ", e$message)
#   })
  
# # Empty dataframe to store the extracted metric table
# extracted_metrics <- data.frame(
#     metric_name = character(),
#     model = character(),
#     variable_model_name = character(),
#     conversion_factor = numeric(),
#     depth_01 = numeric(),
#     stringsAsFactors = FALSE
#   )

#   # Extract selected metrics from the YAML file
#   all_vars <- names(unlist(config, recursive = TRUE, use.names = FALSE))

#   all_metrics <- dict$metric_name

#   # Check if each variable in variable_names is included
#   for (metric in all_vars) {
#     if (metric %in% all_metrics) {
#        # Extract the necessary columns from the dictionary
#       metric_details <- dict %>%
#         dplyr::filter(metric_name == metric) %>%
#         dplyr::select(metric_name,model, variable_model_name, depth_01, conversion_factor, function_name)
        
#       extracted_metrics <- rbind(extracted_metrics, metric_details)
#     }

#      else {
#       cat(metric, "Non-valid metric. Please check the dictionary!\n")
#     }
#   }
#   # Extracted metrics data frame that we will use to create the netcdf file
#   return(extracted_metrics)
# }
check_the_metrics<- function(metric_yaml_file, folder, dict_file) {
  
  # Import the dictionary file (both comma and semicolon should work)
  dict <- tryCatch({
    read.csv(dict_file, sep = ifelse(grepl(";", readLines(dict_file, n = 1)), ";", ","), stringsAsFactors = FALSE)
  }, error = function(e) {
    stop("Error reading the dictionary CSV file: ", dict_file)
  })
  
  # Load the YAML file
  yaml_path <- file.path(folder, metric_yaml_file)
  
  config <- tryCatch({
    yaml::yaml.load_file(yaml_path)
  }, error = function(e) {
    stop("Error reading YAML file: ", e$message)
  })
  
  # Initialize an empty dataframe to store the extracted metric table
  extracted_metrics <- data.frame(
    level = character(),
    module = character(),
    metric_name = character(),
    model = character(),
    variable_model_name = character(),
    conversion_factor = numeric(),
    function_name = character(),
    depth_01 = numeric(),
    arguments = I(list()),  # List column to store arguments
    stringsAsFactors = FALSE
  )
  
  # Iterate over each level in the YAML file
  for (level in names(config)) {
    modules <- config[[level]]
    
    # Iterate over each module within the current level
    for (module in names(modules)) {
      metrics <- modules[[module]]
      
      # Iterate over each metric in the current module
      for (metric_name in names(metrics)) {
        metric_value <- metrics[[metric_name]]
        
        # Check if the metric has arguments (it will be a list if it does)
        if (is.list(metric_value)) {
          arguments <- metric_value
        } else {
          arguments <- list()  # No arguments, so it's just an empty list
        }
        
        # Match the current level, module, and metric_name with the dictionary
        metric_in_dict <- dict %>%
          dplyr::filter(level == !!level, module == !!module, metric_name == !!metric_name)
        
        if (nrow(metric_in_dict) > 0) {
          # If the metric exists in the dictionary, append it to the extracted metrics
          metric_details <- metric_in_dict %>%
            dplyr::select(level, module, metric_name, model, variable_model_name, depth_01, conversion_factor, function_name)
          
          # Add the arguments as a list column
          metric_details$arguments <- I(list(arguments))
          
          extracted_metrics <- rbind(extracted_metrics, metric_details)
        } else {
          cat(paste(metric_name, "in", level, "->", module, "is not valid. Please check the dictionary!\n"))
        }
      }
    }
  }
  
  # Return the final extracted metrics dataframe with arguments
  return(extracted_metrics)
}
