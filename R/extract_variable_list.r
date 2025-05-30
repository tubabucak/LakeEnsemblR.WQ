#'@title Create variable list for calculating the metrics

#'@description
#' The aim is to extract & apply the necessary unit conversions the list of variables data that is selected in output.yaml. 

#' @name extract_variable_list
#' @param extracted_metric_dict character: full filepath for the metric dictionary.
#' @param config_file character:filepath; To Output.yaml
#' @param model_filter character: name of the model to be extracted (GLM, SELMAPROTBAS, WET). If all model outputs, it should be set to model= "all"
#' @return A list of extractedf variables for each model and for each metric defined in output.yaml
#' 
#' @importFrom utils read.csv
#' @importFrom ncdf4 nc_open nc_close
#' @importFrom dplyr filter
#' @examples

#' @export



extract_variable_list <- function(extracted_metric_dict, config_file, model_filter = "all") {

 cfg <- load_config(config_file)
  # cfg$model_folders$GLM, cfg$model_folders$WET, cfg$model_folders$SELMAPROTBAS

    # Empty list to store the results for each metric
    start.time <- Sys.time()
    extracted_data <- list()
    
    # Cache to store already extracted model-variable combinations
    cache <- list()
    
   # Filter model if not "all"
    if (model_filter != "all") {
        extracted_metric_dict <- extracted_metric_dict %>%
            dplyr::filter(model %in% model_filter)
    }

    # Loop through the metrics dictionary
    for (i in 1:nrow(extracted_metric_dict)) {
        # Extract metric details
        metric_name <- extracted_metric_dict$metric_name[i]
        model_name <- extracted_metric_dict$model[i]
        variable_model_name <- extracted_metric_dict$variable_model_name[i]
        depth_01 <- extracted_metric_dict$depth_01[i]
        conversion_factor <- extracted_metric_dict$conversion_factor[i]
        
        # Determine the file path based on the model
        nc_file <- file.path(cfg$model_folders[[model_name]], "Output", "Output.nc")

        # cfg_dir <- dirname(config_file)
        # model_dir <- file.path(cfg_dir, cfg$model_folders[[model_name]])
        # nc_file <- file.path(model_dir, "Output", "Output.nc")

        if (!file.exists(nc_file)) {
            cat("NetCDF file for model", model_name, "not found at:", nc_file, "\n")
            next
        }
        
        # Split and trim variable names
        variable_list <- strsplit(variable_model_name, ",")[[1]]
        
        for (var_name in variable_list) {
            var_name <- trimws(var_name)  # Remove any whitespace
            
            # Define a cache key based only on model_name and var_name to store the already extracted ones
            cache_key <- paste0(model_name, "_", var_name)
            
            # Check if the model-variable combination is already in the cache
            if (cache_key %in% names(cache)) {
                cat("Using cached data for model", model_name, "variable", var_name, "\n")
                metric_data <- cache[[cache_key]]
            } else {
                cat("Extracting data for model", model_name, "using variable", var_name, "\n")
                
                # Open the NetCDF file and extract data
                nc <- tryCatch({
                    ncdf4::nc_open(nc_file)
                }, error = function(e) {
                    cat("Error opening NetCDF file:", nc_file, "\n")
                    
                    return(NULL)
                })
                
                if (!is.null(nc)) {
                    metric_data <- tryCatch({
                        get_output_wq(config_file = cfg$metric_yaml_file, model = model_name, vars = var_name, depth_01 = depth_01, conversion_factor = conversion_factor)
                    }, error = function(e) {
                        cat("Error extracting data for variable:", var_name, "\n")
                        print(e)  # Print the full error message
                        return(NULL)
                    })
                    
                    # Close the NetCDF file
                    ncdf4::nc_close(nc)
                    
                    # Cache the extracted data based on the model-variable combination
                    if (!is.null(metric_data)) {
                        cache[[cache_key]] <- metric_data
                    }
                }
            }
            
            # Store the cached or extracted data in the output list under the metric name
            if (!is.null(metric_data)) {
                if (length(variable_list) == 1) {
                    extracted_data[[paste0(metric_name, "_", model_name)]] <- metric_data
                } else if (length(variable_list) > 1) {
                    extracted_data[[paste0(metric_name, "_", model_name, "_", var_name)]] <- metric_data
                }
            }
        }
    }
    end.time <- Sys.time()
    # Return the list of extracted data
    time.taken <- end.time - start.time
    print(time.taken)
    return(extracted_data)
}



# extract_variable_list <- function(extracted_metric_dict, folder, config_file, model_filter = "all") {


#     # Empty list to store the results for each metric
#     start.time <- Sys.time()
#     extracted_data <- list()
    
#     # Cache to store already extracted model-variable combinations
#     cache <- list()
    
#    # Filter model if not "all"
#     if (model_filter != "all") {
#         extracted_metric_dict <- extracted_metric_dict %>%
#             dplyr::filter(model %in% model_filter)
#     }

#     # Loop through the metrics dictionary
#     for (i in 1:nrow(extracted_metric_dict)) {
#         # Extract metric details
#         metric_name <- extracted_metric_dict$metric_name[i]
#         model_name <- extracted_metric_dict$model[i]
#         variable_model_name <- extracted_metric_dict$variable_model_name[i]
#         depth_01 <- extracted_metric_dict$depth_01[i]
#         conversion_factor <- extracted_metric_dict$conversion_factor[i]
        
#         # Determine the file path based on the model
#         nc_file <- file.path(folder, model_name, "Output", "Output.nc")
#         if (!file.exists(nc_file)) {
#             cat("NetCDF file for model", model_name, "not found at:", nc_file, "\n")
#             next
#         }
        
#         # Split and trim variable names
#         variable_list <- strsplit(variable_model_name, ",")[[1]]
        
#         for (var_name in variable_list) {
#             var_name <- trimws(var_name)  # Remove any whitespace
            
#             # Define a cache key based only on model_name and var_name to store the already extracted ones
#             cache_key <- paste0(model_name, "_", var_name)
            
#             # Check if the model-variable combination is already in the cache
#             if (cache_key %in% names(cache)) {
#                 cat("Using cached data for model", model_name, "variable", var_name, "\n")
#                 metric_data <- cache[[cache_key]]
#             } else {
#                 cat("Extracting data for model", model_name, "using variable", var_name, "\n")
                
#                 # Open the NetCDF file and extract data
#                 nc <- tryCatch({
#                     ncdf4::nc_open(nc_file)
#                 }, error = function(e) {
#                     cat("Error opening NetCDF file:", nc_file, "\n")
                    
#                     return(NULL)
#                 })
                
#                 if (!is.null(nc)) {
#                     metric_data <- tryCatch({
#                         get_output_wq(config_file = config_file, model = model_name, vars = var_name, folder = folder, depth_01 = depth_01, conversion_factor = conversion_factor)
#                     }, error = function(e) {
#                         cat("Error extracting data for variable:", var_name, "\n")
#                         print(e)  # Print the full error message
#                         return(NULL)
#                     })
                    
#                     # Close the NetCDF file
#                     ncdf4::nc_close(nc)
                    
#                     # Cache the extracted data based on the model-variable combination
#                     if (!is.null(metric_data)) {
#                         cache[[cache_key]] <- metric_data
#                     }
#                 }
#             }
            
#             # Store the cached or extracted data in the output list under the metric name
#             if (!is.null(metric_data)) {
#                 if (length(variable_list) == 1) {
#                     extracted_data[[paste0(metric_name, "_", model_name)]] <- metric_data
#                 } else if (length(variable_list) > 1) {
#                     extracted_data[[paste0(metric_name, "_", model_name, "_", var_name)]] <- metric_data
#                 }
#             }
#         }
#     }
#     end.time <- Sys.time()
#     # Return the list of extracted data
#     time.taken <- end.time - start.time
#     print(time.taken)
#     return(extracted_data)
# }










# extract_variable_list <- function(extracted_metric_dict, folder, config_file, model_filter = "all") {


#     # Empty list to store the results for each metric
#     start.time <- Sys.time()
#     extracted_data <- list()
    
#     # Cache to store already extracted model-variable combinations
#     cache <- list()
    
#    # Filter model if not "all"
#     if (model_filter != "all") {
#         extracted_metric_dict <- extracted_metric_dict %>%
#             dplyr::filter(model %in% model_filter)
#     }

#     # Loop through the metrics dictionary
#     for (i in 1:nrow(extracted_metric_dict)) {
#         # Extract metric details
#         metric_name <- extracted_metric_dict$metric_name[i]
#         model_name <- extracted_metric_dict$model[i]
#         variable_model_name <- extracted_metric_dict$variable_model_name[i]
#         depth_01 <- extracted_metric_dict$depth_01[i]
#         conversion_factor <- extracted_metric_dict$conversion_factor[i]
        
#         # Determine the file path based on the model
#         nc_file <- file.path(folder, model_name, "Output", "Output.nc")
#         if (!file.exists(nc_file)) {
#             cat("NetCDF file for model", model_name, "not found at:", nc_file, "\n")
#             next
#         }
        
#         # Split and trim variable names
#         variable_list <- strsplit(variable_model_name, ",")[[1]]
        
#         for (var_name in variable_list) {
#             var_name <- trimws(var_name)  # Remove any whitespace
            
#             # Define a cache key based only on model_name and var_name to store the already extracted ones
#             cache_key <- paste0(model_name, "_", var_name)
            
#             # Check if the model-variable combination is already in the cache
#             if (cache_key %in% names(cache)) {
#                 cat("Using cached data for model", model_name, "variable", var_name, "\n")
#                 metric_data <- cache[[cache_key]]
#             } else {
#                 cat("Extracting data for model", model_name, "using variable", var_name, "\n")
                
#                 # Open the NetCDF file and extract data
#                 nc <- tryCatch({
#                     ncdf4::nc_open(nc_file)
#                 }, error = function(e) {
#                     cat("Error opening NetCDF file:", nc_file, "\n")
                    
#                     return(NULL)
#                 })
                
#                 if (!is.null(nc)) {
#                     metric_data <- tryCatch({
#                         get_output_wq(config_file = config_file, model = model_name, vars = var_name, folder = folder, depth_01 = depth_01, conversion_factor = conversion_factor)
#                     }, error = function(e) {
#                         cat("Error extracting data for variable:", var_name, "\n")
#                         print(e)  # Print the full error message
#                         return(NULL)
#                     })
                    
#                     # Close the NetCDF file
#                     ncdf4::nc_close(nc)
                    
#                     # Cache the extracted data based on the model-variable combination
#                     if (!is.null(metric_data)) {
#                         cache[[cache_key]] <- metric_data
#                     }
#                 }
#             }
            
#             # Store the cached or extracted data in the output list under the metric name
#             if (!is.null(metric_data)) {
#                 if (length(variable_list) == 1) {
#                     extracted_data[[paste0(metric_name, "_", model_name)]] <- metric_data
#                 } else if (length(variable_list) > 1) {
#                     extracted_data[[paste0(metric_name, "_", model_name, "_", var_name)]] <- metric_data
#                 }
#             }
#         }
#     }
#     end.time <- Sys.time()
#     # Return the list of extracted data
#     time.taken <- end.time - start.time
#     print(time.taken)
#     return(extracted_data)
# }
