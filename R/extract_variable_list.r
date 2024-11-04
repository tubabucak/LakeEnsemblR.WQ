#'@title Create variable list for calculating the metrics

#'@description
#' The aim is to extract & apply the necessary unit conversions the list of variables data that is selected in output.yaml. 

#' @name extract_variable_list
#' @param dict_file character: full filepath for the metric dictionary.
#' @param folder character: main filepath; where all the model files are stored.
#' @param config_file character:filepath; To LER configuration yaml file.
#' 
#' @return A list of extractedf variables for each model and for each metric defined in output.yaml
#' 
#' @importFrom utils read.csv
#' @importFrom ncdf4 nc_open nc_close
#' @import get_output_wq
#' @examples
#' Example usage:
#' # temp_diff_result <- cal_bot_surf_temp_dif(temp_glm)
#' # print(temp_diff_result)

#' @export
extract_variable_list <- function(extracted_metric_dict, folder, config_file){

  # Empty list to store the results for each metric
  extracted_data <- list()
  

  # Cache to store already extracted variables (mostly temperature and oxygen)
  cache <- list()
  # Create the new NetCDF file
  #nc_out <- ncdf4::nc_create(output_nc_file, list(var_def))
  
  # Loop through the metrics dictionary
  for (i in 1:nrow(extracted_metric_dict)) {
    # Extract metric details
    metric_name <- extracted_metric_dict$metric_name[i]
    model_name <- extracted_metric_dict$model[i]
    variable_model_name <- extracted_metric_dict$variable_model_name[i]
    depth_01 <- extracted_metric_dict$depth_01[i]
    conversion_factor<- extracted_metric_dict$conversion_factor[i]
    if (model_name == 'GLM'){

     
      nc_file <- file.path(folder, "GLM", "Output", "Output.nc")
      
    }
    else if (model_name == 'SELMAPROTBAS'){
      nc_file <- file.path(folder, "SELMAPROTBAS", "Output", "Output.nc")
    }
    else if (model_name == 'WET'){
      
      nc_file <- file.path(folder, "WET", "Output", "Output.nc")
      if (file.exists(nc_file)) {
            cat("NetCDF file found at:", nc_file, "\n")
    } else {
            cat("NetCDF file for model", model_name, "not found at:", nc_file, "\n")
}

    }
    # Check if the NetCDF file exists before proceeding
    if (file.exists(nc_file)) {

      variable_list <- strsplit(variable_model_name, ",")[[1]]
      
      for (var_name in variable_list) {
        var_name <- trimws(var_name)  # Remove any leading/trailing whitespace
        cat("Extracting", metric_name, "from model", model_name, "using variable", var_name, "\n")
          
      # Open the NetCDF file
      nc <- tryCatch({
        ncdf4::nc_open(nc_file)
      }, error = function(e) {
        cat("Error opening NetCDF file:", nc_file, "\n")
        return(NULL)
      })
      
      if (!is.null(nc)) {
        # Use the 'get_output_wq' function to extract data from the NetCDF file
        metric_data <- tryCatch({
          
          get_output_wq(config_file = config_file, model = model_name, vars = var_name, folder = folder, depth_01 = depth_01, conversion_factor = conversion_factor)  # Assuming this function extracts data based on variable name
        }, error = function(e) {
          cat("Error extracting data for variable:", variable_model_name, "\n")
          return(NULL)
        })
        
        # Close the NetCDF file
        ncdf4::nc_close(nc)
        
        # Store the extracted data in the list
        if (!is.null(metric_data)) {

          if (length(variable_list) == 1){

          extracted_data[[paste0(metric_name, "_", model_name)]] <- metric_data
          }
          else if(length(variable_list) > 1){


          extracted_data[[paste0(metric_name, "_", model_name, "_", var_name)]] <- metric_data
          }

         }
       }
     }
    } else {
      cat("NetCDF file for model", model_name, "not found!\n")
    }
  }
  
  # Return the list of extracted data
  return(extracted_data)
}

