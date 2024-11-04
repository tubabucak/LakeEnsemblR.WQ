
# Load necessary libraries
library(dplyr)
library(ncdf4)  # for handling NetCDF files


create_netcdf_wq <- function(extracted_metric_dict, folder, config_file){
  # folder <- "C:/Users/au721619/OneDrive - Aarhus universitet/Documents/GitHub/LakeEnsemblR.WQ/inst/extdata/example_input_files"
# config_file <- "C:/Users/au721619/OneDrive - Aarhus universitet/Documents/GitHub/LakeEnsemblR.WQ/inst/extdata/example_input_files/LakeEnsemblR.yaml"

  # Empty list to store the results for each metric
  extracted_data <- list()
  

  # Cache to store already extracted variables (e.g., temperature and oxygen)
  cache <- list()
  # Create the new NetCDF file
  #nc_out <- ncdf4::nc_create(output_nc_file, list(var_def))
  
# Defining the dimensions of the netcdf file



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
      cat("Extracting", metric_name, "from model", model_name, "using variable", variable_model_name, "\n")
      
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
          
          get_output_wq(config_file = config_file, model = model_name, vars = variable_model_name, folder = folder, depth_01 = depth_01, conversion_factor = conversion_factor)  # Assuming this function extracts data based on variable name
        }, error = function(e) {
          cat("Error extracting data for variable:", variable_model_name, "\n")
          return(NULL)
        })
        
        # Close the NetCDF file
        ncdf4::nc_close(nc)
        
        # Store the extracted data in the list
        if (!is.null(metric_data)) {
          extracted_data[[paste0(metric_name, "_", model_name)]] <- metric_data
        }
      }
    } else {
      cat("NetCDF file for model", model_name, "not found!\n")
    }
  }
  
  # Return the list of extracted data
  return(extracted_data)
}

# test1 <- test[1:4,]
# deneme <- create_netcdf(test1, )
