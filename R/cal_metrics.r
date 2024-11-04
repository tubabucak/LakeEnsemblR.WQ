#'@title Calculate the Metrics

#'@description
#' The aim is to calculate the metrics in output.yaml and export as netcdf file 

#' @name cal_metrics
#' @param metric_yaml_file Character: Name of the YAML file containing the list of metrics.
#' @param dict_file character: full filepath for the metric dictionary.
#' @param folder character: main filepath; where all the model files are stored.
#' @param config_file character:filepath; To LER configuration yaml file.
#' 
#' @return A list of extractedf variables for each model and for each metric defined in output.yaml
#' 
#' @importFrom utils read.csv
#' @importFrom ncdf4 nc_open nc_close
#' @importFrom rLakeAnalyzer load.bathy
#' @import extract_variable_list
#' @import check_the_metrics
#' @examples
#' Example usage:
#' # temp_diff_result <- cal_bot_surf_temp_dif(temp_glm)
#' # print(temp_diff_result)
#' 
#' @export
#' 
cal_metrics <- function(metric_yaml_file, folder, dict_file, config_file, bathy_file) {

    metric_out <- list()
    
    # Extract the information from metric dictionary regarding output.yaml
    sel_metric <- check_the_metrics(metric_yaml_file, folder, dict_file)

    # Extract bathymetry file
    bathy_file <- load.bathy(bathy_file)

    # Extract all the variables and harmonize the units
    ext_data <- extract_variable_list(sel_metric, folder, config_file)


    # Check if both blue_ice_thickness and white_ice_thickness exist in ext_data
        if ("Ice_Thickness_meter_GLM_blue_ice_thickness" %in% names(ext_data) & 
            "Ice_Thickness_meter_GLM_white_ice_thickness" %in% names(ext_data)) {


        # Extract the blue ice and white ice data frames
        blue_ice <- ext_data$Ice_Thickness_meter_GLM_blue_ice_thickness[[1]]
        white_ice <- ext_data$Ice_Thickness_meter_GLM_white_ice_thickness[[1]]
  

  # Combine blue_ice_thickness and white_ice_thickness
        combined_ice <- blue_ice
        combined_ice$ice_thickness <- blue_ice$blue_ice_thickness + white_ice$white_ice_thickness
        combined_ice <- combined_ice[,-2]  # Remove blue_ice_thickness column

        ext_data[["Ice_Thickness_meter_GLM"]] <- list(Hice_combined = combined_ice)
    
        # Remove the blue and white ice entries from ext_data
        ext_data$Ice_Thickness_meter_GLM_blue_ice_thickness <- NULL
        ext_data$Ice_Thickness_meter_GLM_white_ice_thickness <- NULL

}

# Merge Metric name with lake model to match the extracted dataframe
    sel_metric$metric_mod <- paste(sel_metric$metric_name, sel_metric$model, sep= "_") 

    
    var_list <- names(ext_data)

    for (i in 1:length(var_list)) {
        print(var_list[i])

       # Use metric_name and model from sel_metric directly
        sel_metric_sub <- sel_metric %>%
                          filter(metric_mod == var_list[i])
        
        metric_name <- sel_metric_sub$metric_name
        model_name <- sel_metric_sub$model

        # Initialize the metric list if it doesn't exist
        if (!metric_name %in% names(metric_out)) {
            metric_out[[metric_name]] <- list()
        }

        var_x <- ext_data[[var_list[i]]][1]
        var_x_sub <- names(var_x)
        var_x_df <- var_x[[var_x_sub]]
  


        # Check if function_name is empty
        if (sel_metric_sub$function_name == "") {
            metric_out[[metric_name]][[model_name]] <- var_x_df
        } else {
            # Extract the metric function
            met_fun <- get(sel_metric_sub$function_name)
            
            # Extract arguments needed for functions 
            args_list <- sel_metric_sub$arguments[[1]]

            # Handle NULL or empty args_list
            if (is.null(args_list)) {
                args_list <- list()
            }
            
            # Special handling for 'cal_anoxic_date' function
            if (sel_metric_sub$function_name == "cal_anoxic_date") {
                metric_i <- do.call(met_fun, c(list(var_x_df, bathy_file), args_list))
            } else {
                metric_i <- do.call(met_fun, c(list(var_x_df), args_list))    
            }
            
            # Store the calculated metric in the output list
            metric_out[[metric_name]][[model_name]] <- metric_i
        }



          
    }

if ("DIC_gramsPerCubicMeter" %in% names(metric_out) & 
            "DOC_gramsPerCubicMeter" %in% names(metric_out)) {
  
  DIC_mod_names <- names(metric_out[["DIC_gramsPerCubicMeter"]])
  DOC_mod_names <- names(metric_out[["DOC_gramsPerCubicMeter"]])
  
  for ( model in DIC_mod_names){
    
    DIC<- metric_out[["DIC_gramsPerCubicMeter"]][[model]]
    DOC<- metric_out[["DIC_gramsPerCubicMeter"]][[model]]
    
    # Check if Date columns are the same in both dataframes (for safety)
    if (!all(DIC$datetime == DOC$datetime)) {
      stop("Date columns in DIC_gramsPerCubicMeter and DOC_gramsPerCubicMeter do not match!")
    }
    # divide the corresponding columns directly (excluding datetime column)
    DIC_DOC_ratio <- data.frame(datetime = DIC$datetime, DIC[, -1]/DOC[, -1])
    
    # Initialize the ratio list if it doesn't exist
            if (!"DIC_DOC_ratio" %in% names(metric_out)) {
                metric_out[["DIC_DOC_ratio"]] <- list()
            }

            # Store the ratio in the output list
            metric_out[["DIC_DOC_ratio"]][[model]] <- DIC_DOC_ratio
    
  }
  
}

if ("TP_gramsPerCubicMeter" %in% names(metric_out) & 
            "Total_Chla_miligramsPerCubicMeter" %in% names(metric_out)) {
  
  TP_mod_names <- names(metric_out[["TP_gramsPerCubicMeter"]])
  TChla_mod_names <- names(metric_out[["Total_Chla_miligramsPerCubicMeter"]])
  
  for ( model in TP_mod_names){
    
    TP<- metric_out[["TP_gramsPerCubicMeter"]][[model]]
    TChla<- metric_out[["Total_Chla_miligramsPerCubicMeter"]][[model]]
    
    # Debugging prints
            print(paste("Processing TP and Chla for model:", model))
            print(paste("TP extracted:", !is.null(TP)))
            print(paste("TChla extracted:", !is.null(TChla)))
    # Check if Date columns are the same in both dataframes (for safety)
    if (!all(TP$datetime == TChla$datetime)) {
      stop("Date columns in TP_gramsPerCubicMeter and Total_Chla_miligramsPerCubicMeter do not match!")
    }
    # Unit of Tchla was g/L, hence it is divided by 1000 to harmonize the units
    Chla_TP_ratio <- data.frame(datetime = TChla$datetime, (TChla[, -1]/1000)/TP[, -1])
    
    # Initialize the ratio list if it doesn't exist
            if (!"Chla_TP_ratio" %in% names(metric_out)) {
                metric_out[["Chla_TP_ratio"]] <- list()
            }

            # Store the ratio in the output list
            metric_out[["Chla_TP_ratio"]][[model]] <- Chla_TP_ratio
    
  }
  
}

if ("TP_gramsPerCubicMeter" %in% names(metric_out) & 
            "TN_gramsPerCubicMeter" %in% names(metric_out)) {
  
  TP_mod_names <- names(metric_out[["TP_gramsPerCubicMeter"]])
  TN_mod_names <- names(metric_out[["TN_gramsPerCubicMeter"]])
  
  for ( model in TP_mod_names){
    
    TP<- metric_out[["TP_gramsPerCubicMeter"]][[model]]
    TN<- metric_out[["TN_gramsPerCubicMeter"]][[model]]
    
    # Debugging prints
            print(paste("Processing TP and TN for model:", model))
            print(paste("TP extracted:", !is.null(TP)))
            print(paste("TN extracted:", !is.null(TN)))
    # Check if Date columns are the same in both dataframes (for safety)
    if (!all(TP$datetime == TN$datetime)) {
      stop("Date columns in TP_gramsPerCubicMeter and TN_gramsPerCubicMeter do not match!")
    }
    
   TN_TP_ratio <- data.frame(datetime = TN$datetime, (TN[, -1])/TP[, -1])
    
    # Initialize the ratio list if it doesn't exist
            if (!"TN_TP_ratio" %in% names(metric_out)) {
                metric_out[["TN_TP_ratio"]] <- list()
            }

            # Store the ratio in the output list
            metric_out[["TN_TP_ratio"]][[model]] <- TN_TP_ratio
    
  }
  
}








    return(metric_out)
}
