#' @title Check and Extract Selected Metrics from output.YAML
#'
#' @description
#' This function reads a YAML file containing metrics and extracts the relevant metric details based on a provided dictionary. Firstly, it checks whether each metric in the YAML file exists in the dictionary and, then extracts the necessary information needed for calculating the metric.
#'
#' @param metric_yaml_file Character: Name of the YAML file containing the list of metrics.
#' @param dict_file Character or data.frame: Metric dictionary source. Can be a
#'   full file path (.rda or .csv), a data.frame, or NULL. If NULL/empty, a bundled default
#'   dictionary will be used.
#'

#' @return
#' A data frame containing the extracted metric details which will be used to extract the data from netcdf files (using get_output_wq & create_netcdf_wq)
#'
#' @importFrom yaml yaml.load_file
#' @importFrom dplyr filter select
#' @export
#' 
#' 


.load_metrics_dictionary_wq <- function(dict_file = NULL, metric_yaml_file = NULL) {
  if (is.data.frame(dict_file)) {
    return(dict_file)
  }

  if (!is.null(dict_file) && nzchar(dict_file)) {
    if (grepl("\\.rda$", dict_file, ignore.case = TRUE)) {
      tmp_env <- new.env(parent = emptyenv())
      tryCatch({
        load(dict_file, envir = tmp_env)
      }, error = function(e) {
        stop("Error reading the dictionary RDA file: ", dict_file)
      })

      if (!exists("Metrics_dict", envir = tmp_env, inherits = FALSE)) {
        stop("RDA dictionary file must contain an object named 'Metrics_dict': ", dict_file)
      }

      dict_obj <- get("Metrics_dict", envir = tmp_env, inherits = FALSE)
      if (!is.data.frame(dict_obj)) {
        stop("Object 'Metrics_dict' in RDA file is not a data.frame: ", dict_file)
      }
      return(dict_obj)
    }

    return(tryCatch({
      read.csv(
        dict_file,
        sep = ifelse(grepl(";", readLines(dict_file, n = 1)), ";", ","),
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      stop("Error reading the dictionary CSV file: ", dict_file)
    }))
  }

  default_dataset_names <- c("Metrics_dict")
  for (dataset_name in default_dataset_names) {
    if (exists(dataset_name, inherits = TRUE)) {
      dict_obj <- get(dataset_name, inherits = TRUE)
      if (is.data.frame(dict_obj)) {
        return(dict_obj)
      }
    }

    tmp_env <- new.env(parent = emptyenv())
    try(utils::data(list = dataset_name, package = "LakeEnsemblR.WQ", envir = tmp_env), silent = TRUE)
    if (exists(dataset_name, envir = tmp_env, inherits = FALSE)) {
      dict_obj <- get(dataset_name, envir = tmp_env, inherits = FALSE)
      if (is.data.frame(dict_obj)) {
        return(dict_obj)
      }
    }
  }

  dict_candidates <- c(
    if (!is.null(metric_yaml_file) && nzchar(metric_yaml_file)) file.path(dirname(metric_yaml_file), "Metrics_dict.rda") else "",
    system.file("extdata", "Metrics_dict.rda", package = "LakeEnsemblR.WQ"),
    file.path("data", "Metrics_dict.rda")
  )
  dict_candidates <- dict_candidates[nzchar(dict_candidates)]
  dict_existing <- dict_candidates[file.exists(dict_candidates)]

  if (length(dict_existing) > 0) {
    dict_path <- dict_existing[1]
    tmp_env <- new.env(parent = emptyenv())
    tryCatch({
      load(dict_path, envir = tmp_env)
    }, error = function(e) {
      stop("Error reading the dictionary RDA file: ", dict_path)
    })

    if (!exists("Metrics_dict", envir = tmp_env, inherits = FALSE)) {
      stop("Default dictionary RDA must contain an object named 'Metrics_dict': ", dict_path)
    }

    dict_obj <- get("Metrics_dict", envir = tmp_env, inherits = FALSE)
    if (!is.data.frame(dict_obj)) {
      stop("Object 'Metrics_dict' in default RDA is not a data.frame: ", dict_path)
    }
    return(dict_obj)
  }

  stop(
    "No metrics dictionary provided and no bundled default found. ",
    "Provide files$metrics_dict in the config yaml or include Metrics_dict as package data."
  )
}


check_the_metrics <- function(metric_yaml_file, dict_file = NULL) {
  yaml_path <- metric_yaml_file
  dict <- .load_metrics_dictionary_wq(dict_file = dict_file, metric_yaml_file = yaml_path)
  
  # Load the YAML file
  config <- tryCatch({
    yaml::yaml.load_file(yaml_path)
  }, error = function(e) {
    stop("Error reading YAML file: ", e$message)
  })
  
  # Prepare the output
  extracted_metrics <- data.frame(
    level = character(),
    module = character(),
    metric_name = character(),
    model = character(),
    variable_model_name = character(),
    conversion_factor = numeric(),
    function_name = character(),
    depth_01 = numeric(),
    arguments = I(list()),
    stringsAsFactors = FALSE
  )
  
  # Parse the YAML config
  for (level in names(config)) {
    modules <- config[[level]]
    for (module in names(modules)) {
      metrics <- modules[[module]]
      for (metric_name in names(metrics)) {
        metric_value <- metrics[[metric_name]]
        arguments <- if (is.list(metric_value)) metric_value else list()
        
        metric_in_dict <- dict %>%
          dplyr::filter(level == !!level, module == !!module, metric_name == !!metric_name)
        
        if (nrow(metric_in_dict) > 0) {
          metric_details <- metric_in_dict %>%
            dplyr::select(level, module, metric_name, model, variable_model_name, depth_01, conversion_factor, function_name)
          metric_details$arguments <- I(list(arguments))
          extracted_metrics <- rbind(extracted_metrics, metric_details)
        } else {
          cat(paste(metric_name, "in", level, "->", module, "is not valid. Please check the dictionary!\n"))
        }
      }
    }
  }
  
  return(extracted_metrics)
}


# check_the_metrics<- function(metric_yaml_file, folder, dict_file) {
  
#   # Full path name for dictionary file

#   dict_file<- file.path(folder, dict_file)

#   # Import the dictionary file (both comma and semicolon should work)
#   dict <- tryCatch({
#     read.csv(dict_file, sep = ifelse(grepl(";", readLines(dict_file, n = 1)), ";", ","), stringsAsFactors = FALSE)
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
  
  
#   # Initialize an empty dataframe to store the extracted metric table
#   extracted_metrics <- data.frame(
#     level = character(),
#     module = character(),
#     metric_name = character(),
#     model = character(),
#     variable_model_name = character(),
#     conversion_factor = numeric(),
#     function_name = character(),
#     depth_01 = numeric(),
#     arguments = I(list()),  # List column to store arguments
#     stringsAsFactors = FALSE
#   )
  
#   # Iterate over each level in the YAML file
#   for (level in names(config)) {
#     modules <- config[[level]]
    
#     # Iterate over each module within the current level
#     for (module in names(modules)) {
#       metrics <- modules[[module]]
      
#       # Iterate over each metric in the current module
#       for (metric_name in names(metrics)) {
#         metric_value <- metrics[[metric_name]]
        
#         # Check if the metric has arguments (it will be a list if it does)
#         if (is.list(metric_value)) {
#           arguments <- metric_value
#         } else {
#           arguments <- list()  # No arguments, so it's just an empty list
#         }
        
#         # Match the current level, module, and metric_name with the dictionary
#         metric_in_dict <- dict %>%
#           dplyr::filter(level == !!level, module == !!module, metric_name == !!metric_name)
        
#         if (nrow(metric_in_dict) > 0) {
#           # If the metric exists in the dictionary, append it to the extracted metrics
#           metric_details <- metric_in_dict %>%
#             dplyr::select(level, module, metric_name, model, variable_model_name, depth_01, conversion_factor, function_name)
          
#           # Add the arguments as a list column
#           metric_details$arguments <- I(list(arguments))
          
#           extracted_metrics <- rbind(extracted_metrics, metric_details)
#         } else {
#           cat(paste(metric_name, "in", level, "->", module, "is not valid. Please check the dictionary!\n"))
#         }
#       }
#     }
#   }
  
#   # Return the final extracted metrics dataframe with arguments
#   return(extracted_metrics)
# }
