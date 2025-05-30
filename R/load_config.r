#' Load and validate LakeEnsemblR.WQ configuration file
#'
#' This function reads a YAML configuration file and validates the presence and existence of 
#' required file and folder paths used in LakeEnsemblR.WQ workflows. The configuration file 
#' must include a base folder path, relative or full paths to required input files (bathymetry, 
#' metric dictionary, and metric YAML), and model-specific output folders.
#'
#' @param config_path Full path to the YAML configuration file.

#' @name load_config: 
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{folder}{Base directory specified in the config file.}
#'   \item{bathy_file}{Full path to the bathymetry file.}
#'   \item{metrics_dict_file}{Full path to the metrics dictionary CSV file.}
#'   \item{metric_yaml_file}{Full path to the metric output YAML file.}
#'   \item{LER_config_file}{Full path to the metric output YAML file.}
#'   \item{model_folders}{A named list with full paths to model output folders (e.g., GLM, WET, SELMA).}
#' }
#'
#' @details
#' The function resolves any relative paths in the config file relative to the provided \code{folder}
#' field. It also verifies that each required file and folder exists. If any path is invalid or 
#' missing, the function will stop with a descriptive error message.
#'
#' @examples
#' \dontrun{
#' config <- load_config_strict("path/to/config.yaml")
#' print(config$metrics_dict_file)
#' }
#' @import yaml
#' @export

load_config <- function(config_path) {
  if (!file.exists(config_path)) {
    stop("Config file not found at: ", config_path)
  }
  
  config <- yaml::read_yaml(config_path)
  
  # Ensure required top-level fields
  required_fields <- c("folder", "files", "model_folders")
  missing_fields <- setdiff(required_fields, names(config))
  if (length(missing_fields) > 0) {
    stop("Missing required fields in config: ", paste(missing_fields, collapse = ", "))
  }
  
  folder <- config$folder
  files <- config$files
  model_folders <- config$model_folders
  
  # Resolve file paths relative to 'folder'
  resolve_path <- function(path) {
    if (!grepl("^([A-Za-z]:|/)", path)) file.path(folder, path) else path
  }
  
  # Ensure each required file is present and exists
  required_files <- c("bathy_file", "metrics_dict", "metric_yaml_file", "LER_config_file")
  for (key in required_files) {
    if (is.null(files[[key]])) stop(paste("Missing", key, "in 'files'."))
    
    full_path <- resolve_path(files[[key]])
    if (!file.exists(full_path)) stop(paste("File not found:", full_path))
    
    files[[key]] <- full_path  # overwrite with full path
  }
  
  # Ensure model folders exist
  for (model in names(model_folders)) {
    path <- resolve_path(model_folders[[model]])
    if (!dir.exists(path)) stop(paste("Model folder for", model, "not found at:", path))
    model_folders[[model]] <- path
  }
  
  return(list(
    folder = folder,
    bathy_file = files$bathy_file,
    metrics_dict_file = files$metrics_dict,
    metric_yaml_file = files$metric_yaml_file,
    LER_config_file = files$LER_config_file,
    model_folders = model_folders
  ))
}
