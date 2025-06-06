% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_config.r
\name{load_config:}
\alias{load_config:}
\alias{load_config}
\title{Load and validate LakeEnsemblR.WQ configuration file}
\usage{
load_config(config_path)
}
\arguments{
\item{config_path}{Full path to the YAML configuration file.}
}
\value{
A named list with the following elements:
\describe{
  \item{folder}{Base directory specified in the config file.}
  \item{bathy_file}{Full path to the bathymetry file.}
  \item{metrics_dict_file}{Full path to the metrics dictionary CSV file.}
  \item{metric_yaml_file}{Full path to the metric output YAML file.}
  \item{LER_config_file}{Full path to the metric output YAML file.}
  \item{model_folders}{A named list with full paths to model output folders (e.g., GLM, WET, SELMA).}
}
}
\description{
This function reads a YAML configuration file and validates the presence and existence of 
required file and folder paths used in LakeEnsemblR.WQ workflows. The configuration file 
must include a base folder path, relative or full paths to required input files (bathymetry, 
metric dictionary, and metric YAML), and model-specific output folders.
}
\details{
The function resolves any relative paths in the config file relative to the provided \code{folder}
field. It also verifies that each required file and folder exists. If any path is invalid or 
missing, the function will stop with a descriptive error message.
}
\examples{
\dontrun{
config <- load_config_strict("path/to/config.yaml")
print(config$metrics_dict_file)
}
}
