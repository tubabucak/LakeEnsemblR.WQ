# Load and validate LakeEnsemblR.WQ configuration file

This function reads a YAML configuration file and validates the presence
and existence of required file and folder paths used in LakeEnsemblR.WQ
workflows. The configuration file must include a base folder path,
relative or full paths to required input files (bathymetry, metric
dictionary, and metric YAML), and model-specific output folders.

## Usage

``` r
load_config(config_path)
```

## Arguments

- config_path:

  Full path to the YAML configuration file.

## Value

A named list with the following elements:

- folder:

  Base directory specified in the config file.

- bathy_file:

  Full path to the bathymetry file.

- metrics_dict_file:

  Full path to the metrics dictionary file (.rda or .csv; NULL if not
  provided).

- metric_yaml_file:

  Full path to the metric output YAML file.

- LER_config_file:

  Full path to the metric output YAML file.

- model_folders:

  A named list with full paths to model output folders (e.g., GLM, WET,
  SELMA).

## Details

The function resolves any relative paths in the config file relative to
the provided `folder` field. It also verifies that each required file
and folder exists. If any path is invalid or missing, the function will
stop with a descriptive error message.

## Examples

``` r
if (FALSE) { # \dontrun{
config <- load_config("path/to/config.yaml")
print(config$metrics_dict_file)
} # }
```
