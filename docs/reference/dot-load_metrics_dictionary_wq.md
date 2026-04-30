# Check and Extract Selected Metrics from output.YAML

This function reads a YAML file containing metrics and extracts the
relevant metric details based on a provided dictionary. Firstly, it
checks whether each metric in the YAML file exists in the dictionary
and, then extracts the necessary information needed for calculating the
metric.

## Usage

``` r
.load_metrics_dictionary_wq(dict_file = NULL, metric_yaml_file = NULL)
```

## Arguments

- dict_file:

  Character or data.frame: Metric dictionary source. Can be a full file
  path (.rda or .csv), a data.frame, or NULL. If NULL/empty, a bundled
  default dictionary will be used.

- metric_yaml_file:

  Character: Name of the YAML file containing the list of metrics.

## Value

A data frame containing the extracted metric details which will be used
to extract the data from netcdf files (using get_output_wq &
create_netcdf_wq)
