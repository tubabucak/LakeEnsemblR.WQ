# Calculate the Metrics

The aim is to calculate the metrics in output.yaml and export as list
file

## Usage

``` r
cal_metrics(metric_yaml_file, model_filter = "all", wq_config_file)
```

## Arguments

- metric_yaml_file:

  Character: Name of the YAML file containing the list of metrics.

- model_filter:

  character: name of the model to be extracted (GLM, SELMAPROTBAS, WET).
  If all model outputs, it should be set to model= "all"

- wq_config_file:

  character: path to the LakeEnsemblR_WQ config file (e.g.
  "LakeEnsemblR_WQ.yaml"), used to expand phytoplankton/zooplankton
  group templates in the metrics dictionary.

## Value

A list of extractedf variables for each model and for each metric
defined in output.yaml
