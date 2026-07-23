# Create variable list for calculating the metrics

The aim is to extract & apply the necessary unit conversions the list of
variables data that is selected in output.yaml.

## Usage

``` r
extract_variable_list(extracted_metric_dict, config_file, model_filter = "all")
```

## Arguments

- extracted_metric_dict:

  character: full filepath for the metric dictionary.

- config_file:

  character:filepath; To Output.yaml

- model_filter:

  character: name of the model to be extracted (GLM, SELMAPROTBAS, WET).
  If all model outputs, it should be set to model= "all"

## Value

A list of extractedf variables for each model and for each metric
defined in output.yaml
