# Calculate the Metrics

The aim is to calculate the metrics in output.yaml and export as list
file

## Usage

``` r
cal_metrics(
  metric_yaml_file,
  model_filter = "all",
  wq_config_file,
  layers = c("profile", "epi_hypo")
)
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

- layers:

  character: `"profile"` (default) keeps the depth profiles (`Depth_<z>`
  columns). `"epi_hypo"` reduces every depth profile to daily means of
  the epilimnion and hypolimnion (columns `epi` and `hypo`), split at
  the daily thermocline of the model's `Temp_degreeCelcius` profile
  ([`rLakeAnalyzer::thermo.depth()`](https://rdrr.io/pkg/rLakeAnalyzer/man/thermo.depth.html));
  when the lake is mixed both hold the whole-column mean. Needs
  `Temp_degreeCelcius` in the metrics file. Metrics without depth
  columns are kept as they are.

## Value

A list of extractedf variables for each model and for each metric
defined in output.yaml

## Examples

``` r
if (FALSE) { # requireNamespace("GLMr", quietly = TRUE) && requireNamespace("WETr", quietly = TRUE) && requireNamespace("SelmaprotbasR", quietly = TRUE) && requireNamespace("SimstratR", quietly = TRUE)
# \donttest{
ex <- lerwq_example()
library(LakeEnsemblR)  # export_config() needs LakeEnsemblR attached
export_config("LakeEnsemblR.yaml", folder = ex,
              model = c("GLM", "GOTM", "Simstrat"))
export_config_wq("LakeEnsemblR_WQ.yaml", folder = ex)
run_ensemble_wq("LakeEnsemblR_WQ.yaml", models = "GLM-AED", folder = ex,
                verbose = FALSE)

# Harmonized metrics listed in Output.yaml, for GLM-AED
old <- setwd(ex)
metrics <- cal_metrics(metric_yaml_file = "Output.yaml", model_filter = "GLM",
                       wq_config_file = "LakeEnsemblR_WQ.yaml")
names(metrics)
head(metrics$DO_gramsPerCubicMeter$GLM[[1]][, 1:5])
setwd(old)
# }
}
```
