# LakeEnsemblR.WQ

Run, harmonize, and compare water quality simulations across multiple 1D lake model frameworks.

LakeEnsemblR.WQ extends LakeEnsemblR workflows with tools to:

- configure model-specific WQ settings,
- validate model setups before run,
- run model ensembles,
- calibrate model ensembles,
- extract and harmonize output variables,
- calculate standardized ecosystem metrics and summary statistics.

Supported model frameworks:

- GLM-AED2
- GOTM-WET
- GOTM-Selmaprotbas
- Simstrat-AED2

## Installation

```r
# install.packages("remotes")
# After release it will be pushed to aemon-j github account.
remotes::install_github("tubabucak/LakeEnsemblR.WQ")
```

## Typical workflow

```r
library(LakeEnsemblR)

# 0) Export model-specific configuration and inputs for physical setup

export_config(
  config_file = "LakeEnsemblR.yaml",
  folder = ".",
  verbose = TRUE
)

library(LakeEnsemblR.WQ)


# 1) Export model-specific configuration and inputs for WQ setup
export_config_wq(
  config_file = "LakeEnsemblR_WQ.yaml",
  folder = ".",
  verbose = TRUE
)

# 2) Run selected models
run_res <- run_ensemble_wq(
  config_file = "LakeEnsemblR_WQ.yaml",
  models = c("GLM-AED2", "GOTM-WET", "GOTM-Selmaprotbas", "SIMSTRAT-AED2"),
  folder = ".",
  validate = TRUE,
  verbose = TRUE
)

# 3) Compute metrics listed in Output.yaml
metric_out <- cal_metrics(
  metric_yaml_file = "Output.yaml",
  model_filter = "all",
  wq_config_file = "LakeEnsemblR_WQ.yaml"
)

# 4) Optional: compute summary statistics
stat_out <- cal_stats(metric_out)
```

## Key functions

- Setup and export:
  - export_config_wq()
  - export_inputs()
  - set_up_configs()
  - set_value_config()
- Validation and run:
  - validate_glm_aed()
  - validate_gotm_wet()
  - validate_simstrat()
  - run_ensemble_wq()
- Extraction and metrics:
  - get_output_wq()
  - extract_variable_list()
  - cal_metrics()
  - cal_stats()
  - export_all_stats()
- Calibration
  - run_lhc_wq()

## Input files

Common project files include:

- LakeEnsemblR_WQ master config (for model/module settings)
- LakeEnsemblR physical config
- Output metric config (for selected metric families)

Example from a typical setup:

- LakeEnsemblR_WQ.yaml
- LakeEnsemblR.yaml
- Output.yaml
- LakeEnsemblR_bathymetry_standard
- LakeEnsemblR_ice-height_standard
- LakeEnsemblR_inflow_standard
- LakeEnsemblR_meteo_standard
- LakeEnsemblR_outflow_standard

## Output structure

`cal_metrics()` returns a nested list by metric and model. For each metric/model combination, values are stored as data frames with `datetime` and depth/value columns.

Example access:

```r
metric_out$GLM$TP_gramsPerCubicMeter

```

## Troubleshooting

If a model run succeeds but some metrics are missing:

1. Confirm the metric is enabled in Output.yaml.
2. Confirm the metric-variable mapping exists in the metrics dictionary.
3. Confirm the model actually wrote that variable to output.

For Simstrat-AED2 specifically, missing derived metrics (for example total chlorophyll-a or total phosphorus) are often caused by missing output variables in the Simstrat/AED2 output configuration (OutputDiagnosticVars should be 'true' ), not by run failure.

## Contributing

1. Fork this repository.
2. Clone your fork.
3. Add the original repository as `upstream`.
4. Pull latest changes from `upstream/main`.
5. Create a feature branch, commit changes, and push.
6. Open a pull request.

Example git commands:

```bash
git clone git@github.com:<username>/LakeEnsemblR.WQ.git
cd LakeEnsemblR.WQ
git remote add upstream git@github.com:aemon-j/LakeEnsemblR.WQ.git
git fetch upstream
git checkout -b my-feature
git merge upstream/main
git push -u origin my-feature
```
