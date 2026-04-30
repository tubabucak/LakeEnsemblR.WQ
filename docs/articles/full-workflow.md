# LakeEnsemblR.WQ Full Workflow

## Purpose

This vignette provides an end-to-end workflow for LakeEnsemblR.WQ:

1.  Configure coupled model inputs.
2.  Validate model setups.
3.  Run model ensembles.
4.  Compute harmonized metrics.
5.  Prepare and run calibration.
6.  Run sensitivity analysis.

## Prerequisites

- A physical LakeEnsemblR setup already exported.
- A water-quality config file: LakeEnsemblR_WQ.yaml.
- A metrics config file: Output.yaml.
- Supported model folders, for example:
  - GLM-AED2
  - GOTM-WET
  - GOTM-Selmaprotbas
  - SIMSTRAT-AED2

``` r
library(LakeEnsemblR)
library(LakeEnsemblR.WQ)
```

## 1) Export WQ configuration and inputs

``` r
export_config_wq(
  config_file = "LakeEnsemblR_WQ.yaml",
  folder = ".",
  verbose = TRUE
)

export_inputs(
  config_file = "LakeEnsemblR_WQ.yaml",
  folder = "."
)
```

## 2) Validate model setups

``` r
validate_glm_aed(config_file = "LakeEnsemblR_WQ.yaml", folder = ".")
validate_gotm_wet(config_file = "LakeEnsemblR_WQ.yaml", folder = ".")
validate_simstrat(config_file = "LakeEnsemblR_WQ.yaml", folder = ".")
```

## 3) Run ensemble simulations

``` r
run_res <- run_ensemble_wq(
  config_file = "LakeEnsemblR_WQ.yaml",
  models = c("GLM-AED2", "GOTM-WET", "GOTM-Selmaprotbas", "SIMSTRAT-AED2"),
  folder = ".",
  validate = TRUE,
  verbose = TRUE
)
```

## 4) Compute metrics and statistics

``` r
metric_out <- cal_metrics(
  metric_yaml_file = "Output.yaml",
  model_filter = "all",
  wq_config_file = "LakeEnsemblR_WQ.yaml"
)

stat_out <- cal_stats(metric_out)
```

## 5) Calibration workflow

### 5.1 Generate calibration tables

[`create_calibration_tables()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/create_calibration_tables.md)
creates: - one master file with all module/model parameters - one
editable file per active module

``` r
create_calibration_tables(
  folder = ".",
  config_file = "LakeEnsemblR_WQ.yaml",
  folder_out = "calibration",
  models_coupled = c("GLM-AED2", "GOTM-WET", "GOTM-Selmaprotbas", "SIMSTRAT-AED2"),
  bounds_factor = 0.2
)
```

Edit module files in the calibration folder: - set include = TRUE for
selected parameters - adjust lower, upper, and initial as needed

### 5.2 Build calib_setup from edited tables

``` r
cs_all <- calib_setup_from_tables(
  folder_in = "calibration",
  model_coupled = c("GLM-AED2", "GOTM-WET", "GOTM-Selmaprotbas", "SIMSTRAT-AED2")
)
```

### 5.3 Run small LHC test per model

Run one model at a time.

``` r
cs_glm <- subset(cs_all, model_coupled == "GLM-AED2")

res_glm <- run_lhc_wq(
  model = "GLM-AED2",
  param_names = unique(cs_glm$pars),
  calib_setup = cs_glm,
  yaml_file = "Output.yaml",
  model_dir = "GLM-AED2",
  n_samples = 3,
  wq_config_file = "LakeEnsemblR_WQ.yaml",
  verbose = TRUE
)
```

## 6) Sensitivity analysis

``` r
res_sens <- run_sensitivity(
  param_name = "theta_sed_oxy",
  calib_setup = cs_glm,
  yaml_file = "Output.yaml",
  model_dir = "GLM-AED2",
  n_steps = 10,
  model_filter = "GLM"
)
```

## Troubleshooting

### No metrics found for selected model

Use coupling-specific model_filter values in metrics extraction: -
GLM-AED2 -\> GLM - GOTM-WET -\> WET - GOTM-Selmaprotbas -\>
SELMAPROTBAS - SIMSTRAT-AED2 -\> SIMSTRAT

### Unsupported file type in run_lhc_wq

Calibration tables may store dictionary-style paths (for example,
section/parameter) instead of physical filenames. Current run_lhc_wq
updates those dictionary paths for GLM-AED2, GOTM-WET,
GOTM-Selmaprotbas, and SIMSTRAT-AED2.

## Build package docs

``` r
# Regenerate Rd files from roxygen comments
devtools::document()

# Build the website (in docs/)
pkgdown::build_site()
```
