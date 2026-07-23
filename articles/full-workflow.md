# LakeEnsemblR.WQ Full Workflow

## Purpose

This vignette provides an end-to-end workflow for LakeEnsemblR.WQ:

1.  Configure coupled model inputs.
2.  Validate model setups.
3.  Run model ensembles.
4.  Compute harmonized metrics.
5.  Prepare and run calibration – single-model LHC, optional DE
    refinement, writing best parameters back, and calibrating all
    coupled models at once (sequentially or concurrently) with
    [`cali_ensemble_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cali_ensemble_wq.md).
6.  Run sensitivity analysis.
7.  Compare and visualize model outputs against each other and
    observations.

## Prerequisites

- A physical LakeEnsemblR setup already exported.
- A water-quality config file: LakeEnsemblR_WQ.yaml.
- A metrics config file: Output.yaml.
- Supported model folders, for example:
  - GLM-AED2
  - GOTM-WET
  - GOTM-Selmaprotbas
  - SIMSTRAT-AED2
- For calibration against observations (Section 5): an observed-data CSV
  with columns `datetime`, `depth`, `variable_global_name`, `value`.

``` r

library(LakeEnsemblR)
library(LakeEnsemblR.WQ)
```

## 1) Export WQ configuration and inputs

``` r

export_config_wq("LakeEnsemblR_WQ.yaml")
```

## 2) Validate model setups

``` r

validate_glm_aed(config_file = "LakeEnsemblR_WQ.yaml", folder = ".")
validate_gotm_wet(config_file = "LakeEnsemblR_WQ.yaml", folder = ".")
validate_simstrat(config_file = "LakeEnsemblR_WQ.yaml", folder = ".")
```

[`run_ensemble_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_ensemble_wq.md)
(next section) can also run these automatically via its own
`validate = TRUE` argument – calling them directly here is useful when
you want validation errors surfaced before committing to a full ensemble
run.

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
```

## 5) Calibration workflow

### 5.1 Generate calibration tables

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
  folder_in = "calibration-test",
  model_coupled = c("GLM-AED2", "GOTM-WET", "GOTM-Selmaprotbas", "SIMSTRAT-AED2")
)
```

### 5.3 Run small LHC test per model

Run one model at a time.
[`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)
samples the parameter space with Latin Hypercube Sampling and, for each
sample, edits the model’s own config files in `model_dir` in place, runs
the model, and scores it. If `obs_file` is supplied it returns a
long-format data frame (one row per iteration x observed variable) with
NSE/RMSE/NRMSE/PBIAS/KGE; otherwise it falls back to
[`cal_metrics()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)-derived
metrics only.

``` r

cs_glm <- subset(cs_all, model_coupled == "GLM-AED2")

res_glm <- run_lhc_wq(
  model          = "GLM-AED2",
  param_names    = unique(cs_glm$pars),
  calib_setup    = cs_glm,
  yaml_file      = "Output.yaml",
  model_dir      = "GLM-AED2",
  n_samples      = 3,
  wq_config_file = "LakeEnsemblR_WQ.yaml",
  obs_file       = "obs_data.csv",   # datetime, depth, variable_global_name, value
  best_metric    = "KGE",
  verbose        = TRUE
)

# Note: model_dir's config files are restored to their pre-calibration state
# once run_lhc_wq() returns (including on error) -- the function snapshots
# model_dir before sampling and copies that snapshot back via on.exit(). Your
# working files are not left mutated at the last-tried parameter set.

head(res_glm)
attr(res_glm, "best_parameter_set")   # winning parameter set for this model
```

### 5.4 Optional: refine with Differential Evolution (DE)

Set `use_de = TRUE` to run DEoptim after the LHC phase, seeded from the
best LHC results. When DE is used, `attr(res_glm, "best_parameter_set")`
is overwritten with DE’s refined optimum (DE improves on the LHC seed) –
the original LHC-phase best is still available under
`attr(res_glm, "lhc_best_parameter_set")` for comparison.

``` r

res_glm_de <- run_lhc_wq(
  model            = "GLM-AED2",
  param_names      = unique(cs_glm$pars),
  calib_setup      = cs_glm,
  yaml_file        = "Output.yaml",
  model_dir        = "GLM-AED2",
  n_samples        = 20,
  wq_config_file   = "LakeEnsemblR_WQ.yaml",
  obs_file         = "obs_data.csv",
  best_metric      = "KGE",
  use_de           = TRUE,
  de_iterations    = 30,
  de_seed_from_lhc = TRUE,
  verbose          = TRUE
)

attr(res_glm_de, "best_parameter_set")   # DE-refined best (used for write-back below)
```

### 5.5 Write the best parameters back

[`write_best_calib_to_par_files()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/write_best_calib_to_par_files.md)
takes the best row (LHC’s, or DE’s if `use_de = TRUE`) and writes it
into the module `par_file` CSVs referenced by `LakeEnsemblR_WQ.yaml` (or
directly into model config files with `write_target = "config"`). Re-run
[`export_config_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/export_config_wq.md)
afterward if you need those values pushed all the way into the model
folders for a final verification run.

``` r

write_best_calib_to_par_files(
  lhc_results  = res_glm_de,
  calib_setup  = cs_glm,
  config_file  = "LakeEnsemblR_WQ.yaml",
  folder       = ".",
  metric       = "KGE",
  write_target = "par_file"
)
```

### 5.6 Calibrate all coupled models with cali_ensemble_wq()

[`cali_ensemble_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cali_ensemble_wq.md)
is the multi-model wrapper around
[`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md):
pass it the full `models` vector and the combined `cs_all` table (split
automatically by `model_coupled`) and it runs, scores, and optionally
writes back the best parameters for every model in one call.

``` r

models_coupled <- c("GLM-AED2", "GOTM-WET", "GOTM-Selmaprotbas", "Simstrat-AED2")

result <- cali_ensemble_wq(
  models         = models_coupled,
  calib_setup    = cs_all,
  yaml_file      = "Output.yaml",
  folder         = ".",
  n_samples      = 20,
  obs_file       = "obs_data.csv",
  wq_config_file = "LakeEnsemblR_WQ.yaml",
  best_metric    = "KGE",
  use_de         = TRUE,
  de_iterations  = 30,
  verbose        = TRUE,
  on_error       = "skip",       # a failing model doesn't stop the others
  save_results   = TRUE,
  output_dir     = "calibration_results",
  write_best     = TRUE,
  write_target   = "par_file",
  config_file    = "LakeEnsemblR_WQ.yaml"
)

result$summary                              # one row per model: success, n_rows, n_stats
result$best_parameter_sets[["GLM-AED2"]]     # winning parameters for one model
result$write_back[["GLM-AED2"]]              # whether the write-back succeeded
```

By default the four models run sequentially. Since each model has its
own `model_dir`, they can safely run **concurrently** instead, one
worker process per model:

``` r

result_parallel <- cali_ensemble_wq(
  models          = models_coupled,
  calib_setup     = cs_all,
  yaml_file       = "Output.yaml",
  folder          = ".",
  n_samples       = 20,
  obs_file        = "obs_data.csv",
  wq_config_file  = "LakeEnsemblR_WQ.yaml",
  best_metric     = "KGE",
  parallel_models = TRUE,   # run all requested models at the same time
  n_model_workers = 4,      # one worker process per model
  verbose         = TRUE,
  save_results    = TRUE,
  output_dir      = "calibration_results",
  write_best      = TRUE,
  config_file     = "LakeEnsemblR_WQ.yaml"
)
```

`parallel_models` stacks with `parallel`/`de_parallel` (which
parallelize LHC samples or DE evaluations *within* one model’s run) – if
you turn multiple of these on together, size worker counts so
`n_model_workers * n_workers` (or `* de_n_workers`) doesn’t
oversubscribe your CPU cores. Worker processes have no attached console,
so with `parallel_models = TRUE` progress messages are written to
`<output_dir>/cali_ensemble_wq_workers.log` instead of printing live –
tail that file to watch progress while it runs.

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

## 7) Compare and visualize model outputs

The functions below all plug directly into outputs you’ve already
produced earlier in this vignette:
[`cal_metrics()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)’s
nested list (Section 4) and the NetCDF file from
[`create_netcdf_output()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/create_netcdf_output.md).

### 7.1 Compare one metric across models

[`compare_models_metric()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/compare_models_metric.md)
takes the list returned by
[`cal_metrics()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
directly and line-plots one metric across all coupled models
(auto-detecting the depth/value columns from that structure).

``` r

metric_out <- cal_metrics(
  metric_yaml_file = "Output.yaml",
  model_filter = "all",
  wq_config_file = "LakeEnsemblR_WQ.yaml"
)

cmp <- compare_models_metric(
  metric_out = metric_out,
  metric     = "Temp_degreeCelcius",
  depth      = 1
)
cmp$plot
```

[`compare_models_metric_netcdf()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/compare_models_metric_netcdf.md)
does the same comparison but reads straight from a NetCDF file (see 7.3)
instead of a
[`cal_metrics()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
list – useful when you want to compare a variable that wasn’t included
in `Output.yaml`.

``` r

cmp_nc <- compare_models_metric_netcdf(
  nc_file = "output.nc",
  metric  = "Temp_degreeCelcius",
  depth   = 1
)
cmp_nc$plot
```

### 7.2 Stratification metrics across models and years

[`plot_strat_metrics()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/plot_strat_metrics.md)
accepts the same
[`cal_metrics()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
list (or a NetCDF path) and produces three plots per model:
stratification duration, onset day of year, and mixing onset day of
year.

``` r

strat <- plot_strat_metrics(metric_out, metric_name = "Duration_of_Stratification")
strat$duration_plot
strat$onset_plot
strat$mixing_plot
```

### 7.3 Depth-time heatmap from the ensemble NetCDF

[`create_netcdf_output()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/create_netcdf_output.md)
combines all coupled models’ output into a single NetCDF for easier
downstream analysis;
[`plot_heatmap_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/plot_heatmap_wq.md)
reads it directly to produce a depth-time heatmap faceted by model.
Repeat the
[`plot_heatmap_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/plot_heatmap_wq.md)
call for any metric name present in `Output.yaml`
(e.g. `DO_gramsPerCubicMeter`, `NH4_gramsPerCubicMeter`,
`TP_gramsPerCubicMeter`, `Total_Chla_miligramsPerCubicMeter`).

``` r

create_netcdf_output(
  output_lists    = metric_out,
  folder          = ".",
  model           = c("GLM", "SELMAPROTBAS", "WET", "SIMSTRAT"),
  ler_config_file = "LakeEnsemblR.yaml",
  wq_config_file  = "LakeEnsemblR_WQ.yaml",
  out_file        = "ensemble_output.nc"
)

plot_heatmap_wq("ensemble_output.nc", "Temp_degreeCelcius", spin_up = 30)
```

### 7.4 Anoxic and ice metrics across models and years

[`plot_anoxic_metrics()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/plot_anoxic_metrics.md)
and
[`plot_ice_metrics()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/plot_ice_metrics.md)
follow the same flexible input as
[`plot_strat_metrics()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/plot_strat_metrics.md)
(7.2) – a
[`cal_metrics()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
list or a NetCDF path – and plot their respective derived metrics per
model per year.

``` r

plot_anoxic_metrics("ensemble_output.nc")
plot_ice_metrics("ensemble_output.nc", metric_name = "Ice_Thickness_meter")
```

### 7.5 Per-model time series and scatter plots against observations

[`compare_plot()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/compare_plot.md)
and
[`scat_plot()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/scat_plot.md)
work at a lower level than the functions above: instead of a
[`cal_metrics()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
list, each takes one **wide-format** data frame per model (`datetime` +
`Depth_<n>` columns – the shape
[`get_output_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/get_output_wq.md)
returns) plus one for observations, and compute
[`cal_stats()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_stats.md)
against the observed values internally. They’re most useful once you
already have per-model output pulled out via
[`get_output_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/get_output_wq.md)
(see
[`?compare_plot`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/compare_plot.md)
and
[`?scat_plot`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/scat_plot.md)
for the full column/argument requirements) – for example:

``` r

data_glm <- get_output_wq(
  config_file = "Output.yaml",
  model       = "GLM",
  vars        = "temp",
  depth_01    = 1
)[[1]]

# ...similarly for data_wet, data_selma, data_simstrat, and data_obs

res <- compare_plot(
  data_glm, data_wet, data_selma, data_simstrat, data_obs,
  depth   = 1,
  y_title = "Temperature (°C)"
)
res[[1]]        # the plot
res[[2]]        # cal_stats() output for GLM at this depth
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

### “unused arguments” errors after editing R/ source files

If you’re developing against a local clone and see errors like
`unused arguments (de_parallel = ..., de_n_workers = ...)` for a
parameter that clearly exists in the function’s source, your session is
very likely running a stale **installed** copy of the package (loaded
via
[`library(LakeEnsemblR.WQ)`](https://github.com/aemon-j/LakeEnsemblR.WQ))
rather than your edited source. Reload from source instead:

``` r

devtools::load_all(".")   # or source() the changed R/ files directly
```

### No progress messages with parallel_models = TRUE

This is expected, not a hang – see the note at the end of Section 5.6.
Worker processes have no attached console, so check
`<output_dir>/cali_ensemble_wq_workers.log` instead.

## Build package docs

``` r

# Regenerate Rd files from roxygen comments
devtools::document()

# Build the website (in docs/)
pkgdown::build_site()
```
