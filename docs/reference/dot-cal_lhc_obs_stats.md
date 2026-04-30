# Run Latin Hypercube Calibration for Water Quality Models

Performs a Latin Hypercube Sampling (LHS) based parameter space
exploration for water quality models coupled to lake hydrodynamic
models. Supports GLM-AED2, GOTM-WET, GOTM-Selmaprotbas, and
Simstrat-AED2.

## Usage

``` r
.cal_lhc_obs_stats(
  obs_data,
  dict,
  model_short,
  yaml_file,
  wq_config_file = NULL
)
```

## Arguments

- yaml_file:

  Character. Path to the YAML metrics file passed to
  [`cal_metrics`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md).

- wq_config_file:

  Character. Path to the WQ config file passed to
  [`cal_metrics`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md).

- model:

  Character. One of `"GLM-AED2"`, `"GOTM-WET"`, `"GOTM-Selmaprotbas"`,
  or `"Simstrat-AED2"` (case-insensitive).

- param_names:

  Character vector. Names of parameters to vary (must match the `pars`
  column of `calib_setup`).

- calib_setup:

  Data frame. Calibration setup table with at minimum columns: `pars`
  (parameter names), `lb` (lower bound), `ub` (upper bound), `file`
  (path relative to `model_dir`), and optionally `group_name` (e.g. for
  phytoplankton groups in CSV parameter files).

- model_dir:

  Character. Path to the model simulation directory.

- n_samples:

  Integer. Number of LHS samples (model runs) to perform (default = 50).

- model_filter:

  Character. Optional model filter string passed to
  [`cal_metrics`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md).
  If `NULL` (default), auto-derived from `model` (e.g., "GLM-AED2" →
  "GLM", "GOTM-WET" → "WET", "GOTM-Selmaprotbas" → "SELMAPROTBAS").

- yaml_file_model:

  Character. Name of the GOTM yaml file used when running GOTM-based
  models (default = `"gotm.yaml"`). Ignored for GLM-AED2 and
  Simstrat-AED2.

- par_file:

  Character. Name of the Simstrat `.par` file (default =
  `"simstrat.par"`). Ignored for other models.

- verbose:

  Logical. Print progress messages (default = `TRUE`).

- save_results:

  Logical. If `TRUE`, save the results as an `.rds` file inside
  `model_dir` after all iterations (default = `FALSE`).

- output_file:

  Character. File name for the saved results when `save_results = TRUE`
  (default = `"lhc_results.rds"`).

- obs_file:

  Character or `NULL`. Path to the standard observed data CSV with
  columns `datetime`, `depth`, `variable_global_name`, and `value` (all
  values in global units as defined in the metrics dictionary). When
  provided, each model run is evaluated against the observations and
  only a flat `data.frame` of parameter values plus performance
  statistics is returned (default = `NULL`).

## Value

When `obs_file = NULL`: a list of length `n_samples`. Each element is a
list with:

- `params`:

  Named list of sampled parameter values for this iteration.

- `metrics`:

  Output of
  [`cal_metrics()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
  for this iteration.

- `model_ok`:

  Logical; `TRUE` if the model run produced output.

When `obs_file` is provided: a `data.frame` where each row is one
successful LHC iteration. Columns are the sampled parameter values
followed by performance statistics (NSE, RMSE, NRMSE, PBIAS, KGE) for
every variable–depth combination present in the observed data.

## Details

For each LHS iteration the function:

1.  Samples parameter values from the bounds defined in `calib_setup`.

2.  Writes the sampled values to the relevant `.nml` or `.csv` files.

3.  Runs the selected model.

4.  Extracts metrics via
    [`cal_metrics`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md).

## See also

[`cal_stats`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_stats.md)

## Examples

``` r
if (FALSE) { # \dontrun{
results <- run_lhc_wq(
  model       = "GLM-AED2",
  param_names = c("Kw", "Knitrif"),
  calib_setup = calib_setup,
  yaml_file   = "metrics.yaml",
  model_dir   = "GLM-AED2",
  n_samples   = 30,
  wq_config_file = "LakeEnsemblR_WQ.yaml"
)

results <- run_lhc_wq(
  model       = "GOTM-Selmaprotbas",
  param_names = c("spm_initial", "p_vel"),
  calib_setup = calib_setup,
  yaml_file   = "metrics.yaml",
  model_dir   = "GOTM-Selmaprotbas",
  n_samples   = 50,
  wq_config_file = "LakeEnsemblR_WQ.yaml"
)
} # }
```
