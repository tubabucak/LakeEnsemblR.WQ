# Calibrate an ensemble of LakeEnsemblR.WQ models

High-level calibration wrapper inspired by LakeEnsemblR
[`cali_ensemble()`](https://aemon-j.github.io/LakeEnsemblR/reference/cali_ensemble.html),
implemented for the LakeEnsemblR.WQ calibration workflow. The function
runs
[`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)
model-by-model, using either a combined `calib_setup` table (with
`model_coupled`) or a named list of per-model setup tables.

## Usage

``` r
cali_ensemble_wq(
  models = c("GLM-AED2", "GOTM-WET", "GOTM-Selmaprotbas", "Simstrat-AED2"),
  calib_setup,
  yaml_file,
  folder = ".",
  model_dirs = NULL,
  param_names = NULL,
  model_filter = NULL,
  wq_config_file = NULL,
  n_samples = 50,
  yaml_file_model = "gotm.yaml",
  par_file = "simstrat.par",
  obs_file = NULL,
  obs_to_model_units = TRUE,
  spin_up_days = NULL,
  stats_by_depth = FALSE,
  return_best = TRUE,
  best_metric = "KGE",
  parallel = FALSE,
  n_workers = NULL,
  parallel_dir = tempdir(),
  keep_worker_dirs = FALSE,
  verbose = TRUE,
  on_error = c("skip", "stop"),
  save_results = FALSE,
  output_dir = NULL,
  output_prefix = "lhc_results",
  write_best = FALSE,
  write_target = c("par_file", "config"),
  config_file = NULL
)
```

## Arguments

- models:

  Character vector of models to calibrate. Supported values are
  `"GLM-AED2"`, `"GOTM-WET"`, `"GOTM-Selmaprotbas"`, and
  `"Simstrat-AED2"`. Matching is case-insensitive.

- calib_setup:

  Either a data frame in `calib_setup` format or a named list of such
  data frames. If a data frame is supplied and contains `model_coupled`,
  rows are split by model.

- yaml_file:

  Character scalar, vector, or named list. Path(s) to metric YAML
  file(s) passed to
  [`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md).

- folder:

  Character. Base folder used to resolve relative paths and for optional
  write-back operations.

- model_dirs:

  Character vector/list of model directories. Can be scalar (reused),
  length `length(models)` (positional), or named by model. Defaults to
  `file.path(folder, models)`.

- param_names:

  Optional. Either a character vector reused for all models, or a named
  list/vector by model. If `NULL`, uses `unique(calib_setup$pars)` per
  model.

- model_filter:

  Optional model filter(s) passed to
  [`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md).
  Accepts scalar, length-`length(models)` vector, or named vector/list.

- wq_config_file:

  Character scalar, vector, or named list of WQ config path(s) passed to
  [`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md).

- n_samples:

  Integer. Number of LHS samples per model.

- yaml_file_model:

  Character scalar/vector/list. GOTM YAML file name(s) passed to
  [`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md).

- par_file:

  Character scalar/vector/list. Simstrat `.par` file name(s) passed to
  [`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md).

- obs_file:

  Optional observed-data CSV path. When supplied, each model returns the
  flattened stats data.frame from
  [`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md).

- obs_to_model_units:

  Logical. Passed to
  [`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md).

- spin_up_days:

  Numeric or `NULL`. Passed to
  [`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md).

- stats_by_depth:

  Logical. Passed to
  [`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md).

- return_best:

  Logical. Passed to
  [`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md).

- best_metric:

  Character. Passed to
  [`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md).

- parallel:

  Logical. Passed to
  [`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md).

- n_workers:

  Integer or `NULL`. Passed to
  [`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md).

- parallel_dir:

  Character. Passed to
  [`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md).

- keep_worker_dirs:

  Logical. Passed to
  [`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md).

- verbose:

  Logical. Print progress messages.

- on_error:

  Character. One of `"skip"` or `"stop"`.

- save_results:

  Logical. If `TRUE`, save each model result as RDS.

- output_dir:

  Character. Directory for saved RDS files when `save_results = TRUE`.
  Defaults to `folder`.

- output_prefix:

  Character. Prefix for saved result files when `save_results = TRUE`.

- write_best:

  Logical. If `TRUE`, call
  [`write_best_calib_to_par_files()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/write_best_calib_to_par_files.md)
  for each successful model.

- write_target:

  Character. Passed to
  [`write_best_calib_to_par_files()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/write_best_calib_to_par_files.md).

- config_file:

  Character. Required when `write_best = TRUE`; path to the WQ master
  config used by
  [`write_best_calib_to_par_files()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/write_best_calib_to_par_files.md).

## Value

A list with:

- results:

  Named list of per-model outputs from
  [`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md).

- summary:

  Data frame with one row per requested model and run status.

- best_parameter_sets:

  Named list of per-model best summaries (or `NULL`).

- write_back:

  Named list with best-write status per model (when enabled).
