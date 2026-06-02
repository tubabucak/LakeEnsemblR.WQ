# Run Latin Hypercube Calibration for Water Quality Models

Performs Latin Hypercube Sampling (LHS) based parameter-space
exploration for coupled water-quality model setups and evaluates each
sampled run using either
[`cal_metrics()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
outputs or observed-data statistics.

## Usage

``` r
run_lhc_wq(
  model,
  param_names,
  calib_setup,
  yaml_file,
  model_dir,
  n_samples = 50,
  model_filter = NULL,
  wq_config_file = NULL,
  yaml_file_model = "gotm.yaml",
  par_file = "simstrat.par",
  verbose = TRUE,
  save_results = FALSE,
  output_file = "lhc_results.rds",
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
  lhs_matrix = NULL,
  sample_indices = NULL
)
```

## Arguments

- model:

  Character. One of `"GLM-AED2"`, `"GOTM-WET"`, `"GOTM-Selmaprotbas"`,
  or `"Simstrat-AED2"`.

- param_names:

  Character vector. Parameter names to vary.

- calib_setup:

  Data frame with at least columns `pars`, `lb`, `ub`, and `file`;
  optional `group_name` for group-specific CSV updates.

- yaml_file:

  Character. Path to output metrics YAML file.

- model_dir:

  Character. Path to model simulation directory.

- n_samples:

  Integer. Number of LHS samples (default = 50).

- model_filter:

  Character or `NULL`. Optional model key for
  [`cal_metrics()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md).
  If `NULL`, it is auto-derived from `model`.

- wq_config_file:

  Character or `NULL`. Path to WQ config file.

- yaml_file_model:

  Character. GOTM yaml filename (default `"gotm.yaml"`).

- par_file:

  Character. Simstrat par filename (default `"simstrat.par"`).

- verbose:

  Logical. Print progress messages.

- save_results:

  Logical. If `TRUE`, save results to `output_file` in `model_dir`.

- output_file:

  Character. RDS output filename when `save_results = TRUE`.

- obs_file:

  Character or `NULL`. Optional observed-data CSV for per-run
  statistics.

- obs_to_model_units:

  Logical. When `TRUE` and `obs_file` is provided, observed values
  (assumed harmonized/global units) are converted back to model-specific
  units using dictionary `conversion_factor` before computing
  statistics. Default is `TRUE` (model outputs are kept in
  model-specific units for comparison).

- spin_up_days:

  Numeric or `NULL`. Optional number of days after simulation start to
  exclude from observed-data comparison in `obs_file` mode. This is
  useful to ignore model spin-up transients. Default is `NULL` (no
  spin-up exclusion).

- stats_by_depth:

  Logical. Only used when `obs_file` is provided. If `TRUE`,
  depth-resolved variables return one set of statistics per depth
  instead of one aggregated set per variable. Default is `FALSE`.

- return_best:

  Logical. Only used when `obs_file` is provided. If `TRUE`, identify
  the best parameter set across iterations using `best_metric`. Default
  is `TRUE`.

- best_metric:

  Character. Objective metric used to rank parameter sets when
  `return_best = TRUE`. One of `"KGE"`, `"NSE"`, `"RMSE"`, `"NRMSE"`, or
  `"PBIAS"`. Default is `"KGE"`.

- parallel:

  Logical. If `TRUE`, run in parallel by delegating to
  [`run_lhc_wq_parallel()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq_parallel.md).
  Default is `FALSE`.

- n_workers:

  Integer or `NULL`. Number of workers used when `parallel = TRUE`.
  Passed to
  [`run_lhc_wq_parallel()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq_parallel.md).

- parallel_dir:

  Character. Parent directory for worker copies when `parallel = TRUE`.
  Passed to
  [`run_lhc_wq_parallel()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq_parallel.md).

- keep_worker_dirs:

  Logical. Keep worker directories after completion when
  `parallel = TRUE`. Passed to
  [`run_lhc_wq_parallel()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq_parallel.md).

## Value

If `obs_file = NULL`, a list of length `n_samples` with sampled
parameters and metrics per run. If `obs_file` is supplied, returns a
flattened data frame with sampled parameters and summary statistics. In
`obs_file` mode and when `return_best = TRUE`, the returned data.frame
includes column `is_best`, and attributes `best_parameter_set` and
`best_metric`.
