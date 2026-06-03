# Run Latin Hypercube Calibration in Parallel

Runs
[`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)
in parallel by distributing LHS samples across multiple workers. Each
worker executes its assigned samples sequentially on the shared model
directory.

## Usage

``` r
run_lhc_wq_parallel(
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
  n_workers = NULL,
  parallel_dir = tempdir(),
  keep_worker_dirs = FALSE
)
```

## Arguments

- model:

  Character. One of `"GLM-AED2"`, `"GOTM-WET"`, `"GOTM-Selmaprotbas"`,
  or `"Simstrat-AED2"`.

- param_names:

  Character vector. Parameter names to vary.

- calib_setup:

  Data frame with calibration bounds and target files.

- yaml_file:

  Character. Path to the output metrics YAML file.

- model_dir:

  Character. Path to the model simulation directory.

- n_samples:

  Integer. Number of LHS samples to run.

- model_filter:

  Character or `NULL`. Optional model key for
  [`cal_metrics()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md).

- wq_config_file:

  Character or `NULL`. Path to WQ config file.

- yaml_file_model:

  Character. GOTM yaml filename.

- par_file:

  Character. Simstrat par filename.

- verbose:

  Logical. Print progress messages.

- save_results:

  Logical. If `TRUE`, save the combined result to `output_file` in the
  original `model_dir`.

- output_file:

  Character. RDS output filename when `save_results = TRUE`.

- obs_file:

  Character or `NULL`. Optional observed-data CSV.

- obs_to_model_units:

  Logical. Passed to
  [`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)
  when `obs_file` is provided. If `TRUE` (default), observed values are
  converted from harmonized/global units back to model-specific units
  before computing statistics.

- spin_up_days:

  Numeric or `NULL`. Passed to
  [`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md).
  Number of days after simulation start to exclude from observed-data
  comparison in `obs_file` mode.

- stats_by_depth:

  Logical. Passed to
  [`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)
  in `obs_file` mode. If `TRUE`, compute depth-wise statistics.

- return_best:

  Logical. Passed to
  [`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)
  in `obs_file` mode. If `TRUE`, mark the best parameter set.

- best_metric:

  Character. Objective metric used when `return_best = TRUE`. One of
  `"KGE"`, `"NSE"`, `"RMSE"`, `"NRMSE"`, or `"PBIAS"`.

- n_workers:

  Integer. Number of parallel workers. Defaults to all physical cores
  minus one, capped at `n_samples`.

- parallel_dir:

  Character. Ignored (for backward compatibility).

- keep_worker_dirs:

  Logical. Ignored (for backward compatibility).

## Value

Same structure as
[`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md),
with results combined across workers in iteration order.
