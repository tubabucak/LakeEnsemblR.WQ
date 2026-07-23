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
  target_variables = NULL,
  parallel = FALSE,
  n_workers = NULL,
  parallel_dir = tempdir(),
  keep_worker_dirs = FALSE,
  lhs_matrix = NULL,
  sample_indices = NULL,
  use_de = FALSE,
  de_iterations = 50,
  de_popsize = NULL,
  de_f = 0.8,
  de_cr = 0.9,
  de_seed_from_lhc = TRUE,
  de_parallel = FALSE,
  de_n_workers = NULL
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

- target_variables:

  Character. Vector of variables to be included in the objective
  function

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

- use_de:

  Logical. If `TRUE`, run differential evolution after LHC
  initialization using LHC results as generation 1. Default is `FALSE`
  (LHC-only mode maintains backward compatibility).

- de_iterations:

  Integer. Number of DE generations to run (default = 50). Ignored when
  `use_de = FALSE`.

- de_popsize:

  Integer or `NULL`. DE population size. If `NULL` (default), uses
  `n_samples`. Must be \>= 4 for DEoptim. Ignored when `use_de = FALSE`.

- de_f:

  Numeric. Mutation scaling factor in range \[0.2, 2.0\]. Higher values
  increase mutation magnitude. Default is 0.8. Ignored when
  `use_de = FALSE`.

- de_cr:

  Numeric. Crossover probability in range \[0, 1\]. Higher values
  increase parameter exchange probability. Default is 0.9. Ignored when
  `use_de = FALSE`.

- de_seed_from_lhc:

  Logical. Initialize DE population from best LHC results (default =
  `TRUE`). If `FALSE`, DE starts from random population. Ignored when
  `use_de = FALSE`.

## Value

If `obs_file = NULL`, a list of length `n_samples` with sampled
parameters and metrics per run. When `use_de = TRUE`, includes
additional `de_phase` element with DEoptim results. If `obs_file` is
supplied, returns a flattened data frame with sampled parameters and
summary statistics. In `obs_file` mode and when `return_best = TRUE`,
the returned data.frame includes column `is_best`, and attributes
`best_parameter_set` and `best_metric` (identifying the best LHC
iteration). When `use_de = TRUE` is also set, `best_parameter_set` and
`best_metric` are overwritten with DE's refined optimum (DE runs after
and improves on the LHC seed) – this is the attribute read by
[`write_best_calib_to_par_files()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/write_best_calib_to_par_files.md)
and by `cali_ensemble_wq()$best_parameter_sets`, so both automatically
use the DE result when DE was run. The original LHC-phase best is
preserved under attribute `lhc_best_parameter_set` for comparison. The
DEoptim object itself and its raw best member are always available via
attributes `de_phase` and `de_best_params`.
