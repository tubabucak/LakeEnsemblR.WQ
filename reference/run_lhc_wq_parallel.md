# Run Latin Hypercube Calibration in Parallel

Runs
[`run_lhc_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)
in parallel by distributing LHS samples across multiple workers. Each
worker gets its own isolated sandbox copy of `model_dir` (under
`parallel_dir`) and runs/scores its assigned samples there – `model_dir`
itself is never written to.

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
  yaml_file_model = NULL,
  par_file = NULL,
  ler_config_file = NULL,
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
  n_workers = NULL,
  use_de = FALSE,
  de_parallel = FALSE,
  de_n_workers = NULL,
  de_iterations = 50,
  de_popsize = NULL,
  de_f = 0.8,
  de_cr = 0.9,
  de_seed_from_lhc = TRUE,
  parallel_dir = NULL,
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
  [`cal_metrics()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md).

- wq_config_file:

  Character or `NULL`. Path to WQ config file.

- yaml_file_model:

  Character or `NULL`. GOTM yaml filename. If `NULL`, derived from
  `ler_config_file` (see
  [`run_lhc_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)).

- par_file:

  Character or `NULL`. Simstrat par filename. If `NULL`, derived from
  `ler_config_file` (see
  [`run_lhc_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)).

- ler_config_file:

  Character or `NULL`. Path to the LakeEnsemblR config file, used to
  auto-derive `yaml_file_model`/`par_file` when they are not explicitly
  set. Passed through to
  [`run_lhc_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md).

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
  [`run_lhc_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)
  when `obs_file` is provided. If `TRUE` (default), observed values are
  converted from harmonized/global units back to model-specific units
  before computing statistics.

- spin_up_days:

  Numeric or `NULL`. Passed to
  [`run_lhc_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md).
  Number of days after simulation start to exclude from observed-data
  comparison in `obs_file` mode.

- stats_by_depth:

  Logical. Passed to
  [`run_lhc_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)
  in `obs_file` mode. If `TRUE`, compute depth-wise statistics.

- return_best:

  Logical. Passed to
  [`run_lhc_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)
  in `obs_file` mode. If `TRUE`, mark the best parameter set.

- best_metric:

  Character. Objective metric used when `return_best = TRUE`. One of
  `"KGE"`, `"NSE"`, `"RMSE"`, `"NRMSE"`, or `"PBIAS"`.

- target_variables:

  Character vector or `NULL`. Passed to
  [`run_lhc_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)
  – restricts scoring to these `variable_global_name` value(s) from
  `obs_file` instead of averaging across every observed variable. `NULL`
  (default) uses all.

- n_workers:

  Integer. Number of parallel workers. Defaults to all physical cores
  minus one, capped at `n_samples`.

- use_de:

  Logical. If `TRUE`, run a Differential Evolution refinement phase (via
  [`run_lhc_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md))
  after the parallel LHC phase completes, seeded from the LHC results
  already computed here.

- de_parallel:

  Logical. Passed to
  [`run_lhc_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)'s
  DE phase – parallelizes DE evaluations across workers.

- de_n_workers:

  Integer or `NULL`. Number of workers for the DE phase when
  `de_parallel = TRUE`.

- de_iterations:

  Integer. Number of DE generations. Passed to
  [`run_lhc_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)'s
  DE phase.

- de_popsize:

  Integer or `NULL`. DE population size. Passed to
  [`run_lhc_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)'s
  DE phase.

- de_f:

  Numeric. DE differential weight. Passed to
  [`run_lhc_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)'s
  DE phase.

- de_cr:

  Numeric. DE crossover probability. Passed to
  [`run_lhc_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)'s
  DE phase.

- de_seed_from_lhc:

  Logical. If `TRUE` (default), seed the DE population from the best
  parallel-LHC results instead of re-sampling.

- parallel_dir:

  Character or `NULL`. Parent directory for the isolated
  `worker_<pid>_<timestamp>_N/` sandbox copies of `model_dir`. If `NULL`
  (default), uses `dirname(model_dir)` – a sibling of `model_dir` under
  the project root, so relative parent-path references in model config
  files (e.g. shared meteo forcing files) still resolve correctly.

- keep_worker_dirs:

  Logical. If `TRUE`, worker sandbox directories are left on disk after
  the run instead of being deleted – useful for debugging, but
  accumulates disk usage across repeated runs. Default `FALSE` (deleted
  automatically).

## Value

Same structure as
[`run_lhc_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md),
with results combined across workers in iteration order.
