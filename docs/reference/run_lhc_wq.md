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
  obs_file = NULL
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

## Value

If `obs_file = NULL`, a list of length `n_samples` with sampled
parameters and metrics per run. If `obs_file` is supplied, returns a
flattened data frame with sampled parameters and summary statistics.
