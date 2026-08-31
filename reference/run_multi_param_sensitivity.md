# Run Multi-Parameter Sensitivity Analysis Using Latin Hypercube Sampling

This function performs a multi-parameter sensitivity analysis using
Latin Hypercube Sampling (LHS): at each iteration, ALL parameters in
`param_names` are varied simultaneously (unlike
[`run_sensitivity`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_sensitivity.md),
which varies one parameter at a time). It modifies the input files for a
coupled lake model, runs the model, and extracts either
[`cal_metrics()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
output or raw model output for each iteration.

## Usage

``` r
run_multi_param_sensitivity(
  param_names = NULL,
  calib_setup,
  rel_change = NULL,
  yaml_file,
  model_dir,
  n_steps = 10,
  model = "GLM-AED2",
  model_filter = NULL,
  yaml_file_model = NULL,
  par_file = NULL,
  wq_config_file = NULL,
  output_mode = "metrics",
  vars = NULL,
  obs_depths = NULL,
  depth_01 = 1,
  conversion_factor = 1,
  target_variable = NULL,
  verbose = TRUE
)
```

## Arguments

- param_names:

  Character vector or `NULL`. Parameter names to vary in the sensitivity
  analysis. May contain duplicate names (e.g. one physical parameter
  calibrated independently for two phytoplankton groups); each
  occurrence is matched to its own row in `calib_setup` in order, the
  same way
  [`run_lhc_wq`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)
  disambiguates duplicate `pars` names. `NULL` (default) uses every row
  of `calib_setup$pars` as-is, in order – i.e. by default every
  parameter in `calib_setup` is varied.

- calib_setup:

  A data frame containing calibration setup information. Must include
  columns `pars` (parameter names), `file`/path, and `lb`/`ub` (used
  when `rel_change = NULL`) or `x0` (used when `rel_change` is
  supplied); optionally `group_name` for group-specific CSV columns or
  FABM instance paths.

- rel_change:

  Numeric or `NULL`. When supplied, bounds are `x0 * (1 - rel_change)`
  to `x0 * (1 + rel_change)` for every parameter (e.g. `0.1` for +/-10
  percent). When `NULL` (default), bounds come from
  `calib_setup$lb`/`ub`.

- yaml_file:

  Path to the YAML file used for metric extraction by
  [`cal_metrics()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)/
  model-folder resolution by
  [`get_output_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/get_output_wq.md)
  (output.yaml).

- model_dir:

  Path to the directory containing the lake model files and
  subdirectories.

- n_steps:

  Number of LHS iterations (i.e., model realizations).

- model:

  Character. One of `"GLM-AED2"`, `"GOTM-WET"`, `"GOTM-Selmaprotbas"`,
  or `"Simstrat-AED2"`. Determines both how each parameter is written to
  its target file and which model engine is run. Default `"GLM-AED2"`
  for backwards compatibility.

- model_filter:

  Character or `NULL`. Model identifier used by
  [`cal_metrics()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)/
  [`get_output_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/get_output_wq.md).
  If `NULL` (default), auto-derived from `model`.

- yaml_file_model:

  Character or `NULL`. GOTM yaml filename, only used/required when
  `model` is `"GOTM-WET"` or `"GOTM-Selmaprotbas"`. `NULL` (default)
  falls back to `"gotm.yaml"`.

- par_file:

  Character or `NULL`. Simstrat par filename, only used/required when
  `model` is `"Simstrat-AED2"`. `NULL` (default) falls back to
  `"simstrat.par"`.

- wq_config_file:

  Character or `NULL`. Path to the LakeEnsemblR_WQ config file, passed
  through to
  [`cal_metrics()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md).
  Required when `output_mode = "metrics"`; ignored when
  `output_mode = "raw"`.

- output_mode:

  Character. `"metrics"` (default) runs each step through
  [`cal_metrics()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
  (requires `wq_config_file`). `"raw"` instead pulls one or more model
  output variables directly via
  [`get_output_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/get_output_wq.md)
  (requires `vars`), bypassing the metrics dictionary/template machinery
  entirely.

- vars:

  Character vector. Required when `output_mode = "raw"` – the model
  output variable name(s) to extract at each step, passed through to
  [`get_output_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/get_output_wq.md).

- obs_depths:

  Numeric vector or `NULL`. Only used when `output_mode = "raw"`; passed
  through to
  [`get_output_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/get_output_wq.md).

- depth_01:

  Integer. Only used when `output_mode = "raw"`; passed through to
  [`get_output_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/get_output_wq.md).

- conversion_factor:

  Numeric. Only used when `output_mode = "raw"`; passed through to
  [`get_output_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/get_output_wq.md).

- target_variable:

  Character vector or `NULL`. Only used when `output_mode = "metrics"`.
  If supplied, filters the
  [`cal_metrics()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
  output for each step down to just these metric name(s) instead of
  returning every metric in `yaml_file`.

- verbose:

  Logical. Print progress messages and pass through to the model
  engine's own `verbose` argument. Default `TRUE`.

## Value

A list of length `n_steps`. Each element contains `params` (the named
list of sampled values for that iteration) plus either `metrics`
(`output_mode = "metrics"`) or `output` (`output_mode = "raw"`) – see
[`run_sensitivity`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_sensitivity.md)
for the exact shape of each.

## Details

Parameter sampling is based on Latin Hypercube Sampling (via
[`lhs::randomLHS`](https://rdrr.io/pkg/lhs/man/randomLHS.html)). Bounds
are, per parameter, either `x0 * (1 +/- rel_change)` (when `rel_change`
is supplied) or `calib_setup`'s own `lb`/`ub` columns (when
`rel_change = NULL`, the default) – the same bounds convention
[`run_lhc_wq`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)/[`run_sensitivity`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_sensitivity.md)
use.

## Examples

``` r
if (FALSE) { # \dontrun{
# Joint sensitivity of DO to 10 parameters at once, GOTM-WET, raw output
results <- run_multi_param_sensitivity(
  param_names = c("hO2Nitr", "kNitrW", "cTurbDifO2", "kO2Dif", "fRedMaxS",
                  "kDMinW", "kDMinS", "hO2BOD", "cMuMax", "kDResp"),
  calib_setup = calib_setup,
  yaml_file   = "Output.yaml",
  model_dir   = "GOTM-WET",
  n_steps     = 20,
  model       = "GOTM-WET",
  output_mode = "raw",
  vars        = "sO2W"
)
} # }
```
