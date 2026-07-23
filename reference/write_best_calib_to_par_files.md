# Write best calibration parameter set back to par_file CSVs

After running
[`run_lhc_wq`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)
with `obs_file` supplied (which returns a data frame of parameter
values + performance statistics), this function picks the
best-performing row and writes the calibrated parameter values into the
`par_file` CSVs referenced in the `LakeEnsemblR_WQ.yaml` config. This
closes the loop between the calibration workflow and the
parameter-override files consumed by
[`export_config_wq`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/export_config_wq.md).

## Usage

``` r
write_best_calib_to_par_files(
  lhc_results,
  calib_setup,
  config_file,
  folder = ".",
  metric = "KGE",
  minimize = FALSE,
  write_target = c("par_file", "config"),
  verbose = TRUE
)
```

## Arguments

- lhc_results:

  data.frame; the output of
  [`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)
  when `obs_file` is supplied. Must contain one column per calibrated
  parameter and at least one performance-metric column (e.g. `NSE`,
  `KGE`).

- calib_setup:

  data.frame; the calibration setup table used when running
  [`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md),
  as produced by
  [`calib_setup_from_tables`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/calib_setup_from_tables.md).
  Must contain columns `pars`, `model_coupled`, `domain`, `process`,
  `subprocess`, and optionally `group_name`.

- config_file:

  character; name of the `LakeEnsemblR_WQ.yaml` master config file.

- folder:

  character; path to the directory containing `config_file`. Defaults to
  `"."`.

- metric:

  character; name of the performance metric column in `lhc_results` to
  use for selecting the best run. Defaults to `"KGE"`. The metric column
  name may be prefixed by a variable name (e.g. `"oxygen_NSE"`); the
  function looks for an exact match first, then falls back to columns
  that contain `metric`.

- minimize:

  logical; if `TRUE` the row with the *lowest* metric value is chosen
  (e.g. RMSE). If `FALSE` (default) the row with the *highest* value is
  chosen (e.g. KGE, NSE).

- write_target:

  character; where to write best values. One of `"par_file"` (default,
  updates module `par_file` CSVs) or `"config"` (uses
  [`set_value_config()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/set_value_config.md)
  to write directly to model config files).

- verbose:

  logical; if `TRUE` print a message for each parameter that is updated.
  Defaults to `TRUE`.

## Value

Invisibly returns a named list of the best parameter values that were
written.

## Details

The function resolves each calibrated parameter to its `par_file` CSV by
matching against `model_coupled`, `domain`, `process`, `subprocess`, and
`parameter` columns in the CSV. For biological group modules
(phytoplankton, zooplankton, etc.) the `group_name` column in
`calib_setup` is used to identify the correct group CSV.

Parameters present in `calib_setup` but not found as a matching row in
the target CSV will generate a warning (not an error) so that the
remaining parameters are still written.

## Examples

``` r
if (FALSE) { # \dontrun{
calib_setup <- calib_setup_from_tables(
  folder_in     = "calibration",
  model_coupled = "GOTM-Selmaprotbas"
)

lhc_results <- run_lhc_wq(
  model          = "GOTM-Selmaprotbas",
  param_names    = calib_setup$pars,
  calib_setup    = calib_setup,
  yaml_file      = "metrics.yaml",
  model_dir      = "GOTM-Selmaprotbas",
  n_samples      = 50,
  wq_config_file = "LakeEnsemblR_WQ.yaml",
  obs_file       = "observed_data.csv"
)

write_best_calib_to_par_files(
  lhc_results  = lhc_results,
  calib_setup  = calib_setup,
  config_file  = "LakeEnsemblR_WQ.yaml",
  folder       = ".",
  metric       = "KGE"
)
} # }
```
