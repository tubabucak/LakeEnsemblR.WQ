# Build calib_setup from edited calibration CSVs

Reads the per-module calibration CSV files produced by
[`create_calibration_tables`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/create_calibration_tables.md),
filters to rows where `include == TRUE`, and returns a `calib_setup`
data frame in the format expected by
[`run_lhc_wq`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)
and
[`run_sensitivity`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_sensitivity.md).

## Usage

``` r
calib_setup_from_tables(folder_in, model_coupled, group_name = NULL)
```

## Arguments

- folder_in:

  character; path to the folder containing the
  `calibration_<module>.csv` files (same as `folder_out` used in
  [`create_calibration_tables`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/create_calibration_tables.md)).

- model_coupled:

  character; one coupled model name (e.g. `"GOTM-Selmaprotbas"`,
  `"GLM-AED2"`), or a character vector of coupled model names. Only
  parameters for these models are included in the output.

- group_name:

  character or `NULL`; for biological modules with multiple groups
  (phytoplankton, zooplankton, etc.) this maps to the `group_name`
  column expected by `run_lhc_wq`. When `NULL` (default) the column is
  set to `NA`.

## Value

A data frame with columns `model_coupled`, `module`, `pars`, `lb`, `ub`,
`x0`, `log`, `file`, `group_name`, `unit`, `note`. When a single model
is supplied, the result can be passed directly to `run_lhc_wq` or
`run_sensitivity`. When multiple models are supplied, the output is a
combined reference table and should be filtered per model before running
calibration.

## Details

The function looks for all files matching `calibration_*.csv` inside
`folder_in`, stacks them, filters to `include == TRUE` and the requested
`model_coupled`, then renames columns to match the `calib_setup`
contract used by the calibration runners. If multiple models are
supplied, the returned data frame includes `model_coupled` so rows from
different model couplings remain distinguishable.

## Examples

``` r
if (FALSE) { # \dontrun{
# After editing calibration CSVs to set include = TRUE:
calib_setup <- calib_setup_from_tables(
  folder_in     = "calibration",
  model_coupled = "GOTM-Selmaprotbas"
)

results <- run_lhc_wq(
  model          = "GOTM-Selmaprotbas",
  param_names    = calib_setup$pars,
  calib_setup    = calib_setup,
  yaml_file      = "metrics.yaml",
  model_dir      = "GOTM-Selmaprotbas",
  n_samples      = 50,
  wq_config_file = "LakeEnsemblR_WQ.yaml"
)
} # }
```
