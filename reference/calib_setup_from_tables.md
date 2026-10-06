# Build calib_setup from edited calibration CSVs

Reads the per-module calibration CSV files produced by
[`create_calibration_tables`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/create_calibration_tables.md),
filters to rows where `include == TRUE`, and returns a `calib_setup`
data frame in the format expected by
[`calib_wq`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/calib_wq.md)
and
[`run_sensitivity`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_sensitivity.md).

## Usage

``` r
calib_setup_from_tables(folder_in, model_coupled, group_name = NULL)
```

## Arguments

- folder_in:

  character; path to the folder containing the
  `calibration_<module>.csv` files (same as `folder_out` used in
  [`create_calibration_tables`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/create_calibration_tables.md)).

- model_coupled:

  character; one coupled model name (e.g. `"GOTM-Selmaprotbas"`,
  `"GLM-AED"`), or a character vector of coupled model names. Only
  parameters for these models are included in the output.

- group_name:

  character or `NULL`; for biological modules with multiple groups
  (phytoplankton, zooplankton, etc.) this maps to the `group_name`
  column expected by `calib_wq`. When `NULL` (default) the column is set
  to `NA`.

## Value

A data frame with columns `model_coupled`, `module`, `domain`,
`process`, `subprocess`, `pars`, `lb`, `ub`, `x0`, `log`, `file`,
`group_name`, `unit`, `note`. When a single model is supplied, the
result can be passed directly to `calib_wq` or `run_sensitivity`. When
multiple models are supplied, the output is a combined reference table
and should be filtered per model before running calibration.

## Details

The function looks for all files matching `calibration_*.csv` inside
`folder_in`, stacks them, filters to `include == TRUE` and the requested
`model_coupled`, then renames columns to match the `calib_setup`
contract used by the calibration runners. If multiple models are
supplied, the returned data frame includes `model_coupled` so rows from
different model couplings remain distinguishable.

## Examples

``` r
ex <- lerwq_example()
cal_dir <- file.path(ex, "calibration")
create_calibration_tables(folder = ex, config_file = "LakeEnsemblR_WQ.yaml",
                          folder_out = cal_dir, models_coupled = "GLM-AED")
#> Skipping 16 integer/boolean-typed parameter(s) (not calibratable via percentage-based bounds): alk_mode, co2_model, co2_piston_model, ch4_piston_model, fT_method, lightModel, simN2O, n2o_piston_model, salTol, simDINUptake, simDIPUptake, simDONUptake, simINDynamics, simIPDynamics, simNFixation, simSiUptake
#> Skipping 9 zero-default parameter(s) with no dictionary min/max to fall back on (default * bounds_factor gives a zero-width range): K_Si, N_o, P_0, Fsed_n2o, R_nfix, Si_0, X_sicon, Smin_zoo, oxy_min
#> Created master reference: /tmp/RtmpNbRiC7/lerwq_example/calibration/calibration_master.csv
#> Created: /tmp/RtmpNbRiC7/lerwq_example/calibration/calibration_oxygen.csv
#> Created: /tmp/RtmpNbRiC7/lerwq_example/calibration/calibration_carbon.csv
#> Created: /tmp/RtmpNbRiC7/lerwq_example/calibration/calibration_nitrogen.csv
#> Created: /tmp/RtmpNbRiC7/lerwq_example/calibration/calibration_phosphorus.csv
#> Created: /tmp/RtmpNbRiC7/lerwq_example/calibration/calibration_silicon.csv
#> Created: /tmp/RtmpNbRiC7/lerwq_example/calibration/calibration_diatoms.csv
#> Created: /tmp/RtmpNbRiC7/lerwq_example/calibration/calibration_cyanobacteria.csv
#> Created: /tmp/RtmpNbRiC7/lerwq_example/calibration/calibration_daphnia.csv
#> 
#> Edit the per-module CSVs: set include = TRUE for parameters to calibrate.
#> Then call calib_setup_from_tables() to build the calib_setup for calib_wq().

# Normally done by hand in a spreadsheet editor: mark two sediment oxygen
# parameters for calibration
tab <- read.csv(file.path(cal_dir, "calibration_oxygen.csv"))
tab$include[tab$parameter %in% c("Fsed_oxy", "Ksed_oxy")] <- TRUE
write.csv(tab, file.path(cal_dir, "calibration_oxygen.csv"), row.names = FALSE)

calib_setup <- calib_setup_from_tables(folder_in = cal_dir,
                                       model_coupled = "GLM-AED")
#> calib_setup built: 2 parameter(s) for GLM-AED
calib_setup[, c("pars", "lb", "ub", "x0", "file")]
#>       pars     lb    ub    x0                file
#> 1 Fsed_oxy -12.00 -8.00 -10.0 aed_oxygen/Fsed_oxy
#> 2 Ksed_oxy   5.44  8.16   6.8 aed_oxygen/Ksed_oxy
```
