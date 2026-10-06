# Create calibration tables

Generates a master calibration CSV and per-module CSVs from the
LakeEnsemblR.WQ dictionary. Every parameter gets `include = FALSE` by
default so users can review and selectively opt-in. Lower and upper
bounds are set to `default * (1 - bounds_factor)` and
`default * (1 + bounds_factor)` (the other way round for negative
defaults, so that `lower <= upper`). When the dictionary provides
`min`/`max` values for a parameter, they are carried through as
`dict_min`/`dict_max` reference columns (not used to compute
`lower`/`upper` automatically) so they can be checked against, and
copied into `lower`/`upper` by hand, when editing the CSV.

## Usage

``` r
create_calibration_tables(
  folder = ".",
  config_file,
  folder_out = folder,
  models_coupled = c("GLM-AED", "GOTM-Selmaprotbas", "GOTM-WET", "Simstrat-AED2"),
  bounds_factor = 0.2
)
```

## Arguments

- folder:

  path; directory containing the config file.

- config_file:

  character; name of the LakeEnsemblR_WQ YAML config file.

- folder_out:

  path; output directory for the CSV files (created if needed).

- models_coupled:

  character vector; model couplings to include.

- bounds_factor:

  numeric; fractional deviation from default for bounds (default = 0.2,
  i.e. ±20%).

## Value

Invisibly returns the master calibration table as a data frame.

## Details

**Workflow:**

1.  Run `create_calibration_tables()` — generates
    `calibration_master.csv` (read-only reference) and one
    `calibration_<module>.csv` per active module.

2.  Open the per-module CSVs, set `include = TRUE` for parameters you
    want to calibrate, and adjust `lower` / `upper` / `initial` as
    needed — use `dict_min` / `dict_max` as a reference for the
    parameter's plausible physical range, where available.

3.  Call
    [`calib_setup_from_tables`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/calib_setup_from_tables.md)
    to read the edited CSVs and build the `calib_setup` data frame
    expected by
    [`calib_wq`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/calib_wq.md)
    and
    [`run_sensitivity`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_sensitivity.md).

## Examples

``` r
ex <- lerwq_example()
create_calibration_tables(
  folder         = ex,
  config_file    = "LakeEnsemblR_WQ.yaml",
  folder_out     = file.path(ex, "calibration"),
  models_coupled = c("GOTM-Selmaprotbas", "GLM-AED"),
  bounds_factor  = 0.2
)
#> Skipping 28 integer/boolean-typed parameter(s) (not calibratable via percentage-based bounds): alk_mode, co2_model, co2_piston_model, ch4_piston_model, fT_method, lightModel, buoy_nutrient, simN2O, n2o_piston_model, buoy_temperature, buoyancy_regulation, couple_dom, diagnostics, llim, salTol, simDINUptake, simDIPUptake, nitrogen_fixation, simDONUptake, simINDynamics, simIPDynamics, simNFixation, simSiUptake, tlim, use_24h_light, nprey
#> Skipping 23 zero-default parameter(s) with no dictionary min/max to fall back on (default * bounds_factor gives a zero-width range): alpha_si, K_Si, N_o, P_0, buoy_nutrient_limit, Fsed_n2o, buoy_temp_limit, dd_p, R_nfix, Si_0, rfs, sedrate, X_sicon, vert_vel_nutrient, c0, vert_vel_temp, Smin_zoo, vert_vel3, wz, oxy_min, o2corr_method, elevation
#> Created master reference: /tmp/RtmpSSn8qT/lerwq_example/calibration/calibration_master.csv
#> Created: /tmp/RtmpSSn8qT/lerwq_example/calibration/calibration_oxygen.csv
#> Created: /tmp/RtmpSSn8qT/lerwq_example/calibration/calibration_carbon.csv
#> Created: /tmp/RtmpSSn8qT/lerwq_example/calibration/calibration_nitrogen.csv
#> Created: /tmp/RtmpSSn8qT/lerwq_example/calibration/calibration_phosphorus.csv
#> Created: /tmp/RtmpSSn8qT/lerwq_example/calibration/calibration_silicon.csv
#> Created: /tmp/RtmpSSn8qT/lerwq_example/calibration/calibration_diatoms.csv
#> Created: /tmp/RtmpSSn8qT/lerwq_example/calibration/calibration_cyanobacteria.csv
#> Created: /tmp/RtmpSSn8qT/lerwq_example/calibration/calibration_daphnia.csv
#> 
#> Edit the per-module CSVs: set include = TRUE for parameters to calibrate.
#> Then call calib_setup_from_tables() to build the calib_setup for calib_wq().
list.files(file.path(ex, "calibration"))
#> [1] "calibration_carbon.csv"        "calibration_cyanobacteria.csv"
#> [3] "calibration_daphnia.csv"       "calibration_diatoms.csv"      
#> [5] "calibration_master.csv"        "calibration_nitrogen.csv"     
#> [7] "calibration_oxygen.csv"        "calibration_phosphorus.csv"   
#> [9] "calibration_silicon.csv"      

# Next: edit calibration/calibration_<module>.csv, set include = TRUE for
# the parameters to calibrate, and build the setup table with
# calib_setup_from_tables() (see its examples).
```
