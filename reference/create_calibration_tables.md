# Create calibration tables

Generates a master calibration CSV and per-module CSVs from the
LakeEnsemblR.WQ dictionary. Every parameter gets `include = FALSE` by
default so users can review and selectively opt-in. Lower and upper
bounds are set to `default * (1 - bounds_factor)` and
`default * (1 + bounds_factor)`.

## Usage

``` r
create_calibration_tables(
  folder = ".",
  config_file,
  folder_out = folder,
  models_coupled = c("GLM-AED2", "GOTM-Selmaprotbas", "GOTM-WET", "Simstrat-AED2"),
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
    needed.

3.  Call
    [`calib_setup_from_tables`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/calib_setup_from_tables.md)
    to read the edited CSVs and build the `calib_setup` data frame
    expected by
    [`run_lhc_wq`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)
    and
    [`run_sensitivity`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_sensitivity.md).

## Examples

``` r
if (FALSE) { # \dontrun{
create_calibration_tables(
  folder         = ".",
  config_file    = "LakeEnsemblR_WQ.yaml",
  folder_out     = "calibration",
  models_coupled = c("GOTM-Selmaprotbas", "GLM-AED2"),
  bounds_factor  = 0.2
)
# Then edit calibration/calibration_<module>.csv files,
# set include = TRUE for chosen parameters, and run:
calib_setup <- calib_setup_from_tables(folder_in = "calibration",
                                       model_coupled = "GOTM-Selmaprotbas")
} # }
```
