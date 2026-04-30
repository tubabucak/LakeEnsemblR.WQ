# Create input tables (Deprecated)

**Deprecated.** This function generates CSV scaffolds for parameter
overrides but no function in the package reads them back. Use
[`set_value_config`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/set_value_config.md)
to set fixed parameter values directly, or
[`create_calibration_tables`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/create_calibration_tables.md)
to set up calibration ranges.

## Usage

``` r
create_input_tables(
  folder = ".",
  config_file,
  folder_out = folder,
  input = NULL,
  models_coupled = c("GLM-AED2", "GOTM-Selmaprotbas", "GOTM-WET", "Simstrat-AED2",
    "MyLake", "PCLake")
)
```

## Arguments

- folder:

  path; where is the config_file located

- config_file:

  character; read groups of phytoplankton, zooplankton, etc. from here

- folder_out:

  path; in what folder should the files be placed

- input:

  character vector; for what parameters do you want to fill in values
