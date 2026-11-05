# Create parameter files from the dictionary

Writes one `par_file` CSV per active module (one per group for
phytoplankton, zooplankton and fish) containing every dictionary
parameter of the selected models, with `value` set to the dictionary
default. Unlike
[`create_calibration_tables`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/create_calibration_tables.md),
nothing is filtered out: boolean and integer switches (e.g.
`buoyancy_regulation`, `nitrogen_fixation`, `lNfix`) and zero-default
parameters are all included.

## Usage

``` r
create_par_files(
  folder = ".",
  config_file,
  folder_out = folder,
  models_coupled = NULL,
  overwrite = FALSE
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

  character vector; model couplings to include. Defaults to the `models`
  listed in the config file.

- overwrite:

  logical; overwrite existing files in `folder_out`? Defaults to `FALSE`
  so edited parameter files are not replaced.

## Value

Invisibly returns the paths of the files written.

## Details

The files are meant as templates for group- or lake-specific parameter
sets: edit `value` for the parameters to change, delete the rows to
leave at their dictionary default, and point the corresponding
`par_file` entry of the LakeEnsemblR_WQ config file at the result.
[`export_config_wq`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/export_config_wq.md)
then applies the listed values; the `unit`, `path` and `note` columns
are for reference only.

## Examples

``` r
ex <- lerwq_example()
create_par_files(
  folder         = ex,
  config_file    = "LakeEnsemblR_WQ.yaml",
  folder_out     = file.path(ex, "par_files"),
  models_coupled = "GOTM-Selmaprotbas"
)
#> Created: /tmp/Rtmph9ct1E/lerwq_example/par_files/par_oxygen.csv
#> Created: /tmp/Rtmph9ct1E/lerwq_example/par_files/par_carbon.csv
#> Created: /tmp/Rtmph9ct1E/lerwq_example/par_files/par_nitrogen.csv
#> Created: /tmp/Rtmph9ct1E/lerwq_example/par_files/par_phosphorus.csv
#> Created: /tmp/Rtmph9ct1E/lerwq_example/par_files/par_diatoms.csv
#> Created: /tmp/Rtmph9ct1E/lerwq_example/par_files/par_cyanobacteria.csv
#> Created: /tmp/Rtmph9ct1E/lerwq_example/par_files/par_daphnia.csv
#> 
#> Edit 'value' (and delete rows to keep at dictionary default), then set the par_file entries in LakeEnsemblR_WQ.yaml.
list.files(file.path(ex, "par_files"))
#> [1] "par_carbon.csv"        "par_cyanobacteria.csv" "par_daphnia.csv"      
#> [4] "par_diatoms.csv"       "par_nitrogen.csv"      "par_oxygen.csv"       
#> [7] "par_phosphorus.csv"   
```
