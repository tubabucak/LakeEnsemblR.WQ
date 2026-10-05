# Copy the bundled example setup

Copies the example setup shipped with the package to a folder of your
choice, so it can be configured and run without modifying the installed
package. The example covers one year (1995) of Lake Mendota (Wisconsin,
USA): meteorological forcing, inflow/outflow, bathymetry, observed water
temperature and water quality, and LakeEnsemblR/LakeEnsemblR.WQ
configuration files for GLM-AED, GOTM-WET, GOTM-Selmaprotbas and
Simstrat-AED2. It is used by the examples throughout this package.

## Usage

``` r
lerwq_example(dest = file.path(tempdir(), "lerwq_example"), overwrite = FALSE)
```

## Arguments

- dest:

  character; folder to copy the example into. Created if it does not
  exist. Defaults to a folder in
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- overwrite:

  logical; overwrite files that already exist in `dest`.

## Value

The normalized path to `dest`.

## Examples

``` r
ex <- lerwq_example()
list.files(ex)
#>  [1] "LakeEnsemblR.yaml"                      
#>  [2] "LakeEnsemblR_WQ.yaml"                   
#>  [3] "LakeEnsemblR_bathymetry_standard.csv"   
#>  [4] "LakeEnsemblR_ice-height_standard.csv"   
#>  [5] "LakeEnsemblR_inflow_standard.csv"       
#>  [6] "LakeEnsemblR_meteo_standard_daily.csv"  
#>  [7] "LakeEnsemblR_outflow_standard.csv"      
#>  [8] "LakeEnsemblR_wtemp_profile_standard.csv"
#>  [9] "Output.yaml"                            
#> [10] "WQ_input_files"                         
#> [11] "calibration"                            
#> [12] "standart_observed_data.csv"             
```
