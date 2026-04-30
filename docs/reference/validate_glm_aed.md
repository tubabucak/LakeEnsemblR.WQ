# Validate a GLM-AED model simulation folder

The function checks that the GLM namelist file exists and that key input
files referenced inside the namelist are present in the simulation
folder. It also checks that the output directory exists, and attempts to
create it if missing. If any required file is missing, the function
stops with an informative error message describing which file(s) could
not be found.

## Usage

``` r
validate_glm_aed(sim_folder = ".", file = "glm3.nml", verbose = TRUE)
```

## Arguments

- sim_folder:

  Character. Path to the GLM simulation folder containing the namelist
  and input files.

- file:

  Character. Name of the GLM namelist file inside `sim_folder`. Default
  is `"glm3.nml"`.

- verbose:

  Logical. If `TRUE`, progress messages are printed using
  [`message()`](https://rdrr.io/r/base/message.html). Default is `TRUE`.

## Value

Invisibly returns `TRUE` if validation succeeds. Otherwise, the function
throws an error.

## Examples

``` r
if (FALSE) { # \dontrun{
# Validate a prepared GLM run directory
validate_glm_aed(sim_folder = "runs/lake_001", file = "glm3.nml")

# Quiet mode
validate_glm_aed(sim_folder = "runs/lake_001", verbose = FALSE)
} # }
```
