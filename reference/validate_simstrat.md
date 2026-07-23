# Validate a Simstrat-AED2 model simulation folder

The function checks that the Simstrat configuration and key input files
referenced inside the simstrat.par are present in the simulation folder.
It also checks that the output directory exists, and attempts to create
it if missing.

If any required file is missing, the function stops with an informative
error message describing which file(s) could not be found.

## Usage

``` r
validate_simstrat(
  sim_folder = ".",
  file = "simstrat.par",
  verbose = TRUE,
  check_time_coverage = TRUE
)
```

## Arguments

- sim_folder:

  Character. Path to the Simstrat-AED2 simulation folder containing the
  namelist and input files.

- file:

  Character. Name of the Simstrat-AED2 configuration file inside
  `sim_folder`. Default is `"simstrat.par"`.

- verbose:

  Logical. If `TRUE`, progress messages are printed using
  [`message()`](https://rdrr.io/r/base/message.html). Default is `TRUE`.

- check_time_coverage:

  Logical. If `TRUE`, checks that time series inputs cover the
  simulation end time. Default is `TRUE`.

## Value

Invisibly returns `TRUE` if validation succeeds. Otherwise, the function
throws an error.

## Examples

``` r
if (FALSE) { # \dontrun{
validate_simstrat(sim_folder = "models/simstrat_aed2", file = "simstrat.par")
} # }
```
