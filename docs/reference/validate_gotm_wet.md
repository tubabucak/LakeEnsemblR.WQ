# Validate a GOTM-WET model simulation folder

The function checks that the GOTM configuration and key input files
referenced inside the namelist are present in the simulation folder. It
also checks that the output directory exists, and attempts to create it
if missing. If any required file is missing, the function stops with an
informative error message describing which file(s) could not be found.

## Usage

``` r
validate_gotm_wet(sim_folder = ".", file = "gotm.yaml", verbose = TRUE)
```

## Arguments

- sim_folder:

  Character. Path to the GOTM-WET simulation folder containing the
  namelist and input files.

- file:

  Character. Name of the GOTM-WET namelist file inside `sim_folder`.
  Default is `"gotm.nml"`.

- verbose:

  Logical. If `TRUE`, progress messages are printed using
  [`message()`](https://rdrr.io/r/base/message.html). Default is `TRUE`.

## Value

Invisibly returns `TRUE` if validation succeeds. Otherwise, the function
throws an error.

## Examples
