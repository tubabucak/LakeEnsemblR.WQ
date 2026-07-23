# Extract zooplankton group name from `aed2_zoop_pars.nml`

This helper reads the AED2 zooplankton parameter file and extracts the
zooplankton name from the `zoop_param%zoop_name` line.

## Usage

``` r
get_zoop_names(zoop_pars_file, sanitize = TRUE)
```

## Arguments

- zoop_pars_file:

  Character string; path to `aed2_zoop_pars.nml`.

- sanitize:

  Logical; if `TRUE`, return file-name-safe names.

## Value

A character vector of zooplankton names.
