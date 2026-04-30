# Extract active AED2 modules from an `aed2.nml` file

This helper reads an AED2 namelist file (typically `aed2.nml`) and
parses the `&aed2_models` block to determine which AED2 modules are
activated. The returned character vector can be used to filter
\[aed2_inflow_map\] and to decide which Simstrat–AED2 inflow files need
to be created.

## Usage

``` r
get_active_aed2_modules(aed2_file)
```

## Arguments

- aed2_file:

  Character string; path to `aed2.nml`.

## Value

A character vector with the names of the active AED2 modules, e.g.
`c("aed2_carbon", "aed2_nitrogen", "aed2_phytoplankton")`.

## See also

\[generate_simstrat_aed2_inflows()\]

## Examples

``` r
if (FALSE) { # \dontrun{
active <- get_active_aed2_modules("aed2.nml")
active
} # }
```
