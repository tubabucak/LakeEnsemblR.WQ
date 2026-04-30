# Extract phytoplankton group names from `aed2_phyto_pars.nml`

This helper reads the AED2 phytoplankton parameter file (typically
`aed2_phyto_pars.nml`) and extracts the phytoplankton group names from
the `pd%p_name` line. These names are then used to construct
Simstrat–AED2 inflow file names for phytoplankton variables, replacing
the `"XX"` placeholder in entries such as `"PHY_XX_inflow"`

## Usage

``` r
get_phyto_names(phyto_pars_file, sanitize = TRUE)
```

## Arguments

- phyto_pars_file:

  Character string; path to `aed2_phyto_pars.nml`.

- sanitize:

  Logical; if `TRUE` (default), return file–name–safe versions of the
  group names (lowercase, with non–alphanumeric characters replaced by
  underscores). If `FALSE`, the raw names from `pd%p_name` are returned.

## Value

A character vector of phytoplankton group names (possibly sanitized),
e.g. `c("diatoms", "cyanobacteria", "some_random_group")`.

## Details

By default, names are "sanitized" for safe use in file names: converted
to lower case and with non–alphanumeric characters replaced by
underscores.

## See also

\[generate_simstrat_aed2_inflows()\]

## Examples

``` r
if (FALSE) { # \dontrun{
get_phyto_names("aed2_phyto_pars.nml")
} # }
```
