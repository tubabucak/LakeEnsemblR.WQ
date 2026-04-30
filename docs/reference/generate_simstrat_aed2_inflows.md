# Generate Simstrat–AED2 inflow files for active modules

Creates template Simstrat inflow `.dat` files for all AED2 variables
associated with the active modules in `aed2_file`. Phytoplankton
variables are expanded based on group names in `aed2_phyto_pars.nml`.

## Usage

``` r
generate_simstrat_aed2_inflows(
  aed2_file,
  phyto_pars_file = NULL,
  sim_par,
  out_dir,
  levels = NULL,
  surf_flow = NULL,
  default_value = 0,
  unit = "millimolesPerMeterCubed",
  overwrite = FALSE
)
```

## Arguments

- aed2_file:

  Character; path to `aed2.nml`.

- phyto_pars_file:

  Character; path to `aed2_phyto_pars.nml`. Required if
  `aed2_phytoplankton` is active.

- sim_par:

  Character; path to `simstrat.par`-style JSON config.

- out_dir:

  Character; directory to which inflow `.dat` files will be written.

- levels:

  Numeric vector; inflow depths (m). If `NULL` together with
  `surf_flow`, a single surface inflow at 0 m is assumed.

- surf_flow:

  Logical vector; TRUE for surface inflows, FALSE for deep inflows. Must
  be the same length as `levels`.

- default_value:

  Numeric; default initial inflow value (0 for most tracers).

- unit:

  Character; default unit string to use for non-pH variables.

- inflow_map:

  Data frame mapping AED2 modules to inflow variable base names;
  defaults to \[aed2_inflow_map\].

## Value

Invisibly returns the character vector of inflow variable names for
which files were written (without `.dat` extension).

## Details

By default, all inflows are treated as surface inflows at 0 m, and all
variables are initialised with `default_value`, except `CAR_pH_inflow`,
which is initialised to 8 with unit `"pH"`.
