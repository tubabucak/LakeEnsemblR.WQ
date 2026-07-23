# Format a Simstrat-style AED2 inflow file (character vector)

This helper creates the contents of a Simstrat inflow file for a single
AED2 variable, given a time axis and simple inflow geometry (deep vs
surface inflows). It mirrors the structure of `format_flow_simstrat()`
used for Q/T/S inflows, but is applied to AED2 tracers.

## Usage

``` r
format_aed_inflow_simstrat(
  varname,
  levels,
  surf_flow,
  sim_par = "simstrat.par",
  times = NULL,
  default_value = 0,
  unit = "millimolesPerMeterCubed"
)
```

## Arguments

- varname:

  Character; AED2 inflow variable name, e.g. `"PHS_frp_inflow"`.

- levels:

  Numeric vector; inflow depths (m), one per branch.

- surf_flow:

  Logical vector; same length as `levels`, TRUE for surface inflows,
  FALSE for deep inflows.

- sim_par:

  Character; path to `simstrat.par`-style JSON config.

- times:

  Numeric vector of times \[days\] for the template time series. If
  `NULL`, `Simulation$"Start d"` and `Simulation$"End d"` are used with
  a daily step.

- default_value:

  Numeric; default value to assign at all depths and times (e.g. 0 for
  most tracers, 8 for `CAR_pH_inflow`).

- unit:

  Character; unit string to include in the header, e.g.
  `"millimolesPerMeterCubed"` or `"pH"`.

## Value

A character vector of lines to be written to a `.dat` file.

## See also

\[generate_simstrat_aed2_inflows()\]
