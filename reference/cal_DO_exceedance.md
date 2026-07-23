# Calculate DO exceedance

Calculates the percentage time exceeded curve for O2 (similar approach
to Flow Duration Curve)

## Usage

``` r
cal_DO_exceedance(oxy_data, depth = 0)
```

## Arguments

- oxy_data:

  data.frame A dataframe containing oxygen concentrations. The first
  column should be \`datetime\`, and the remaining columns should
  represent oxygen values at different depths.

- depth:

  numeric The depth at which to extract data. Must match a column name
  in \`oxy_data\` (e.g., 1.0, 1.5, etc.).

## Value

a data.frame
