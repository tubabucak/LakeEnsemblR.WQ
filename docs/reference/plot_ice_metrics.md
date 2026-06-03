# Plot and compare ice metrics across models

Supports two inputs: 1) nested cal_metrics output containing a
cal_ice_duration result (ice_duration_period and ice_thickness), or 2) a
NetCDF output file path.

## Usage

``` r
plot_ice_metrics(
  metrics_list,
  metric_name = "Ice_Duration_Days",
  member = 1,
  years = NULL,
  colors = NULL,
  free_y = FALSE
)
```

## Arguments

- metrics_list:

  list or character; full cal_metrics output, a metric sub-list, or
  NetCDF file path (.nc).

- metric_name:

  character; metric name to extract from list input. If not found, the
  function tries to auto-detect a block containing ice_duration_period
  and ice_thickness.

- member:

  integer; ensemble member index used in NetCDF mode.

- years:

  integer vector or NULL; optional year subset.

- colors:

  named character vector or NULL; optional model colors.

- free_y:

  logical; if TRUE, yearly and daily plots use free y by model.

## Value

Named list with duration_plot, thickness_plot, duration_data,
thickness_data.
