# Plot and compare anoxia metrics across models

Takes the output of
[`cal_anoxic_date()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_anoxic_date.md)
(either directly or as part of the nested
[`cal_metrics()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
output) and builds comparison plots for:

- yearly number of anoxic days,

- yearly anoxic factor (AF),

- daily anoxic depth time series.

## Usage

``` r
plot_anoxic_metrics(
  metrics_list,
  metric_name = "Anoxic_Factor",
  member = 1,
  years = NULL,
  colors = NULL,
  free_y = FALSE
)
```

## Arguments

- metrics_list:

  list or character; either:

  - the full
    [`cal_metrics()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
    output,

  - one metric sub-list (model -\> metric instance), or

  - a named model list of
    [`cal_anoxic_date()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_anoxic_date.md)
    outputs,

  - a path to an ensemble NetCDF file (`.nc`).

- metric_name:

  character; metric name to extract when `metrics_list` is the full
  [`cal_metrics()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
  output. If not found, the function assumes `metrics_list` is already
  the per-model list. In NetCDF mode, this is used as preferred variable
  name for AF.

- member:

  integer; ensemble member index to read in NetCDF mode. Defaults to
  `1`.

- years:

  integer vector or `NULL`; optional subset of years to include in
  yearly plots (and used to filter the depth time series).

- colors:

  named character vector or `NULL`; optional model colours.

- free_y:

  logical; if `TRUE` use free y-axis scaling in yearly comparisons.
  Defaults to `FALSE`.

## Value

A named list with:

- `anoxic_days_plot`:

  ggplot of yearly number of anoxic days per model.

- `af_plot`:

  ggplot of yearly AF per model.

- `anoxic_depth_plot`:

  ggplot of daily anoxic depth per model.

- `anoxic_days_data`:

  tidy data used for anoxic days plot.

- `af_data`:

  tidy data used for AF plot.

- `anoxic_depth_data`:

  tidy data used for depth plot.

## Examples

``` r
if (FALSE) { # \dontrun{
plots <- plot_anoxic_metrics(mendota_metrics)
plots$anoxic_days_plot
plots$af_plot
plots$anoxic_depth_plot

plots_subset <- plot_anoxic_metrics(mendota_metrics, years = 1995:2005)
} # }
```
