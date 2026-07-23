# Plot and compare stratification metrics across models

Takes the `Duration_of_Stratification` (or any metric with columns
`Year`, `Strat_Start_Date`, `Consecutive_Strat_Days`, and
`Mixing_Start_Date`) from the
[`cal_metrics()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
output list and produces a multi-panel comparison across all available
models.

## Usage

``` r
plot_strat_metrics(
  metrics_list,
  member = 1,
  metric_name = "Duration_of_Stratification",
  obs = NULL,
  years = NULL,
  colors = NULL,
  free_y = FALSE
)
```

## Arguments

- metrics_list:

  list or character; the full output of
  [`cal_metrics()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md),
  the sub-list for a single metric, or a path to a NetCDF file. When a
  NetCDF path is provided, stratification variables are read directly
  from file.

- member:

  integer; ensemble member index to read in NetCDF mode. Defaults to
  `1`.

- metric_name:

  character; name of the stratification metric to extract from
  `metrics_list` when list input is used. In NetCDF mode this is used as
  preferred duration variable name. Defaults to
  `"Duration_of_Stratification"`.

- obs:

  data.frame or `NULL`; optional observed reference data in the same
  format (columns `Year` and `Consecutive_Strat_Days`). When supplied it
  is overlaid as black points.

- years:

  integer vector or `NULL`; subset of years to include in the plots
  (e.g. `1990:2000` or `c(1992, 1995, 2000)`). When `NULL` (default) all
  available years are shown.

- colors:

  named character vector or `NULL`; custom colours for models. Names
  must match the model names in the metric list (e.g.
  `c(GLM = "#E41A1C", WET = "#377EB8")`). When `NULL`, a default palette
  is used.

- free_y:

  logical; if `TRUE` each panel uses a free y-axis scale. Defaults to
  `FALSE` (shared axis for direct comparison).

## Value

A named list with elements:

- `duration_plot`:

  ggplot — yearly stratification duration (days) per model.

- `onset_plot`:

  ggplot — day-of-year of stratification onset per model.

- `mixing_plot`:

  ggplot — day-of-year of mixing onset per model.

- `combined_data`:

  data.frame — the tidy data used for plotting.

## Examples

``` r
if (FALSE) { # \dontrun{
metrics <- cal_metrics(metric_yaml_file = "Output.yaml",
                       wq_config_file   = "LakeEnsemblR_WQ.yaml")

plots <- plot_strat_metrics(metrics)
plots$duration_plot
plots$onset_plot
} # }
```
