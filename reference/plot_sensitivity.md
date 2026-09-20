# Plot a sensitivity envelope from run_sensitivity() output

Plots how a model variable responds to a parameter that
`run_sensitivity(output_mode = "raw")` stepped through its range: one
thin grey line per parameter value, a shaded band for the chosen
quantile range across steps, and the median line. A thin band means the
parameter has little effect on this variable at this depth and is a poor
calibration candidate.

## Usage

``` r
plot_sensitivity(
  res,
  depth = NULL,
  var = 1,
  ylab = NULL,
  title = NULL,
  quantiles = c(0.05, 0.95),
  show_runs = TRUE
)
```

## Arguments

- res:

  The list returned by
  [`run_sensitivity()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_sensitivity.md)
  with `output_mode = "raw"`.

- depth:

  Numeric or `NULL`. Depth to extract. The nearest available output
  depth is used. If `NULL` (default), all depths are returned.

- var:

  Character or integer. Which variable to use when a step's `output`
  holds several (one data frame per entry of `vars`): its name or
  position. Default `1`.

- ylab, title:

  Character or `NULL`. Axis label and plot title. Defaults to the
  variable name (when known) and `"Sensitivity at <depth> m"`.

- quantiles:

  Numeric length 2. Lower and upper quantile of the shaded band. Default
  `c(0.05, 0.95)`.

- show_runs:

  Logical. Draw the individual runs underneath. Default `TRUE`.

## Value

A `ggplot` object. For the underlying numbers, summarise the output of
[`sensitivity_to_long()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/sensitivity_to_long.md)
yourself.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- run_sensitivity("kc", calib_setup, yaml_file = "Output.yaml",
                       model_dir = "GOTM-Selmaprotbas", n_steps = 10,
                       model = "GOTM-Selmaprotbas", output_mode = "raw",
                       vars = "selmaprotbas_DO_mg")
plot_sensitivity(res, depth = 5, ylab = "DO (mg/m3)")
} # }
```
