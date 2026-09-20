# Convert raw sensitivity output to a long data frame

Reshapes the result of `run_sensitivity(output_mode = "raw")` into one
long data frame (one row per parameter step x timestep) at a single
depth, ready for custom plotting or summaries.
[`plot_sensitivity()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/plot_sensitivity.md)
uses it internally.

## Usage

``` r
sensitivity_to_long(res, depth = NULL, var = 1)
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

## Value

A data frame with columns `datetime`, `depth`, `value`, `iteration`
(position of the parameter step) and `param_value` (the parameter value
used in that step). When `depth` is given, the depth actually used is
stored in the `"depth_used"` attribute. Steps with no usable output are
skipped with a warning.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- run_sensitivity("kc", calib_setup, yaml_file = "Output.yaml",
                       model_dir = "GOTM-Selmaprotbas", n_steps = 10,
                       model = "GOTM-Selmaprotbas", output_mode = "raw",
                       vars = "selmaprotbas_DO_mg")
long <- sensitivity_to_long(res, depth = 5)
} # }
```
