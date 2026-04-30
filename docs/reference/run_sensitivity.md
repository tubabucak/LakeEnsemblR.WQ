# Run Sensitivity Analysis for a Model Parameter

Performs a one-at-a-time sensitivity analysis for a specified model
parameter, optionally targeting a specific group (e.g., a phytoplankton
group in a parameter CSV). The parameter value is iteratively changed
across a defined range, the model is run, and the selected metrics are
calculated at each step.

## Usage

``` r
run_sensitivity(
  param_name,
  calib_setup,
  yaml_file,
  model_dir,
  n_steps = 10,
  model_filter = "GLM",
  group_name = NULL
)
```

## Arguments

- param_name:

  Character. Name of the parameter to modify (must match \`pars\` in
  \`calib_setup\`).

- calib_setup:

  Data frame. Calibration setup table containing parameter names, bounds
  (\`lb\`, \`ub\`), initial values (\`x0\`), file names (\`file\`), and
  optionally a \`group_name\` column for group-specific parameters.

- yaml_file:

  Character. Path to the YAML file defining which metrics to extract
  (output.yaml).

- model_dir:

  Character. Path to the model directory where the input files and
  outputs are located.

- n_steps:

  Integer. Number of steps (iterations) in the parameter value sequence
  (default = 10).

- model_filter:

  Character. Model identifier used by \`cal_metrics()\` to filter the
  results (default = \`"GLM"\`).

- group_name:

  Character or \`NULL\`. Optional if certain pyhtoplankton group should
  be selected. If provided, only updates the specified group/column in
  the parameter CSV (e.g., \`"cyano"\`, \`"green"\`, \`"diatom"\`). If
  \`NULL\`, all relevant rows in \`calib_setup\` for \`param_name\` are
  used.

## Value

A list with one element per parameter value step. Each element contains:

- param_value:

  The value used in this iteration.

- metrics:

  The output from \`cal_metrics()\` for this iteration.

## Details

The function supports both \`.nml\` and \`.csv\` parameter files. For
\`.csv\` files, it automatically handles quoted column names and
parameter names (e.g., \`'p_name'\`, \`'R_growth'\`) by stripping
quotes. When \`group_name\` is provided, it only updates that specific
group column. Otherwise, all columns (from column 2 onward) are updated
for the matching parameter row.

## Examples

``` r
if (FALSE) { # \dontrun{
results <- run_sensitivity("R_growth", calib_setup, yaml_file = "metrics.yaml",
                           model_dir = "model/", n_steps = 10, group_name = "cyano")
} # }
```
