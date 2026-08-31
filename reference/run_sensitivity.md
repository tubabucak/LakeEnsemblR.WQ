# Run Sensitivity Analysis for a Model Parameter

Performs a one-at-a-time sensitivity analysis for a specified model
parameter, optionally targeting a specific group (e.g., a phytoplankton
group in a parameter CSV or a FABM instance for
GOTM-WET/GOTM-Selmaprotbas). The parameter value is iteratively changed
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
  model = "GLM-AED2",
  model_filter = NULL,
  group_name = NULL,
  yaml_file_model = NULL,
  par_file = NULL,
  wq_config_file = NULL,
  output_mode = "metrics",
  vars = NULL,
  obs_depths = NULL,
  depth_01 = 1,
  conversion_factor = 1,
  target_variable = NULL,
  verbose = TRUE
)
```

## Arguments

- param_name:

  Character. Name of the parameter to modify (must match \`pars\` in
  \`calib_setup\`).

- calib_setup:

  Data frame. Calibration setup table containing parameter names, bounds
  (\`lb\`, \`ub\`), initial values (\`x0\`), file/path names (\`file\`),
  and optionally a \`group_name\` column for group-specific parameters.
  This is the same table produced by
  [`calib_setup_from_tables`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/calib_setup_from_tables.md)
  and consumed by
  [`run_lhc_wq`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md).

- yaml_file:

  Character. Path to the YAML file defining which metrics to extract
  (output.yaml).

- model_dir:

  Character. Path to the model directory where the input files and
  outputs are located.

- n_steps:

  Integer. Number of steps (iterations) in the parameter value sequence
  (default = 10).

- model:

  Character. One of `"GLM-AED2"`, `"GOTM-WET"`, `"GOTM-Selmaprotbas"`,
  or `"Simstrat-AED2"`. Determines both how `param_name` is written to
  its target file and which model engine is run. Default `"GLM-AED2"`
  for backwards compatibility.

- model_filter:

  Character or `NULL`. Model identifier used by \`cal_metrics()\` to
  filter the results. If `NULL` (default), auto-derived from `model`
  (e.g. `"GLM"`, `"WET"`, `"SELMAPROTBAS"`, `"SIMSTRAT"`).

- group_name:

  Character or \`NULL\`. Optional if a certain group (e.g. a
  phytoplankton group in a parameter CSV, or a FABM instance such as
  \`"diatoms"\` for GOTM-WET/GOTM-Selmaprotbas) should be selected. If
  provided, only updates the specified group/column in the parameter CSV
  (e.g., \`"cyano"\`, \`"green"\`, \`"diatom"\`), or substitutes into a
  \`{group_name}\` placeholder in a GOTM-WET/GOTM-Selmaprotbas \`path\`.
  If \`NULL\`, all relevant rows in \`calib_setup\` for \`param_name\`
  are used.

- yaml_file_model:

  Character or `NULL`. GOTM yaml filename (e.g. `"gotm.yaml"`), only
  used/required when `model` is `"GOTM-WET"` or `"GOTM-Selmaprotbas"`.
  If `NULL` (default), falls back to `"gotm.yaml"`.

- par_file:

  Character or `NULL`. Simstrat par filename, only used/required when
  `model` is `"Simstrat-AED2"`. If `NULL` (default), falls back to
  `"simstrat.par"`.

- wq_config_file:

  Character or `NULL`. Path to the LakeEnsemblR_WQ config file (e.g.
  `"LakeEnsemblR_WQ.yaml"`), passed through to
  [`cal_metrics()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md).
  Required when `output_mode = "metrics"` –
  [`cal_metrics()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
  uses it to expand phytoplankton/zooplankton group templates in the
  metrics dictionary and has no default of its own. Ignored when
  `output_mode = "raw"`.

- output_mode:

  Character. `"metrics"` (default) runs each step through
  [`cal_metrics()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
  – the harmonized/derived-metric pipeline used elsewhere in this
  package, requiring `wq_config_file` and an entry for the metric(s) of
  interest in `yaml_file`. `"raw"` instead pulls one or more model
  output variables directly via
  [`get_output_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/get_output_wq.md),
  bypassing the metrics dictionary/template machinery entirely – useful
  for a quick look at how a raw output variable (e.g. `"sO2W"` for
  GOTM-WET) responds to `param_name` without needing `wq_config_file` or
  an `Output.yaml` entry for it.

- vars:

  Character vector. Required when `output_mode = "raw"` – the model
  output variable name(s) to extract at each step (as they appear in the
  model's native output, e.g. `"sO2W"` for GOTM-WET/GOTM-Selmaprotbas,
  `"OXY_oxy"` for GLM-AED2/Simstrat-AED2). Passed through to
  [`get_output_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/get_output_wq.md)'s
  `vars` argument.

- obs_depths:

  Numeric vector or `NULL`. Only used when `output_mode = "raw"`. Passed
  through to
  [`get_output_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/get_output_wq.md)
  – depths to interpolate the raw output to, when `depth_01 = 1`. `NULL`
  (default) returns output at the model's native depths.

- depth_01:

  Integer. Only used when `output_mode = "raw"`. Passed through to
  [`get_output_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/get_output_wq.md):
  `0` if `vars` has no depth dimension, `1` (default) if it does.

- conversion_factor:

  Numeric. Only used when `output_mode = "raw"`. Passed through to
  [`get_output_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/get_output_wq.md)'s
  unit conversion factor. Default `1` (no conversion).

- target_variable:

  Character vector or `NULL`. Only used when `output_mode = "metrics"`.
  If supplied, the
  [`cal_metrics()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
  output for each step is filtered down to just these metric name(s)
  (i.e. `names(metrics)` in the returned `metrics` list, e.g.
  `"DO_gramsPerCubicMeter"`) instead of returning every metric defined
  in `yaml_file`. `NULL` (default) keeps all of them. Has no effect in
  `output_mode = "raw"` – there, `vars` already selects exactly what's
  extracted.

- verbose:

  Logical. Print progress messages and pass through to the model
  engine's own `verbose` argument. Default `TRUE`.

## Value

A list with one element per parameter value step. Each element contains:

- param_value:

  The value used in this iteration.

- metrics:

  The output from \`cal_metrics()\` for this iteration. Only present
  when `output_mode = "metrics"`.

- output:

  The output from \`get_output_wq()\` for this iteration (raw model
  variable(s), one data frame per variable in `vars`). Only present when
  `output_mode = "raw"`.

## Details

The function supports \`.nml\` and \`.csv\` parameter files (used by
`"GLM-AED2"` and `"Simstrat-AED2"`), as well as FABM \`.yaml\`/\`.yml\`
parameter files (used by `"GOTM-WET"` and `"GOTM-Selmaprotbas"`, where
\`calib_setup\$file\` holds the FABM instance/key path, e.g.
\`"abiotic_water/parameters/hO2Nitr"\`, written into \`fabm.yaml\` via
[`LakeEnsemblR::input_yaml_multiple()`](https://aemon-j.github.io/LakeEnsemblR/reference/input_yaml_multiple.html)).
For \`.csv\` files, it automatically handles quoted column names and
parameter names (e.g., \`'p_name'\`, \`'R_growth'\`) by stripping
quotes. When \`group_name\` is provided, it only updates that specific
group column (\`.csv\`) or substitutes it into the \`{group_name}\`
placeholder of the \`path\` (\`.yaml\`/\`.yml\`). Otherwise, all columns
(from column 2 onward, \`.csv\`) are updated for the matching parameter
row.

## Examples

``` r
if (FALSE) { # \dontrun{
results <- run_sensitivity("R_growth", calib_setup, yaml_file = "metrics.yaml",
                           model_dir = "model/", n_steps = 10, model = "GLM-AED2",
                           group_name = "cyano", wq_config_file = "LakeEnsemblR_WQ.yaml",
                           target_variable = "DO_gramsPerCubicMeter")

# raw model output instead of cal_metrics() -- no wq_config_file needed
results <- run_sensitivity("hO2Nitr", calib_setup, yaml_file = "Output.yaml",
                           model_dir = "GOTM-WET/", n_steps = 10, model = "GOTM-WET",
                           output_mode = "raw", vars = "sO2W")
} # }
```
