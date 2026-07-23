# Run Multi-Parameter Sensitivity Analysis Using Latin Hypercube Sampling

This function performs a multi-parameter sensitivity analysis using
Latin Hypercube Sampling (LHS) within a specified relative range for
each parameter. It modifies the input files (either \`.nml\` or
\`.csv\`) for a lake model, runs the model, and extracts defined metrics
for each iteration. The function reads model configuration files (which
are in the model directory), applies sampled parameter values, runs the
lake model using (\`run_glm()\`), and collects metrics using
(\`cal_metrics()\`). It supports modifying both \`.nml\` and \`.csv\`
files. For \`.csv\`, the parameter is matched using a column named
(\`pars\`), and optionally updated using (\`group_name\`) if provided.

## Usage

``` r
run_multi_param_sensitivity(
  param_names,
  calib_setup,
  rel_change,
  yaml_file,
  model_dir,
  n_steps = 10,
  model_filter = "GLM"
)
```

## Arguments

- param_names:

  A character vector of parameter names to vary in the sensitivity
  analysis.

- calib_setup:

  A data frame containing calibration setup information. Must include
  columns: `pars` (parameter names), `x0` (default values), `file` (file
  path relative to `model_dir`), and optionally `group_name` (for
  phytoplankton related parameters).

- rel_change:

  A numeric value specifying the relative range (e.g. 0.1 for plus or
  minus 10 percent) around the default value `x0`.

- yaml_file:

  Path to the YAML file used for metric extraction by
  [`cal_metrics()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md).

- model_dir:

  Path to the directory containing the lake model files and
  subdirectories. It should include the parameter files (e.g., for
  phytoplankton).

- n_steps:

  Number of LHS iterations (i.e., model realizations).

- model_filter:

  A character string specifying the model filter passed to
  [`cal_metrics()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
  (default is `"GLM"`).

## Value

A list of length `n_steps`, where each element contains sampled
parameter values and extracted metrics.

## Details

Parameter sampling is based on Latin Hypercube Sampling (via
(\`lhs::randomLHS\`)), and the sampled values are scaled to a symmetric
range around (\`x0\`).

## Examples

``` r
if (FALSE) { # \dontrun{
results <- run_multi_param_sensitivity(
  param_names = c("kw", "sed_temp", "DO_sat"),
  calib_setup = calib_setup,
  rel_change = 0.1,
  yaml_file = "config/metrics.yaml",
  model_dir = "GLM",
  n_steps = 20
)
} # }
```
