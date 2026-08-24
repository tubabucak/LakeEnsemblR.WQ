# Plot a single model's output against observed data at matching depths

Extracts a model's simulated output at the depths present in an observed
dataset (via `get_output_wq(obs_depths = ...)`) and plots it against the
observations, one facet per depth, with KGE/RMSE annotated. Reads
whatever `output.nc` currently exists for `model` – it does not run the
model itself, and has no notion of "best"/calibrated parameters. To
compare against a calibrated run, first write the winning parameters
back
([`write_best_calib_to_par_files()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/write_best_calib_to_par_files.md))
and re-run the model
([`run_ensemble_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_ensemble_wq.md))
before calling this. For comparing multiple coupled models at once, see
[`compare_plot()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/compare_plot.md)/[`scat_plot()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/scat_plot.md)
instead.

## Usage

``` r
plot_model_vs_obs_wq(
  config_file,
  model,
  vars = NULL,
  obs_data,
  variable_global_name,
  y_title = variable_global_name,
  conversion_factor = NULL,
  dict_file = NULL
)
```

## Arguments

- config_file:

  character; path to the Output config YAML (as used by
  [`load_config()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/load_config-colon.md)/[`get_output_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/get_output_wq.md)).

- model:

  character; model to extract, e.g. `"GLM-AED2"`, `"GOTM-WET"`,
  `"GOTM-Selmaprotbas"`, or `"Simstrat-AED2"` (also accepts the short
  forms `"GLM"`, `"WET"`, `"SELMAPROTBAS"`, `"SIMSTRAT"`).

- vars:

  character or `NULL`. Model-native variable name to extract (passed to
  [`get_output_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/get_output_wq.md)'s
  `vars` argument). If `NULL` (default), auto-derived from the metrics
  dictionary's `variable_model_name` for `model`/`variable_global_name`
  – the same lookup calibration itself uses. Only single-variable
  extraction is supported here.

- obs_data:

  character or data.frame; either a path to a CSV, or an already-loaded
  data frame, with columns `datetime`, `depth`, `variable_global_name`,
  `value`.

- variable_global_name:

  character; which `variable_global_name` in `obs_data` to compare
  against.

- y_title:

  character; y-axis label for the plot (e.g. `"DO (g/m3)"`).

- conversion_factor:

  numeric or `NULL`. Applied to the model output so it matches
  `obs_data`'s (harmonized) units, since model output from
  [`get_output_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/get_output_wq.md)
  is in model-native units (e.g. GLM's DO is mmol O2/m3, not grams/m3 as
  in a typical observed CSV). If `NULL` (default), auto-derived from the
  metrics dictionary the same way
  [`run_lhc_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)'s
  calibration scoring does: looked up by `model`/`variable_global_name`.
  Pass a number explicitly to override the dictionary lookup.

- dict_file:

  character, data.frame, or `NULL`. Metrics dictionary source passed to
  the internal dictionary loader when auto-deriving
  `vars`/`conversion_factor`. If `NULL` (default), uses
  `load_config(config_file)$metrics_dict_file`, falling back to the
  package's bundled default dictionary.

## Value

A list with:

- plot:

  A ggplot2 object: one facet per observed depth, modeled line vs.
  observed points, with per-depth KGE/RMSE in the facet strip.

- data:

  The joined long-format data frame (`datetime`, `depth`, `Predicted`,
  `Observed`) used to build the plot.

- stats:

  A data frame with one row per depth: `depth`, `NSE`, `RMSE`, `NRMSE`,
  `PBIAS`, `KGE`, `n`.
