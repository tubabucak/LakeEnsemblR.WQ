# Plot one or more models' output against observed data at matching depths

Extracts each requested model's simulated output at the depths present
in an observed dataset (via `get_output_wq(obs_depths = ...)`) and plots
it against the observations, one facet per depth, one line per model
(colored), with observed points overlaid once per facet. Reads whatever
`output.nc` currently exists for each `model` – it does not run the
model itself, and has no notion of "best"/calibrated parameters. To
compare against a calibrated run, first write the winning parameters
back
([`write_best_calib_to_par_files()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/write_best_calib_to_par_files.md))
and re-run the model
([`run_ensemble_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_ensemble_wq.md))
before calling this.

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
  dict_file = NULL,
  wq_config_file = NULL,
  depth_tol = 0.5
)
```

## Arguments

- config_file:

  character; path to the Output config YAML (as used by
  [`load_config()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/load_config-colon.md)/[`get_output_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/get_output_wq.md)).

- model:

  character vector; one or more models to extract, e.g. `"GLM-AED2"`,
  `"GOTM-WET"`, `"GOTM-Selmaprotbas"`, or `"Simstrat-AED2"` (also
  accepts the short forms `"GLM"`, `"WET"`, `"SELMAPROTBAS"`,
  `"SIMSTRAT"`). Passing more than one plots them together against the
  same observations, one line per model per depth facet; passing one
  reproduces the original single-model behavior (including the per-depth
  KGE/RMSE facet labels).

- vars:

  character or `NULL`. Model-native variable name to extract (passed to
  [`get_output_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/get_output_wq.md)'s
  `vars` argument). If `NULL` (default), auto-derived independently for
  each `model` from the metrics dictionary's `variable_model_name` for
  that `model`/`variable_global_name` – the same lookup calibration
  itself uses. Only single-variable extraction is supported here. A
  non-`NULL` value is used as-is for every requested model, so leave it
  `NULL` when `model` has more than one entry unless you're sure the
  same native variable name applies to all of them.

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
  in a typical observed CSV). If `NULL` (default), auto-derived
  independently for each `model` from the metrics dictionary the same
  way
  [`calib_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/calib_wq.md)'s
  calibration scoring does: looked up by that
  `model`/`variable_global_name`. Pass a number explicitly to override
  the dictionary lookup for every requested model.

- dict_file:

  character, data.frame, or `NULL`. Metrics dictionary source passed to
  the internal dictionary loader when auto-deriving
  `vars`/`conversion_factor`. If `NULL` (default), uses
  `load_config(config_file)$metrics_dict_file`, falling back to the
  package's bundled default dictionary.

- wq_config_file:

  character or `NULL`. Path to the `LakeEnsemblR_WQ.yaml` config file.
  Only needed for `model = "GOTM-Selmaprotbas"`/`"GOTM-WET"` when the
  auto-derived `vars` resolves to the dictionary's generic
  `"zooplankton_*"` placeholder – since SELMAPROTBAS/WET always create
  one named FABM instance per configured zooplankton group (never a
  literal instance called `"zooplankton"`), that placeholder is expanded
  into each group's own output variable (e.g. `"daphnia_c"`,
  `"cyclops_c"`), fetched, and summed into one total zooplankton series
  before plotting.

- depth_tol:

  numeric; tolerance (in the same units as `depth` in `obs_data`) used
  to bin observed depths before faceting. Depths are rounded to the
  nearest multiple of `depth_tol` (default `0.5`), so observations from
  different casts that land within `depth_tol` of each other (e.g.
  `21.9`, `22.0`, `22.3`) are treated as one sampling depth/facet
  instead of three, and averaged where they share a binned depth and
  datetime. Set to a smaller value (or `0`) to disable binning and facet
  on raw observed depths.

## Value

A list with:

- plot:

  A ggplot2 object: one facet per (binned) observed depth that has at
  least one matched observation for at least one model, one colored line
  per model plus observed points, with a legend distinguishing models
  (and "Observed"). Depths with no matched observation at all (e.g. an
  observed date that never lines up with any simulated one) are dropped
  rather than shown as an empty panel. When only one `model` is
  requested, the facet strip additionally shows that model's per-depth
  KGE/RMSE, matching the single-model behavior from before this function
  supported multiple models.

- data:

  The joined long-format data frame (`Model`, `datetime`, `depth`,
  `Predicted`, `Observed`) used to build the plot – covers the full
  simulated series at each depth kept in `plot` for each model, with
  `Observed` `NA` wherever there's no observation on that particular
  date (matching is by calendar date, not exact timestamp, since model
  output is daily-or-coarser while observed records can carry an
  arbitrary time-of-day).

- stats:

  A data frame with one row per `Model`/`depth` combination: `Model`,
  `depth`, `NSE`, `RMSE`, `NRMSE`, `PBIAS`, `KGE`, `n`.
