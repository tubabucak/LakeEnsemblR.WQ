# Harmonized metrics dictionary

Maps each model's native output variable to a standardized, harmonized
metric name and unit, used by
[`cal_metrics`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
and
[`get_output_wq`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/get_output_wq.md)
to produce comparable output across coupled models. This is the same
dictionary referenced by the `Level1`/`Level2`/`Level3` blocks in
`Output.yaml` (see
[`vignette("config-reference")`](https://tubabucak.github.io/LakeEnsemblR.WQ/articles/config-reference.md)).

## Usage

``` r
Metrics_dict
```

## Format

A data frame with (at least) the following columns:

- metric_name:

  Full metric name as it appears in `Output.yaml` (e.g.
  `"Duration_of_Stratification"`).

- metric_short_name:

  Short label for the metric.

- domain:

  Where the metric applies (e.g. `"Water"`).

- module:

  Metric category (e.g. `"LER"`, `"Oxygen"`).

- level:

  Complexity tier: `"Level1"` (direct variables), `"Level2"`/`"Level3"`
  (derived metrics).

- variable_global_name:

  Harmonized variable name shared across models (e.g.
  `"DO_gramsPerCubicMeter"`) – the same name used in observed-data CSVs'
  `variable_global_name` column.

- unit_global:

  Unit of `variable_global_name`.

- variable_model_name_old, variable_model_name:

  The model-native variable name this harmonized metric is derived from.

- depth_01:

  Whether the variable has a depth dimension (`1`) or not (`0`).

- model:

  Which coupled model this row applies to (e.g. `"GLM"`, `"WET"`,
  `"SELMAPROTBAS"`, `"SIMSTRAT"`).

- unit_model:

  Native unit of the model variable.

- conversion_factor:

  Factor applied to convert from `unit_model` to `unit_global`.

- function_name:

  Name of the internal function used to compute this metric, when it's a
  derived (Level2/Level3) metric.

## Source

`data-raw/Metrics_dict_v2_add.csv`
