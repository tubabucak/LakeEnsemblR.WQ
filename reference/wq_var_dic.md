# Nutrient inflow variable dictionary

Maps each standardized nutrient inflow variable name (as used in the
nutrient inflow CSV read by
[`export_inputs`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/export_inputs.md)
– see
[`vignette("config-reference")`](https://tubabucak.github.io/LakeEnsemblR.WQ/articles/config-reference.md))
to its model-native equivalent for each coupled model.

## Usage

``` r
wq_var_dic
```

## Format

A data frame with columns:

- standard_name:

  Standardized variable name expected in the nutrient inflow CSV (e.g.
  `"wq_NO3_gramsPerCubicMeter"`), validated by `chk_names_nutr_flow()`.

- short_name:

  Short label for the variable.

- nutrient:

  Which nutrient element this variable represents (`"N"`, `"P"`, or
  `"Si"`).

- unit:

  Unit of `standard_name`.

- aed2, selmaprotbas, wet, mylake, pclake:

  The equivalent model-native variable name for each coupled model
  (`"-"` if not applicable to that model).

## Source

`data-raw/wq_var_dic.csv`
