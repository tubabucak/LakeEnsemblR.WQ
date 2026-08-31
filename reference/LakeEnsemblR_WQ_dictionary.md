# LakeEnsemblR.WQ parameter dictionary

The master dictionary of biogeochemical model parameters used throughout
the calibration workflow
([`create_calibration_tables`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/create_calibration_tables.md),
[`set_value_config`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/set_value_config.md),
[`export_config_wq`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/export_config_wq.md),
and related functions). Each row maps one parameter, for one coupled
model, to its default value, physical location in that model's native
config file, and (for module/domain/process/subprocess) its place in the
package's calibration-table hierarchy.

## Usage

``` r
LakeEnsemblR_WQ_dictionary
```

## Format

A data frame with (at least) the following columns:

- module:

  Biogeochemical module the parameter belongs to (e.g. `"carbon"`,
  `"nitrogen"`, `"phytoplankton"`).

- domain:

  Where the process occurs (e.g. `"water"`, `"sediment"`).

- process:

  Higher-level process category (e.g. `"growth"`, `"nitrification"`).

- subprocess:

  More specific process label within `process`.

- model:

  Short model key the parameter applies to (e.g. `"aed2"`, `"wet"`,
  `"selmaprotbas"`, `"pclake"`).

- parameter:

  The parameter's native name in that model.

- path:

  Location of the parameter within the model's native config file (e.g.
  `"aed2_carbon/ionic"`), used by
  [`set_value_config`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/set_value_config.md)
  to write values in place.

- unit:

  Parameter's unit, or a type marker such as `"(integer)"`/`"(boolean)"`
  for non-continuous parameters – see
  [`create_calibration_tables`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/create_calibration_tables.md),
  which excludes these from percentage-based calibration bounds.

- default:

  Default value shipped with the model/dictionary.

- min:

  Literature/plausible-range minimum for the parameter, when known.
  Carried through by
  [`create_calibration_tables`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/create_calibration_tables.md)
  as the `dict_min` reference column, and used as a fallback bound when
  `default` is `0` (see
  [`create_calibration_tables`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/create_calibration_tables.md)
  for why a percentage-of-default bound doesn't work in that case).

- max:

  Literature/plausible-range maximum for the parameter, when known. Same
  role as `min`, carried through as `dict_max`.

- version:

  Dictionary/model version this row applies to.

- note:

  Free-text description of the parameter.

## Source

`data-raw/LakeEnsemblR_WQ_dictionary_20260811.csv`
