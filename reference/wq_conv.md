# Molar mass conversion factors

Molar masses (g/mol) for the nutrient elements tracked across coupled
models, used to convert between mass-based and mole-based units (e.g.
grams per cubic meter to millimoles per cubic meter for GOTM-based
models) in
[`export_inputs`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/export_inputs.md).

## Usage

``` r
wq_conv
```

## Format

A data frame (single row) with columns `mol_mass_N`, `mol_mass_P`,
`mol_mass_Si`, `mol_mass_O2`, and `mol_mass_C`, giving the molar mass of
nitrogen, phosphorus, silicon, oxygen (as O2), and carbon respectively.

## Source

`data-raw/wq_conv.csv`
