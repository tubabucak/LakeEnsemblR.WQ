# Reads and sets nutrient inputs for all models

Handles nutrient concentrations in inflows and other sources

## Usage

``` r
export_inputs(
  config_file,
  folder = ".",
  ler_config_file = "LakeEnsemblR.yaml",
  verbose = FALSE
)
```

## Arguments

- config_file:

  character; name of LakeEnsemblR_WQ config file

- folder:

  path; location of config_file

- ler_config_file:

  character; name of LakeEnsemblR config file

- verbose:

  boolean; print changed parameters on screen
