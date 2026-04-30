# Sets PCLake physics settings

A LakeEnsemblR::export_config function for the PCLake physics settings
are based on the LakeEnsemblR config file

## Usage

``` r
export_pclake_physics(
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
