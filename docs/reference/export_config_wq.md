# Export settings to model-specific configuration files for LER.WQ

Takes the high-level settings from a LakeEnsemblR_WQ configuration file
and distributes them into the model-specific configuration files (e.g.,
FABM, AED) It handles module activation, parameter mapping, and ensures
YAML boolean consistency. If Simstrat is used with AED2, it also
generates the necessary inflow `.dat` files.

## Usage

``` r
export_config_wq(
  config_file,
  folder = ".",
  verbose = FALSE,
  convert_from_lakeensemblr = TRUE,
  ler_config_file = "LakeEnsemblR.yaml",
  overwrite = FALSE
)
```

## Arguments

- config_file:

  character; name of the LakeEnsemblR_WQ master configuration file
  (e.g., "LakeEnsemblR_WQ.yaml").

- folder:

  character; path to the directory containing the configuration files.
  Defaults to the current working directory (".").

- verbose:

  logical; if TRUE, prints detailed information about changed parameters
  to the console.

- convert_from_lakeensemblr:

  logical; if TRUE, runs a conversion step to sync settings from the
  physical LakeEnsemblR configuration before exporting WQ settings.

- ler_config_file:

  character; name of the base LakeEnsemblR (physical) config file.

- overwrite:

  logical; if TRUE, overwrites existing inflow files. Defaults to FALSE.

## Details

The function automates the setup of water quality modules. For
Simstrat-AED2 setups, it automatically calls
`generate_simstrat_aed2_inflows` to create template boundary condition
files for all active AED2 modules, including expanded phytoplankton
groups.

## Examples

``` r
if (FALSE) { # \dontrun{
export_config_wq(config_file = "LakeEnsemblR_WQ.yaml", 
                 folder = "/Model_setup", 
                 verbose = TRUE)
} # }
```
