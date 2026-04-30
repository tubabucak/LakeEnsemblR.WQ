# Set value in model-specific config file

Set value in model-specific config file based on dictionary path

## Usage

``` r
set_value_config(
  config_file,
  module,
  group_name = NULL,
  group_position = NULL,
  domain,
  process,
  subprocess,
  model_coupled,
  parameter,
  value,
  folder,
  verbose = FALSE
)
```

## Arguments

- config_file:

  character; path to LakeEnsemblR_WQ config file

- module:

  character;

- group_name:

  character; only for biological modules

- group_position:

  integer; only for biological modules

- domain:

  character;

- process:

  character;

- subprocess:

  character;

- model_coupled:

  character; options one of "GLM-AED2", "GOTM-Selmaprotbas", "GOTM-WET",
  "Simstrat-AED2", "MyLake", "PCLake"

- parameter:

  character;

- value:

  character or numeric; what value to enter

- folder:

  path; relative to what folder

- verbose:

  boolean; print output to console
