# Create NetCDF output from model-specific runs

Create a NetCDF file from model output lists (e.g., temperature, ice
thickness).

## Usage

``` r
create_netcdf_output(
  output_lists,
  folder = ".",
  model,
  out_time = NULL,
  longitude = NULL,
  latitude = NULL,
  ler_config_file = NULL,
  wq_config_file = NULL,
  compression = 4,
  members = 25,
  out_file = "ensemble_output.nc"
)
```

## Arguments

- output_lists:

  list; list containing lists of output data.frames.

- folder:

  character; folder that contains model folders.

- model:

  character vector; model names to include (e.g., c("GOTM", "GLM",
  "Simstrat")).

- out_time:

  data.frame; optional data.frame with datetime column. If NULL, it is
  inferred from the first metric data.frame.

- longitude:

  numeric; longitude of lake in decimal degrees. If NULL, it is inferred
  from \`LakeEnsemblR.yaml\` when possible.

- latitude:

  numeric; latitude of lake in decimal degrees. If NULL, it is inferred
  from \`LakeEnsemblR.yaml\` when possible.

- ler_config_file:

  character; optional path to LakeEnsemblR.yaml. If NULL, the function
  tries \`files\$LER_config_file\` from \`wq_config_file\`, then
  \`folder/LakeEnsemblR.yaml\`.

- wq_config_file:

  character; optional path to LakeEnsemblR_WQ.yaml. Used only to
  discover \`files\$LER_config_file\` if \`ler_config_file\` is NULL.

- compression:

  integer; compression level from 1 (least) to 9 (most).

- members:

  integer; number of ensemble members in output NetCDF.

- out_file:

  character; output NetCDF filename.

## Value

Invisibly returns the output NetCDF file path.
