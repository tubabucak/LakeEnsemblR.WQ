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
  out_file = "ensemble_output.nc",
  format = c("lerwq", "isimip"),
  isimip = list()
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

  character; optional path to LakeEnsemblR_WQ.yaml. Used to discover
  \`files\$LER_config_file\` if \`ler_config_file\` is NULL, and for the
  phyto-/zooplankton group names: per-group metrics are written as one
  variable per group, \`\<metric\>\_\<group\>\` (e.g.
  \`Phyto_C_miligramsPerCubicMeter_diatoms\`). Without it, the model's
  own variable name is used as suffix (e.g. \`...\_PHY_diatoms\`), which
  differs between models.

- compression:

  integer; compression level from 1 (least) to 9 (most).

- members:

  integer; number of ensemble members in output NetCDF.

- out_file:

  character; output NetCDF filename.

- format:

  character; `"lerwq"` (default) writes one ensemble file with all
  metrics and models. `"isimip"` writes ISIMIP3 lake-sector files
  instead: one file per variable and model, daily, full profile,
  dimensions `(time, levlak, lat, lon)` with a `depth(levlak)` variable,
  units in mol m-3 (g m-3 for `chl`). Variables: `chl`, `phytobio`
  (total and one file per group, e.g. `phytobio-diatoms`), `zoobio`,
  `tp`, `pp`, `tpd`, `tn`, `pn`, `tdn`, `do`, `doc`, `si`. `tpd` = PO4 +
  DOP, `pp` = TP - tpd, `tdn` = NO3 + NH4 + DON, `pn` = TN - tdn. A
  variable is skipped (with a message) when a metric it needs is not in
  `output_lists`. `out_file`, `out_time` and `members` are not used.

- isimip:

  list; settings for `format = "isimip"`, all optional: `forcing`
  (default `"gswp3-w5e5"`), `bias_adjustment` (default `""`, left out of
  the file name), `climate_scenario` (`"obsclim"`), `soc_scenario`
  (`"histsoc"`), `sens_scenario` (`"default"`), `lake` (default
  `location$name` from LakeEnsemblR.yaml), `model_names` (named vector
  overriding the ISIMIP model names, defaults `GLM = "glm-aed"`,
  `SIMSTRAT = "simstrat-aed2"`, `WET = "gotm-wet"`,
  `SELMAPROTBAS = "gotm-selmaprotbas"`), `time_ref` (`"1901-01-01"` for
  ISIMIP3a; use `"1601-01-01"` for ISIMIP3b), `contact`, `institution`,
  `comment` (global attributes; contact and institution are required by
  ISIMIP), `variables` (subset to write) and `out_dir` (default
  `folder/output/isimip`). File names follow
  `<model>_<forcing>[_<bias>]_<climate>_<soc>_<sens>_<var>_<lake>_daily_<start>_<end>.nc`.

## Value

Invisibly returns the output NetCDF file path (for `format = "isimip"`,
the paths of all files written).

## Examples

``` r
if (FALSE) { # \dontrun{
create_netcdf_output(metric_out, folder = ".", model = c("GLM", "SIMSTRAT"),
                     wq_config_file = "LakeEnsemblR_WQ.yaml",
                     format = "isimip",
                     isimip = list(contact = "name <mail>", institution = "Aarhus University"))
} # }
```
