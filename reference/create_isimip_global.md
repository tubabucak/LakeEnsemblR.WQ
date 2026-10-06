# Write ISIMIP3 global lake-sector NetCDF files for many lakes

Writes water-quality output of many LakeEnsemblR.WQ lake runs into
ISIMIP3 `lakes_global` files: each lake goes into its cell of the global
0.5 degree grid (360 x 720; latitude 89.75 to -89.75, longitude -179.75
to 179.75), all other cells are 1e20. There is one file per variable,
model and decade (`1901_1910`, `1911_1920`, ...; the first and last
period end/start at the simulation years), daily, NETCDF4_CLASSIC (see
below) with compression level 5, float32 data, double coordinates,
`_FillValue` and `missing_value` 1e20.

## Usage

``` r
create_isimip_global(
  lakes,
  model,
  metric_yaml_file = "Output.yaml",
  wq_config_file = "LakeEnsemblR_WQ.yaml",
  start_year = NULL,
  end_year = NULL,
  phyto_groups = NULL,
  out_dir = "isimip_global",
  isimip = list(),
  verbose = TRUE
)
```

## Arguments

- lakes:

  data.frame; one row per lake with columns `lat` and `lon` (decimal
  degrees; snapped to the 0.5 degree grid cell they fall in – one lake
  per cell) and `folder` (the lake's LakeEnsemblR.WQ project folder;
  [`cal_metrics()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
  is run there) and/or `rds` (path to a saved
  [`cal_metrics()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
  result, used instead of running
  [`cal_metrics()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)).
  An optional `id` column is used in messages.

- model:

  character; models to write, e.g. `c("GLM", "SIMSTRAT")`.

- metric_yaml_file:

  character; metrics file inside each lake folder. It must enable
  `Temp_degreeCelcius` (for the thermocline) and the water-quality
  metrics to be written.

- wq_config_file:

  character; LakeEnsemblR_WQ config file inside each lake folder (for
  [`cal_metrics()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
  and the phytoplankton group names).

- start_year, end_year:

  integer; period to write. If `NULL`, taken from `time` in the first
  lake's LakeEnsemblR.yaml.

- phyto_groups:

  character or `NULL`; phytoplankton group names for the
  `phytobio-<group>` files. If `NULL`, collected from the lakes' WQ
  config files.

- out_dir:

  character; output directory.

- isimip:

  list; file-name parts and attributes, as for
  `create_netcdf_output(format = "isimip")`: `forcing`,
  `bias_adjustment`, `climate_scenario`, `soc_scenario`,
  `sens_scenario`, `model_names`, `time_ref` (`"1901-01-01"` for
  ISIMIP3a, `"1601-01-01"` for ISIMIP3b), `contact`, `institution`,
  `comment` and `variables`.

- verbose:

  logical; print progress per lake.

## Value

Invisibly, the paths of the files written. Files that would hold no data
at all (e.g. a variable none of the lakes has) are not kept.

## Details

The water-quality variables have dimensions `(time, levlak, lat, lon)`
with two levels: `levlak = 1` is the mean over the epilimnion and
`levlak = 2` the mean over the hypolimnion. They are split at the daily
thermocline from the lake's temperature profile
([`rLakeAnalyzer::thermo.depth()`](https://rdrr.io/pkg/rLakeAnalyzer/man/thermo.depth.html),
as used for the stratification metrics); when the lake is mixed (no
thermocline), both levels hold the mean over the whole water column.
Means are over the model layers in the
[`cal_metrics()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
output. See
[`create_netcdf_output()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/create_netcdf_output.md)
for the variables and how they are derived (`format = "isimip"`).

Lakes are processed one at a time and written straight into the files,
so memory use does not grow with the number of lakes.

File format: R's NetCDF packages cannot write compressed NETCDF4_CLASSIC
files, so the files are written as compressed NETCDF4 (within the
classic data model) and then converted to NETCDF4_CLASSIC with `nccopy`
from the netCDF tools, if it is on the PATH. Otherwise a message lists
how to convert them (`nccopy -k nc7 -d 5` or
`cdo -f nc4c -z zip_5 copy`).

## Examples

``` r
if (FALSE) { # \dontrun{
lakes <- data.frame(id = c("ravn", "mendota"),
                    lat = c(56.1, 43.1), lon = c(9.8, -89.4),
                    folder = c("runs/ravn", "runs/mendota"))
create_isimip_global(lakes, model = c("GLM", "SIMSTRAT"),
                     isimip = list(contact = "Name <mail>",
                                   institution = "Aarhus University"))
} # }
```
