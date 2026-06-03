# Plot heat map from LakeEnsemblR.WQ NetCDF output

Plot a heat map for a metric stored in a NetCDF created by
create_netcdf_output(). Unlike LakeEnsemblR::plot_heatmap(), this
function uses the metric names as written in the file (e.g.,
Temp_degreeCelcius).

## Usage

``` r
plot_heatmap_wq(
  ncdf,
  metric,
  models = NULL,
  member = 1,
  spin_up = 0,
  tile_width = NULL,
  tile_height = NULL
)
```

## Arguments

- ncdf:

  character; path to NetCDF file.

- metric:

  character; metric variable name in the NetCDF.

- models:

  character vector; optional subset/order of model names. If NULL, all
  available models except Obs are used.

- member:

  integer; ensemble member index to plot. Defaults to 1.

- spin_up:

  numeric; number of days to omit from start of series.

- tile_width:

  numeric; optional tile width in seconds.

- tile_height:

  numeric; optional tile height in depth units.

## Value

ggplot object.
