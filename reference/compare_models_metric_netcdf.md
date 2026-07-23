# Compare one metric across models from NetCDF output

Plot a selected metric across models directly from a NetCDF file created
by create_netcdf_output().

## Usage

``` r
compare_models_metric_netcdf(
  nc_file,
  metric,
  models = NULL,
  depth = NULL,
  member = 1
)
```

## Arguments

- nc_file:

  character; path to NetCDF file.

- metric:

  character; variable name in NetCDF to compare.

- models:

  character vector; optional subset/order of models. If NULL, all models
  in the NetCDF model attribute are used except Obs.

- depth:

  numeric; optional depth selector for 3D variables. For 2D variables
  this is ignored.

- member:

  integer; member index to plot (default 1).

## Value

A list with:

- plot: ggplot object.

- data: long-format data used for plotting.

- dimension_info: list with variable dimensions and selected depth.
