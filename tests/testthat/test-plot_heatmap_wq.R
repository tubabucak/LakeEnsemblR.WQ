test_that("plot_heatmap_wq() errors clearly on a missing file", {
  expect_error(
    plot_heatmap_wq(ncdf = "does-not-exist.nc", metric = "Temp_degreeCelcius"),
    "File does not exist"
  )
})

test_that("plot_heatmap_wq() validates 'metric'", {
  f <- tempfile(fileext = ".nc")
  file.create(f)
  on.exit(unlink(f))

  expect_error(
    plot_heatmap_wq(ncdf = f, metric = c("a", "b")),
    "metric must be a single non-empty character string"
  )
  expect_error(
    plot_heatmap_wq(ncdf = f, metric = ""),
    "metric must be a single non-empty character string"
  )
})

test_that("plot_heatmap_wq() errors clearly when the metric isn't in the file", {
  skip_if_not_installed("ncdf4")
  f <- tempfile(fileext = ".nc")
  on.exit(unlink(f))

  dim_depth <- ncdf4::ncdim_def("depth", "m", 0:1)
  dim_time  <- ncdf4::ncdim_def("time", "seconds since 1970-01-01", 0:1)
  var_other <- ncdf4::ncvar_def("Other_var", "units", list(dim_depth, dim_time))
  nc <- ncdf4::nc_create(f, list(var_other))
  ncdf4::nc_close(nc)

  expect_error(
    plot_heatmap_wq(ncdf = f, metric = "Temp_degreeCelcius"),
    "Metric variable not found in NetCDF"
  )
})
