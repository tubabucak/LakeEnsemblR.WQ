test_that("compare_models_metric() validates metric_out", {
  expect_error(
    compare_models_metric(metric_out = list(), metric = "Temp_degreeCelcius"),
    "metric_out must be a non-empty list"
  )
  expect_error(
    compare_models_metric(metric_out = "not a list", metric = "Temp_degreeCelcius"),
    "metric_out must be a non-empty list"
  )
})

test_that("compare_models_metric() validates metric", {
  metric_out <- list(Temp_degreeCelcius = list())

  expect_error(
    compare_models_metric(metric_out = metric_out, metric = c("a", "b")),
    "metric must be a single non-empty character string"
  )
  expect_error(
    compare_models_metric(metric_out = metric_out, metric = ""),
    "metric must be a single non-empty character string"
  )
})

test_that("compare_models_metric() errors clearly when the metric isn't in metric_out", {
  metric_out <- list(Temp_degreeCelcius = list())

  expect_error(
    compare_models_metric(metric_out = metric_out, metric = "DO_gramsPerCubicMeter"),
    "Metric not found in metric_out"
  )
})

test_that("compare_models_metric_netcdf() errors clearly on a missing file", {
  expect_error(
    compare_models_metric_netcdf(nc_file = "does-not-exist.nc", metric = "Temp_degreeCelcius"),
    "NetCDF file not found"
  )
})

test_that("compare_models_metric_netcdf() validates metric", {
  f <- tempfile(fileext = ".nc")
  file.create(f)
  on.exit(unlink(f))

  expect_error(
    compare_models_metric_netcdf(nc_file = f, metric = c("a", "b")),
    "metric must be a single non-empty character string"
  )
})
