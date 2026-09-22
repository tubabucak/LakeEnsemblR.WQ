test_that("plot_ice_metrics() errors clearly on an unusable metrics_list", {
  expect_error(
    plot_ice_metrics(metrics_list = list()),
    "Could not locate an ice metric block"
  )
})

test_that("plot_ice_metrics() errors clearly when the metric block has no models", {
  # Named list whose single element (the metric block itself) has no names.
  bad_block <- list(Ice_Duration_Days = list(1, 2))

  expect_error(
    plot_ice_metrics(metrics_list = bad_block),
    "No model entries found in ice metric block"
  )
})

test_that("plot_ice_metrics() errors clearly on a missing NetCDF file", {
  # A non-existent path that doesn't end in .nc falls through to the
  # list-mode branch instead, so use a name that does look like a NetCDF
  # path but doesn't exist -- file.exists() is FALSE, so it's still treated
  # as list input and hits the "no metric block" error rather than a
  # dedicated file-not-found check; this documents that current behavior.
  expect_error(
    plot_ice_metrics(metrics_list = "does-not-exist.nc"),
    "Could not locate an ice metric block"
  )
})
