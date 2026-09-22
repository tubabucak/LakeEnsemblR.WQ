test_that("plot_strat_metrics() errors clearly when the metric list has no models", {
  expect_error(
    plot_strat_metrics(metrics_list = list()),
    "No model entries found in the supplied metric list"
  )
})

test_that("plot_strat_metrics() errors clearly on a missing NetCDF file", {
  expect_error(
    plot_strat_metrics(metrics_list = "does-not-exist.nc"),
    "No model entries found in the supplied metric list"
  )
})
