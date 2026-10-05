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

test_that("plot_strat_metrics() explains a metric missing from cal_metrics() output", {
  metrics <- list(Temp_degreeCelcius = list(GLM = list(data.frame(x = 1))),
                  DO_gramsPerCubicMeter = list(GLM = list(data.frame(x = 1))))

  expect_error(plot_strat_metrics(metrics),
               "not found in metrics_list.*Output.yaml.*Temp_degreeCelcius")
})
