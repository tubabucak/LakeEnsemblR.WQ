test_that("plot_anoxic_metrics() errors clearly on an unusable metrics_list", {
  expect_error(
    plot_anoxic_metrics(metrics_list = list()),
    "Could not locate anoxia metric block"
  )
})

test_that("plot_anoxic_metrics() errors clearly on a missing NetCDF file", {
  # As with plot_ice_metrics(): a non-existent path falls through to
  # list-mode rather than a dedicated file-not-found check.
  expect_error(
    plot_anoxic_metrics(metrics_list = "does-not-exist.nc"),
    "Could not locate anoxia metric block"
  )
})
