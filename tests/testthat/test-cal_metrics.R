test_that("cal_metrics() validates model_filter before touching any files", {
  # model_filter is checked before metric_yaml_file is even read, so this is
  # reachable with paths that don't exist.
  expect_error(
    cal_metrics(metric_yaml_file = "does-not-exist.yaml", model_filter = 123,
               wq_config_file = "does-not-exist.yaml"),
    "model_filter must be either 'all' or a character vector"
  )
})
