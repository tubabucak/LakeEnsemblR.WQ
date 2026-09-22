test_that("plot_model_vs_obs_wq() validates obs_data columns", {
  bad_obs <- data.frame(datetime = "2020-01-01", depth = 1, value = 1)

  expect_error(
    plot_model_vs_obs_wq(
      config_file = "unused.yaml", model = "GLM-AED2", obs_data = bad_obs,
      variable_global_name = "DO_gramsPerCubicMeter"
    ),
    "missing required column"
  )
})

test_that("plot_model_vs_obs_wq() errors when variable_global_name has no rows", {
  obs <- data.frame(
    datetime = "2020-01-01", depth = 1,
    variable_global_name = "Temp_degreeCelsius", value = 5
  )

  expect_error(
    plot_model_vs_obs_wq(
      config_file = "unused.yaml", model = "GLM-AED2", obs_data = obs,
      variable_global_name = "DO_gramsPerCubicMeter"
    ),
    "No rows in 'obs_data' match variable_global_name"
  )
})

test_that("plot_model_vs_obs_wq() errors when no usable depths remain", {
  obs <- data.frame(
    datetime = "2020-01-01", depth = NA_real_,
    variable_global_name = "DO_gramsPerCubicMeter", value = 5
  )

  expect_error(
    plot_model_vs_obs_wq(
      config_file = "unused.yaml", model = "GLM-AED2", obs_data = obs,
      variable_global_name = "DO_gramsPerCubicMeter"
    ),
    "No usable \\(non-NA\\) depths found"
  )
})

test_that("plot_model_vs_obs_wq() rejects an unrecognized model", {
  obs <- data.frame(
    datetime = "2020-01-01", depth = 1,
    variable_global_name = "DO_gramsPerCubicMeter", value = 5
  )

  expect_error(
    plot_model_vs_obs_wq(
      config_file = "unused.yaml", model = "NOT-A-MODEL", obs_data = obs,
      variable_global_name = "DO_gramsPerCubicMeter"
    ),
    "Could not determine model type"
  )
})
