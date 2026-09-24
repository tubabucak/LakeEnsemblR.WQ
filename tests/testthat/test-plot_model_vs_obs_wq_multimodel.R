# Exercises the new multi-model path with a mocked get_output_wq() -- no
# real model output needed. vars/conversion_factor are supplied explicitly
# so the metrics-dictionary lookup (and therefore load_config()) is never
# hit, keeping this fully self-contained.

.fake_sim_df <- function(shift) {
  data.frame(
    datetime = as.POSIXct("2020-01-01", tz = "UTC") + 86400 * 0:4,
    Depth_1  = shift + 1:5,
    Depth_5  = shift + 6:10
  )
}

test_that("plot_model_vs_obs_wq() combines multiple models into one plot/stats table", {
  skip_if_not_installed("ggplot2")

  obs <- data.frame(
    datetime = rep(as.character(as.Date("2020-01-01") + 0:4), 2),
    depth = rep(c(1, 5), each = 5),
    variable_global_name = "Temp_degreeCelsius",
    value = c(1:5, 6:10)
  )

  testthat::local_mocked_bindings(
    get_output_wq = function(config_file, model, vars, obs_depths, depth_01,
                             conversion_factor) {
      shift <- if (identical(model, "GLM")) 0 else 10
      list(.fake_sim_df(shift))
    },
    .package = "LakeEnsemblR.WQ"
  )

  out <- plot_model_vs_obs_wq(
    config_file           = "unused.yaml",
    model                 = c("GLM-AED2", "Simstrat-AED2"),
    vars                  = "temp",
    obs_data              = obs,
    variable_global_name  = "Temp_degreeCelsius",
    conversion_factor     = 1
  )

  expect_s3_class(out$plot, "ggplot")
  expect_setequal(unique(out$data$Model), c("GLM", "SIMSTRAT"))
  expect_setequal(unique(out$stats$Model), c("GLM", "SIMSTRAT"))
  # GLM's fake series matches obs exactly (shift = 0) -> perfect score.
  glm_stats <- out$stats[out$stats$Model == "GLM", ]
  expect_true(all(glm_stats$KGE > 0.99))
  # Simstrat's fake series is offset by 10 -> much worse score than GLM's.
  sim_stats <- out$stats[out$stats$Model == "SIMSTRAT", ]
  expect_true(all(sim_stats$KGE < glm_stats$KGE))
})

test_that("plot_model_vs_obs_wq() with a single model still returns per-depth KGE/RMSE labels", {
  skip_if_not_installed("ggplot2")

  obs <- data.frame(
    datetime = as.character(as.Date("2020-01-01") + 0:4),
    depth = 1,
    variable_global_name = "Temp_degreeCelsius",
    value = 1:5
  )

  testthat::local_mocked_bindings(
    get_output_wq = function(config_file, model, vars, obs_depths, depth_01,
                             conversion_factor) {
      list(.fake_sim_df(0))
    },
    .package = "LakeEnsemblR.WQ"
  )

  out <- plot_model_vs_obs_wq(
    config_file           = "unused.yaml",
    model                 = "GLM-AED2",
    vars                  = "temp",
    obs_data              = obs,
    variable_global_name  = "Temp_degreeCelsius",
    conversion_factor     = 1
  )

  expect_equal(unique(out$data$Model), "GLM")
  expect_true(!is.null(out$stats$Model))
})
