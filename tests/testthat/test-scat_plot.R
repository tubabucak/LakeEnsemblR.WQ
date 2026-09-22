# Minimal wide-format (datetime, Depth_*) fixture -- no real model run needed.
.fake_wide <- function(seed) {
  set.seed(seed)
  data.frame(
    datetime = as.POSIXct("2020-01-01", tz = "UTC") + 86400 * 0:4,
    Depth_1  = seed + 1:5,
    Depth_5  = seed + 6:10
  )
}

test_that("scat_plot() returns a plot and per-model stats for well-formed input", {
  skip_if_not_installed("ggplot2")

  obs <- .fake_wide(0)
  res <- scat_plot(
    temp_glm        = .fake_wide(1),
    temp_wet        = .fake_wide(2),
    temp_selma      = .fake_wide(3),
    temp_avg        = .fake_wide(4),
    temp_avg_pareto = .fake_wide(5),
    temp_obs        = obs,
    y_title         = "Temp (degC)"
  )

  expect_type(res, "list")
  expect_s3_class(res[[1]], "ggplot")
})

test_that("scat_plot() silently produces NA stats (not an error) when a model has no matching depths/dates", {
  # Documents current behavior: a model whose dates never overlap with
  # temp_obs joins to zero rows and its stats come back as NA rather than
  # scat_plot() raising an error -- worth knowing if you rely on this to
  # catch a genuinely broken/misaligned model run.
  obs <- .fake_wide(0)
  mismatched <- data.frame(
    datetime = as.POSIXct("2099-01-01", tz = "UTC") + 86400 * 0:4,
    Depth_1  = 1:5, Depth_5 = 6:10
  )

  res <- scat_plot(
    temp_glm        = mismatched,
    temp_wet        = .fake_wide(2),
    temp_selma      = .fake_wide(3),
    temp_avg        = .fake_wide(4),
    temp_avg_pareto = .fake_wide(5),
    temp_obs        = obs,
    y_title         = "Temp (degC)"
  )

  expect_true(all(is.na(unlist(res[[2]]$GLM))))
})
