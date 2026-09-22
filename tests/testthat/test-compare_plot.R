# Minimal wide-format (datetime, Depth_*) fixture -- no real model run needed.
.fake_wide <- function(seed) {
  data.frame(
    datetime = as.POSIXct("2020-01-01", tz = "UTC") + 86400 * 0:4,
    Depth_1  = seed + 1:5
  )
}

test_that("compare_plot() returns a plot plus per-model stats for well-formed input", {
  skip_if_not_installed("ggplot2")

  res <- compare_plot(
    data_glm      = .fake_wide(1),
    data_wet      = .fake_wide(2),
    data_selma    = .fake_wide(3),
    data_simstrat = .fake_wide(4),
    data_obs      = .fake_wide(0),
    depth         = 1,
    y_title       = "Temp (degC)"
  )

  expect_type(res, "list")
  expect_length(res, 5)
  expect_s3_class(res[[1]], "ggplot")
})

test_that("compare_plot() errors clearly when the requested depth column is missing", {
  expect_error(
    compare_plot(
      data_glm      = .fake_wide(1),
      data_wet      = .fake_wide(2),
      data_selma    = .fake_wide(3),
      data_simstrat = .fake_wide(4),
      data_obs      = .fake_wide(0),
      depth         = 99,
      y_title       = "Temp (degC)"
    )
  )
})
