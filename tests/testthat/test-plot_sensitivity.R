# Hand-built stand-in for run_sensitivity(output_mode = "raw") output: no
# model run needed.
.fake_sens <- function(n_steps = 3, named = TRUE) {
  dt <- as.POSIXct("2020-01-01", tz = "UTC") + 86400 * 0:3
  lapply(seq_len(n_steps), function(i) {
    df <- data.frame(datetime = dt, Depth_1 = i * c(1, 2, 3, 4), Depth_5 = i * 10 + 0:3)
    list(param_value = i / 10, output = if (named) list(myvar = df) else list(df))
  })
}

test_that("sensitivity_to_long stacks all steps and depths", {
  long <- sensitivity_to_long(.fake_sens())

  expect_named(long, c("datetime", "depth", "value", "iteration", "param_value"))
  expect_equal(nrow(long), 3 * 4 * 2)
  expect_setequal(unique(long$depth), c(1, 5))
  expect_equal(sort(unique(long$param_value)), c(0.1, 0.2, 0.3))
})

test_that("sensitivity_to_long picks the nearest depth", {
  long <- sensitivity_to_long(.fake_sens(), depth = 4)

  expect_equal(unique(long$depth), 5)
  expect_equal(attr(long, "depth_used"), 5)
  expect_equal(nrow(long), 3 * 4)
})

test_that("sensitivity_to_long skips steps without usable output, with a warning", {
  res <- .fake_sens()
  res[[2]]$output <- NULL

  expect_warning(long <- sensitivity_to_long(res), "no usable output")
  expect_equal(sort(unique(long$iteration)), c(1, 3))
})

test_that("sensitivity_to_long rejects metrics-mode output", {
  res <- list(list(param_value = 1, metrics = list()))

  expect_error(sensitivity_to_long(res), "output_mode")
})

test_that("plot_sensitivity returns a ggplot and validates quantiles", {
  skip_if_not_installed("ggplot2")
  p <- plot_sensitivity(.fake_sens(), depth = 1)

  expect_s3_class(p, "ggplot")
  expect_error(plot_sensitivity(.fake_sens(), quantiles = c(0.9, 0.1)), "quantiles")
})
