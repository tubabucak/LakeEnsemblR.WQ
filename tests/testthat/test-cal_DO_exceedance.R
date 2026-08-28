test_that("cal_DO_exceedance() sorts descending and computes exceedance percentages", {
  oxy_data <- data.frame(datetime = 1:5, Depth_1 = c(10, 8, 6, 4, 2))

  out <- cal_DO_exceedance(oxy_data, depth = 1)

  expect_equal(out$DO, c(10, 8, 6, 4, 2))
  expect_equal(out$exceedance, c(20, 40, 60, 80, 100))
})

test_that("cal_DO_exceedance() drops NA values before ranking", {
  oxy_data <- data.frame(datetime = 1:3, Depth_1 = c(10, NA, 6))

  out <- cal_DO_exceedance(oxy_data, depth = 1)

  expect_equal(nrow(out), 2)
  expect_equal(out$DO, c(10, 6))
  expect_equal(out$exceedance, c(50, 100))
})

test_that("cal_DO_exceedance() errors when the requested depth column is missing", {
  oxy_data <- data.frame(datetime = 1:3, Depth_1 = c(10, 8, 6))

  expect_error(cal_DO_exceedance(oxy_data, depth = 5), regexp = "depth column is not found")
})
