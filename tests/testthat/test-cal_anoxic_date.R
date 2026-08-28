# Fixture: 3 depths (0, 5, 10 m; areas 100, 50, 10 m2), 5 days. Depth_5 is
# anoxic (< threshold) on days 2-3 (a 2-day run) and again on day 5 (a
# separate 1-day run) -- deliberately two distinct anoxic periods so
# duration = "full" and duration = "longest" give genuinely different
# answers, which is also what exercises the match.arg() fix (previously
# duration = "longest" errored outright; see R/cal_anoxic_date.r).
.anoxic_fixture <- function() {
  bathy_file <- data.frame(depths = c(0, 5, 10), areas = c(100, 50, 10))
  oxy_data <- data.frame(
    datetime = as.Date("2020-01-01") + 0:4,
    Depth_0  = rep(5, 5),
    Depth_5  = c(5, 0.5, 0.5, 5, 0.5),
    Depth_10 = rep(5, 5)
  )
  list(bathy_file = bathy_file, oxy_data = oxy_data)
}

test_that("cal_anoxic_date() with duration = 'full' sums all anoxic days/area", {
  fx <- .anoxic_fixture()

  out <- cal_anoxic_date(fx$oxy_data, fx$bathy_file, threshold = 1, duration = "full")

  # area_anoxic per day: 0, 50, 50, 0, 50 -> sum = 150; surface area = 100
  expect_equal(out$AF_yearly$AF_total, 1.5)
  expect_equal(out$num_anoxic_days$num_anoxic_days, 3)
})

test_that("cal_anoxic_date() with duration = 'longest' only counts the longest run (and does not error)", {
  fx <- .anoxic_fixture()

  # This is the exact call shape that used to fail with
  # "'arg' should be one of "full"" before the match.arg() default was fixed.
  out <- cal_anoxic_date(fx$oxy_data, fx$bathy_file, threshold = 1, duration = "longest")

  # Longest run is days 2-3 (2 days, area_anoxic = 50 each) -> sum = 100
  expect_equal(out$AF_yearly$AF_total, 1.0)
  expect_equal(out$num_anoxic_days$num_anoxic_days, 2)
})

test_that("cal_anoxic_date() rejects an invalid duration value", {
  fx <- .anoxic_fixture()

  expect_error(
    cal_anoxic_date(fx$oxy_data, fx$bathy_file, duration = "not_a_real_option")
  )
})

test_that("cal_anoxic_date() reports no anoxia when nothing crosses the threshold", {
  fx <- .anoxic_fixture()
  fx$oxy_data$Depth_5 <- rep(5, 5)  # never anoxic

  out <- cal_anoxic_date(fx$oxy_data, fx$bathy_file, threshold = 1, duration = "full")

  expect_equal(out$AF_yearly$AF_total, 0)
  expect_equal(out$num_anoxic_days$num_anoxic_days, 0)
})
