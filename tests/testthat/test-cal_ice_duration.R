test_that("cal_ice_duration() counts days with ice_thickness > 0 per year", {
  ice_sum <- data.frame(
    datetime = as.Date(c("2020-01-01", "2020-01-02", "2020-06-01", "2021-01-01")),
    thickness = c(0.1, 0.2, 0, 0.05)
  )

  out <- cal_ice_duration(ice_sum)

  expect_setequal(names(out), c("ice_duration_period", "ice_thickness"))
  expect_equal(out$ice_duration_period$unique_years, c(2020, 2021))
  # 2020: two days with thickness > 0 (the third, 0, doesn't count); 2021: one day
  expect_equal(out$ice_duration_period$ice_duration_day, c(2, 1))
})

test_that("cal_ice_duration() reports zero ice days for a year with no ice", {
  ice_sum <- data.frame(
    datetime = as.Date(c("2020-01-01", "2020-01-02")),
    thickness = c(0, 0)
  )

  out <- cal_ice_duration(ice_sum)

  expect_equal(out$ice_duration_period$ice_duration_day, 0)
})

test_that("cal_ice_duration() renames whatever 2 columns it's given to datetime/ice_thickness", {
  # The function forcibly overwrites colnames() positionally -- document that
  # contract explicitly, since passing anything other than exactly
  # (datetime, ice_thickness) in that order silently mislabels the data.
  ice_sum <- data.frame(some_date = as.Date("2020-01-01"), some_value = 0.5)

  out <- cal_ice_duration(ice_sum)

  expect_equal(names(out$ice_thickness)[1:2], c("datetime", "ice_thickness"))
})

test_that("cal_ice_duration() errors if given a data.frame without exactly 2 columns", {
  ice_sum <- data.frame(datetime = as.Date("2020-01-01"), thickness = 0.5, extra = 1)

  expect_error(cal_ice_duration(ice_sum))
})
