test_that("cal_bot_surf_temp_dif() uses the last non-NA column as 'bottom'", {
  temp_data <- data.frame(
    datetime = c("d1", "d2"),
    Depth_1  = c(20, 15),
    Depth_2  = c(18, NA),
    Depth_3  = c(16, NA)
  )

  out <- cal_bot_surf_temp_dif(temp_data)

  # Row 1: surface 20, deepest valid value 16 -> 20 - 16 = 4
  # Row 2: surface 15, no other valid depths -> last valid value is 15 itself -> 0
  expect_equal(out$temp_diff, c(4, 0))
})

test_that("cal_bot_surf_temp_dif() returns NA when the surface value itself is NA", {
  temp_data <- data.frame(datetime = "d1", Depth_1 = NA_real_, Depth_2 = 10)

  out <- cal_bot_surf_temp_dif(temp_data)

  expect_true(is.na(out$temp_diff))
})

test_that("cal_bot_surf_temp_dif() preserves datetime and row order", {
  temp_data <- data.frame(datetime = c("a", "b", "c"),
                          Depth_1 = c(10, 11, 12),
                          Depth_2 = c(9, 10, 11))

  out <- cal_bot_surf_temp_dif(temp_data)

  expect_equal(out$datetime, c("a", "b", "c"))
  expect_equal(out$temp_diff, c(1, 1, 1))
})
