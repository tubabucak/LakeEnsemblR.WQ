test_that("cal_nitrif_selma() matches the documented formula directly", {
  DO   <- data.frame(datetime = "d1", Depth_1 = 8)
  Temp <- data.frame(datetime = "d1", Depth_1 = 20)
  NH4  <- data.frame(datetime = "d1", Depth_1 = 1)

  out <- cal_nitrif_selma(DO = DO, Temp = Temp, NH4 = NH4)

  # Encode the formula itself as the expectation (rather than a hand-computed
  # magic number) so this documents the contract, not just today's output.
  expected <- ((8 / (0.01 + 8)) * 0.1 * exp(0.11 * 20)) * 1 * 14 / 1000
  expect_equal(out$Depth_1, expected)
})

test_that("cal_nitrif_selma() keeps datetime as the first column", {
  DO   <- data.frame(datetime = c("d1", "d2"), Depth_1 = c(8, 7), Depth_2 = c(6, 5))
  Temp <- data.frame(datetime = c("d1", "d2"), Depth_1 = c(20, 18), Depth_2 = c(15, 14))
  NH4  <- data.frame(datetime = c("d1", "d2"), Depth_1 = c(1, 1.2), Depth_2 = c(0.8, 0.9))

  out <- cal_nitrif_selma(DO = DO, Temp = Temp, NH4 = NH4)

  expect_equal(names(out)[1], "datetime")
  expect_equal(out$datetime, c("d1", "d2"))
  expect_setequal(names(out), c("datetime", "Depth_1", "Depth_2"))
})

test_that("cal_nitrif_selma() returns zero nitrification when NH4 is zero", {
  DO   <- data.frame(datetime = "d1", Depth_1 = 8)
  Temp <- data.frame(datetime = "d1", Depth_1 = 20)
  NH4  <- data.frame(datetime = "d1", Depth_1 = 0)

  out <- cal_nitrif_selma(DO = DO, Temp = Temp, NH4 = NH4)

  expect_equal(out$Depth_1, 0)
})
