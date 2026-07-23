test_that("cal_stats returns perfect scores for identical observed/predicted", {
  obs <- c(1, 2, 3, 4, 5)
  pred <- obs

  st <- cal_stats(obs, pred)

  expect_equal(st$NSE, 1)
  expect_equal(st$RMSE, 0)
  expect_equal(st$NRMSE, 0)
  expect_equal(st$PBIAS, 0)
  expect_equal(st$KGE, 1, tolerance = 1e-8)
  expect_equal(st$residual, rep(0, 5))
})

test_that("cal_stats computes RMSE/NSE against hand-derived values", {
  obs  <- c(1, 2, 3, 4)
  pred <- c(2, 2, 2, 2)

  st <- cal_stats(obs, pred)

  # RMSE = sqrt(mean((obs-pred)^2)) = sqrt(mean(c(1, 0, 1, 4))) = sqrt(1.5)
  expect_equal(st$RMSE, sqrt(1.5))

  # NSE = 1 - SSE/SST; mean(obs) = 2.5, SST = sum((obs-2.5)^2) = 5, SSE = sum((obs-pred)^2) = 6
  expect_equal(st$NSE, 1 - 6 / 5)
})

test_that("cal_stats drops NA pairs before computing", {
  obs  <- c(1, NA, 3, 4)
  pred <- c(1, 2, NA, 4)

  st <- cal_stats(obs, pred)

  # only indices 1 and 4 have non-NA values on both sides
  expect_length(st$residual, 2)
  expect_equal(st$RMSE, 0)
})

test_that("cal_stats returns an all-NA list when no valid pairs remain", {
  obs  <- c(NA, NA)
  pred <- c(1, 2)

  st <- cal_stats(obs, pred)

  expect_length(st$residual, 0)
  expect_true(is.na(st$NSE))
  expect_true(is.na(st$RMSE))
  expect_true(is.na(st$NRMSE))
  expect_true(is.na(st$PBIAS))
  expect_true(is.na(st$KGE))
})

test_that("cal_stats NRMSE is NA when observed has zero range", {
  obs  <- c(5, 5, 5)
  pred <- c(4, 5, 6)

  st <- cal_stats(obs, pred)

  expect_true(is.na(st$NRMSE))
})

test_that("cal_stats NSE is NA when observed has zero variance", {
  obs  <- c(5, 5, 5)
  pred <- c(4, 5, 6)

  st <- cal_stats(obs, pred)

  expect_true(is.na(st$NSE))
})
