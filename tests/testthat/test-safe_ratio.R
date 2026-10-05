test_that(".safe_ratio() gives NA instead of Inf/NaN for zero denominators", {
  num <- data.frame(Depth_1 = c(2, 1, 0), Depth_2 = c(4, 3, 1))
  den <- data.frame(Depth_1 = c(1, 0, 0), Depth_2 = c(2, 1, NA))

  r <- LakeEnsemblR.WQ:::.safe_ratio(num, den)
  expect_equal(names(r), c("Depth_1", "Depth_2"))
  expect_equal(r$Depth_1, c(2, NA, NA))
  expect_equal(r$Depth_2, c(2, 3, NA))

  # single depth: plain vectors
  expect_equal(LakeEnsemblR.WQ:::.safe_ratio(c(1, 2), c(0, 4)), c(NA, 0.5))
})
