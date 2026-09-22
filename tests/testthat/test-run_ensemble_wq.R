test_that("run_ensemble_wq() validates on_error before touching any files", {
  # match.arg() is the first thing run_ensemble_wq() does, so this is
  # reachable with a config_file that doesn't exist.
  expect_error(
    run_ensemble_wq(config_file = "does-not-exist.yaml", on_error = "not-a-choice"),
    "'arg' should be one of"
  )
})
