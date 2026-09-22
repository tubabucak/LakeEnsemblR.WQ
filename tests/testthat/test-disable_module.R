test_that("disable_module() is currently a no-op stub", {
  # disable_module() is exported and documented ("Make sure that a certain
  # module is not used in all models") but its body is just
  # `invisible(NULL)` -- it doesn't read config_file/folder or touch
  # anything. This test documents the current (very likely unintended)
  # behavior so a real implementation shows up here as an intentional
  # test change rather than a silent behavior shift.
  expect_null(disable_module(config_file = "does-not-exist.yaml", folder = ".",
                             module = "oxygen"))
})
