# These tests exercise cali_ensemble_wq()'s argument-resolution and
# model-name canonicalization logic via paths that never reach run_lhc_wq()
# (empty calib_setup => "no calib_setup rows for model" short-circuit), so no
# real model directories or binaries are required.

test_that("cali_ensemble_wq rejects unsupported model names", {
  cs <- data.frame(pars = character(0), model_coupled = character(0), stringsAsFactors = FALSE)

  expect_error(
    cali_ensemble_wq(
      models = "NotAModel",
      calib_setup = cs,
      yaml_file = "Output.yaml",
      verbose = FALSE
    ),
    "Unsupported model"
  )
})

test_that("cali_ensemble_wq canonicalizes model name variants and skips models with no calib_setup rows", {
  cs <- data.frame(pars = character(0), model_coupled = character(0), stringsAsFactors = FALSE)

  result <- cali_ensemble_wq(
    models = c("glm_aed2", "gotm-wet", "GOTM_SELMA"),
    calib_setup = cs,
    yaml_file = "Output.yaml",
    on_error = "skip",
    verbose = FALSE
  )

  expect_s3_class(result, "cali_ensemble_wq_result")
  expect_equal(result$summary$model, c("GLM-AED2", "GOTM-WET", "GOTM-Selmaprotbas"))
  expect_false(any(result$summary$success))
  expect_true(all(grepl("No calib_setup rows", result$summary$message)))
  expect_true(all(vapply(result$results, is.null, logical(1))))
})

test_that("cali_ensemble_wq stops immediately on the first failure when on_error = 'stop'", {
  cs <- data.frame(pars = character(0), model_coupled = character(0), stringsAsFactors = FALSE)

  expect_error(
    cali_ensemble_wq(
      models = "GLM-AED2",
      calib_setup = cs,
      yaml_file = "Output.yaml",
      on_error = "stop",
      verbose = FALSE
    ),
    "No calib_setup rows"
  )
})

test_that("cali_ensemble_wq splits a combined calib_setup by model_coupled", {
  cs <- data.frame(
    pars          = c("p1", "p2"),
    model_coupled = c("GLM-AED2", "GOTM-WET"),
    lb            = c(0, 0),
    ub            = c(1, 1),
    file          = c("a.nml", "b.yaml"),
    stringsAsFactors = FALSE
  )

  # Both models will still fail (no real model_dir/binary to run), but the
  # per-model split itself -- one param each, not zero -- is what's under
  # test here: it should reach run_lhc_wq() (and fail *there*, e.g. on a
  # missing model_dir), not bail out early with "No parameter names".
  result <- cali_ensemble_wq(
    models = c("GLM-AED2", "GOTM-WET"),
    calib_setup = cs,
    yaml_file = "Output.yaml",
    folder = tempdir(),
    on_error = "skip",
    verbose = FALSE
  )

  expect_false(any(grepl("No calib_setup rows|No parameter names", result$summary$message)))
})
