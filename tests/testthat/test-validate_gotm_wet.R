test_that("validate_gotm_wet() errors when the yaml file itself is missing", {
  dir <- tempfile("lerwq_gotm_")
  dir.create(dir)

  expect_error(validate_gotm_wet(sim_folder = dir, verbose = FALSE),
              regexp = "Missing GOTM yaml file")
})

test_that("validate_gotm_wet() errors when a referenced 'file' field points at a missing file", {
  dir <- tempfile("lerwq_gotm_")
  dir.create(dir)
  writeLines(c(
    "streams:",
    "  inflow_1:",
    "    file: inflow.dat"
  ), file.path(dir, "gotm.yaml"))

  expect_error(validate_gotm_wet(sim_folder = dir, verbose = FALSE),
              regexp = "Missing file\\(s\\) referenced in GOTM yaml")
})

test_that("validate_gotm_wet() succeeds when all referenced files exist", {
  dir <- tempfile("lerwq_gotm_")
  dir.create(dir)
  writeLines("dummy", file.path(dir, "inflow.dat"))
  writeLines(c(
    "streams:",
    "  inflow_1:",
    "    file: inflow.dat"
  ), file.path(dir, "gotm.yaml"))

  expect_true(isTRUE(validate_gotm_wet(sim_folder = dir, verbose = FALSE)))
})

test_that("validate_gotm_wet() also checks location$hypsograph even without a 'file' key", {
  dir <- tempfile("lerwq_gotm_")
  dir.create(dir)
  writeLines(c(
    "location:",
    "  hypsograph: hypso.dat"
  ), file.path(dir, "gotm.yaml"))

  expect_error(validate_gotm_wet(sim_folder = dir, verbose = FALSE),
              regexp = "Missing file\\(s\\) referenced in GOTM yaml")
})

test_that("validate_gotm_wet() succeeds when the yaml has no file references at all", {
  dir <- tempfile("lerwq_gotm_")
  dir.create(dir)
  writeLines("some_setting: true", file.path(dir, "gotm.yaml"))

  expect_true(isTRUE(validate_gotm_wet(sim_folder = dir, verbose = FALSE)))
})
