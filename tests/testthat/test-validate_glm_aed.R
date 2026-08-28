.write_glm_nml <- function(dir, extra_lines = character(0)) {
  nml <- file.path(dir, "glm3.nml")
  writeLines(c(
    "&glm_setup",
    "   sim_name = 'test'",
    "/",
    "&file_io",
    extra_lines,
    "/"
  ), nml)
  nml
}

test_that("validate_glm_aed() errors when the nml file itself is missing", {
  dir <- tempfile("lerwq_glm_")
  dir.create(dir)

  expect_error(validate_glm_aed(sim_folder = dir, verbose = FALSE),
              regexp = "Missing nml file")
})

test_that("validate_glm_aed() errors when a referenced meteo file is missing", {
  dir <- tempfile("lerwq_glm_")
  dir.create(dir)
  .write_glm_nml(dir, "   meteo_fl = 'meteo.csv'")

  expect_error(validate_glm_aed(sim_folder = dir, verbose = FALSE),
              regexp = "Missing file\\(s\\) referenced in nml for meteo_fl")
})

test_that("validate_glm_aed() succeeds when all referenced files exist", {
  dir <- tempfile("lerwq_glm_")
  dir.create(dir)
  writeLines("dummy", file.path(dir, "meteo.csv"))
  writeLines("dummy", file.path(dir, "inflow.csv"))
  .write_glm_nml(dir, c(
    "   meteo_fl = 'meteo.csv'",
    "   inflow_fl = 'inflow.csv'"
  ))

  expect_true(isTRUE(validate_glm_aed(sim_folder = dir, verbose = FALSE)))
})

test_that("validate_glm_aed() creates a missing output directory rather than failing", {
  dir <- tempfile("lerwq_glm_")
  dir.create(dir)
  .write_glm_nml(dir, "   out_dir = 'output'")

  expect_true(isTRUE(validate_glm_aed(sim_folder = dir, verbose = FALSE)))
  expect_true(dir.exists(file.path(dir, "output")))
})

test_that("validate_glm_aed() succeeds when nml references no files at all", {
  dir <- tempfile("lerwq_glm_")
  dir.create(dir)
  .write_glm_nml(dir)

  expect_true(isTRUE(validate_glm_aed(sim_folder = dir, verbose = FALSE)))
})
