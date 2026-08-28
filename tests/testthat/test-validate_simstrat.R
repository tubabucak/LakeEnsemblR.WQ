# validate_simstrat() parses simstrat.par as JSON (via a small quoting
# transform + jsonlite::fromJSON) -- fixtures here are written as fully
# valid, already-quoted JSON so they parse correctly regardless of that
# transform's exact regex behavior.

.write_par <- function(dir, input, simulation, output = NULL, model_config = NULL, aed2_config = NULL) {
  parts <- c(
    sprintf('"Input": %s', input),
    sprintf('"Simulation": %s', simulation)
  )
  if (!is.null(output)) parts <- c(parts, sprintf('"Output": %s', output))
  if (!is.null(model_config)) parts <- c(parts, sprintf('"ModelConfig": %s', model_config))
  if (!is.null(aed2_config)) parts <- c(parts, sprintf('"AED2Config": %s', aed2_config))

  writeLines(paste0("{\n", paste(parts, collapse = ",\n"), "\n}"),
            file.path(dir, "simstrat.par"))
}

test_that("validate_simstrat() errors when the par file itself is missing", {
  dir <- tempfile("lerwq_sim_")
  dir.create(dir)

  expect_error(validate_simstrat(sim_folder = dir, verbose = FALSE),
              regexp = "Missing Simstrat parameter file")
})

test_that("validate_simstrat() errors when the 'Input' section is missing", {
  dir <- tempfile("lerwq_sim_")
  dir.create(dir)
  writeLines('{"Simulation": {"Start d": 0, "End d": 10}}', file.path(dir, "simstrat.par"))

  expect_error(validate_simstrat(sim_folder = dir, verbose = FALSE),
              regexp = "Missing 'Input' section")
})

test_that("validate_simstrat() errors when a referenced input file is missing", {
  dir <- tempfile("lerwq_sim_")
  dir.create(dir)
  .write_par(dir,
    input = '{"Forcing": "forcing.dat"}',
    simulation = '{"Start d": 0, "End d": 10}'
  )

  expect_error(validate_simstrat(sim_folder = dir, verbose = FALSE, check_time_coverage = FALSE),
              regexp = "Missing input file \\(Forcing\\)")
})

test_that("validate_simstrat() errors when CoupleAED2 = TRUE but the AED2 config file is missing", {
  dir <- tempfile("lerwq_sim_")
  dir.create(dir)
  writeLines("dummy", file.path(dir, "forcing.dat"))
  .write_par(dir,
    input = '{"Forcing": "forcing.dat"}',
    simulation = '{"Start d": 0, "End d": 10}',
    model_config = '{"CoupleAED2": true}',
    aed2_config = '{"AED2ConfigFile": "aed2.nml"}'
  )

  expect_error(validate_simstrat(sim_folder = dir, verbose = FALSE, check_time_coverage = FALSE),
              regexp = "Missing AED2 config file")
})

test_that("validate_simstrat() errors when an input file's time series doesn't cover the simulation end", {
  dir <- tempfile("lerwq_sim_")
  dir.create(dir)
  writeLines(c("Time [d]\tQ_in [m3/s]", "0\t1.0", "5\t1.0"), file.path(dir, "inflow.dat"))
  .write_par(dir,
    input = '{"Inflow": "inflow.dat"}',
    simulation = '{"Start d": 0, "End d": 10}'
  )

  expect_error(validate_simstrat(sim_folder = dir, verbose = FALSE, check_time_coverage = TRUE),
              regexp = "does not cover simulation end")
})

test_that("validate_simstrat() skips the time-coverage check when check_time_coverage = FALSE", {
  dir <- tempfile("lerwq_sim_")
  dir.create(dir)
  writeLines(c("Time [d]\tQ_in [m3/s]", "0\t1.0", "5\t1.0"), file.path(dir, "inflow.dat"))
  .write_par(dir,
    input = '{"Inflow": "inflow.dat"}',
    simulation = '{"Start d": 0, "End d": 10}'
  )

  expect_true(isTRUE(validate_simstrat(sim_folder = dir, verbose = FALSE, check_time_coverage = FALSE)))
})

test_that("validate_simstrat() succeeds when everything is present and consistent", {
  dir <- tempfile("lerwq_sim_")
  dir.create(dir)
  writeLines("dummy", file.path(dir, "forcing.dat"))
  .write_par(dir,
    input = '{"Forcing": "forcing.dat"}',
    simulation = '{"Start d": 0, "End d": 10}',
    output = '{"Path": "output"}'
  )

  expect_true(isTRUE(validate_simstrat(sim_folder = dir, verbose = FALSE, check_time_coverage = FALSE)))
  expect_true(dir.exists(file.path(dir, "output")))
})
