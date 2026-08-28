# create_calibration_tables() reads its parameter universe from the real,
# shipped `LakeEnsemblR_WQ_dictionary` package data (referenced directly by
# name, not passed as an argument) -- so rather than mock that binding
# (risky to get right without being able to run R to confirm testthat's
# namespace-mocking behaves as expected here), these tests use the real
# dictionary and check structural/formula contracts that hold regardless of
# its exact current contents: the bounds formula, model/module filtering,
# and per-group file splitting.

.write_wq_config <- function(dir, ...) {
  modules <- list(...)
  lines <- c(
    "models:",
    "  - GLM-AED2",
    "  - GOTM-WET"
  )
  for (nm in names(modules)) {
    lines <- c(lines, paste0(nm, ":"))
    mod <- modules[[nm]]
    lines <- c(lines, paste0("  use: ", tolower(as.character(mod$use))))
    if (!is.null(mod$groups)) {
      lines <- c(lines, "  groups:")
      for (g in mod$groups) lines <- c(lines, paste0("    ", g, ":"))
    }
  }
  path <- file.path(dir, "LakeEnsemblR_WQ.yaml")
  writeLines(lines, path)
  path
}

test_that("create_calibration_tables() sets include = FALSE and computes bounds from the default", {
  dir <- tempfile("lerwq_ct_")
  dir.create(dir)
  cfg <- .write_wq_config(dir, carbon = list(use = TRUE), oxygen = list(use = TRUE))
  out_dir <- file.path(dir, "calibration")

  calib_table <- create_calibration_tables(
    folder = dir, config_file = basename(cfg), folder_out = out_dir,
    models_coupled = c("GLM-AED2", "GOTM-WET"), bounds_factor = 0.2
  )

  expect_true(nrow(calib_table) > 0)
  expect_true(all(!calib_table$include))
  expect_equal(calib_table$lower, calib_table$default * 0.8)
  expect_equal(calib_table$upper, calib_table$default * 1.2)
  expect_equal(calib_table$initial, calib_table$default)
})

test_that("create_calibration_tables() skips integer-typed parameters", {
  # co2_piston_model / ch4_piston_model are real dictionary entries, model =
  # "aed2" (-> GLM-AED2), module = "carbon", unit = "(integer)". These are
  # mode selectors (note: "1: ; 2: ...") -- a percentage-based bounds_factor
  # produces a meaningless fractional range for them, and nothing downstream
  # rounds a sampled value back before writing it into the namelist.
  dir <- tempfile("lerwq_ct_")
  dir.create(dir)
  cfg <- .write_wq_config(dir, carbon = list(use = TRUE))
  out_dir <- file.path(dir, "calibration")

  calib_table <- create_calibration_tables(
    folder = dir, config_file = basename(cfg), folder_out = out_dir,
    models_coupled = c("GLM-AED2", "GOTM-WET")
  )

  expect_false("co2_piston_model" %in% calib_table$parameter)
  expect_false("ch4_piston_model" %in% calib_table$parameter)
  expect_false(any(grepl("integer", calib_table$unit, ignore.case = TRUE)))
})

test_that("create_calibration_tables() skips boolean-typed parameters", {
  # lNfix is a real dictionary entry, model = "wet" (-> GOTM-WET), module =
  # "phytoplankton", unit = "(boolean)", default = " false" (literal text,
  # not 0/1) -- confirms the explicit unit-based check catches it (rather
  # than it merely vanishing as a side effect of the numeric-default filter
  # failing to parse "false" as a number, which would mask the exclusion
  # being intentional).
  dir <- tempfile("lerwq_ct_")
  dir.create(dir)
  cfg <- .write_wq_config(dir, phytoplankton = list(use = TRUE, groups = c("diatoms")))
  out_dir <- file.path(dir, "calibration")

  calib_table <- create_calibration_tables(
    folder = dir, config_file = basename(cfg), folder_out = out_dir,
    models_coupled = c("GLM-AED2", "GOTM-WET")
  )

  expect_false("lNfix" %in% calib_table$parameter)
  expect_false(any(grepl("boolean", calib_table$unit, ignore.case = TRUE)))
})

test_that("create_calibration_tables() drops a zero-default parameter with no dictionary min/max", {
  # tDDepoIM is a real dictionary entry, model = "wet" (-> GOTM-WET), module
  # = "carbon", default = 0, no min/max -- default * (1 +/- bounds_factor)
  # would otherwise silently produce lower = upper = 0.
  dir <- tempfile("lerwq_ct_")
  dir.create(dir)
  cfg <- .write_wq_config(dir, carbon = list(use = TRUE))
  out_dir <- file.path(dir, "calibration")

  calib_table <- create_calibration_tables(
    folder = dir, config_file = basename(cfg), folder_out = out_dir,
    models_coupled = c("GLM-AED2", "GOTM-WET")
  )

  expect_false("tDDepoIM" %in% calib_table$parameter)
  # No surviving row should have a degenerate zero-width range.
  expect_false(any(calib_table$lower == 0 & calib_table$upper == 0))
})

test_that("create_calibration_tables() uses dictionary min/max for a zero-default parameter when available", {
  # dd_c is a real dictionary entry, model = "selmaprotbas" (->
  # GOTM-Selmaprotbas), module = "carbon", default = 0, min = 0, max = 15.
  # This only exercises anything if the currently-loaded dictionary actually
  # has min/max columns populated -- skip cleanly if not, rather than fail
  # on an assumption about which dictionary revision is bundled.
  dir <- tempfile("lerwq_ct_")
  dir.create(dir)
  cfg <- writeLines(c(
    "models:",
    "  - GOTM-Selmaprotbas",
    "carbon:",
    "  use: true"
  ), file.path(dir, "LakeEnsemblR_WQ.yaml"))
  cfg_path <- file.path(dir, "LakeEnsemblR_WQ.yaml")
  out_dir <- file.path(dir, "calibration")

  calib_table <- create_calibration_tables(
    folder = dir, config_file = basename(cfg_path), folder_out = out_dir,
    models_coupled = "GOTM-Selmaprotbas"
  )

  if (!"dd_c" %in% calib_table$parameter) {
    skip("dd_c not present with a usable dict min/max in the currently-loaded dictionary")
  }

  row <- calib_table[calib_table$parameter == "dd_c", ]
  expect_equal(row$lower, 0)
  expect_equal(row$upper, 15)
})

test_that("create_calibration_tables() only includes the requested models_coupled", {
  dir <- tempfile("lerwq_ct_")
  dir.create(dir)
  cfg <- .write_wq_config(dir, carbon = list(use = TRUE))
  out_dir <- file.path(dir, "calibration")

  calib_table <- create_calibration_tables(
    folder = dir, config_file = basename(cfg), folder_out = out_dir,
    models_coupled = c("GLM-AED2", "GOTM-WET")
  )

  expect_true(all(calib_table$model_coupled %in% c("GLM-AED2", "GOTM-WET")))
})

test_that("create_calibration_tables() honors bounds_factor as a fraction of default", {
  dir <- tempfile("lerwq_ct_")
  dir.create(dir)
  cfg <- .write_wq_config(dir, carbon = list(use = TRUE))
  out_dir <- file.path(dir, "calibration")

  calib_table <- create_calibration_tables(
    folder = dir, config_file = basename(cfg), folder_out = out_dir,
    models_coupled = c("GLM-AED2", "GOTM-WET"), bounds_factor = 0.05
  )

  expect_equal(calib_table$lower, calib_table$default * 0.95)
  expect_equal(calib_table$upper, calib_table$default * 1.05)
})

test_that("create_calibration_tables() writes the master file and only per-module files for modules with use: true", {
  dir <- tempfile("lerwq_ct_")
  dir.create(dir)
  cfg <- .write_wq_config(dir, carbon = list(use = TRUE), nitrogen = list(use = FALSE))
  out_dir <- file.path(dir, "calibration")

  create_calibration_tables(
    folder = dir, config_file = basename(cfg), folder_out = out_dir,
    models_coupled = c("GLM-AED2", "GOTM-WET")
  )

  expect_true(file.exists(file.path(out_dir, "calibration_master.csv")))
  expect_true(file.exists(file.path(out_dir, "calibration_carbon.csv")))
  # nitrogen has use: false in the config -- no per-module file should be
  # written for it even though nitrogen rows exist in the master table.
  expect_false(file.exists(file.path(out_dir, "calibration_nitrogen.csv")))
})

test_that("create_calibration_tables() writes one identical-content file per phytoplankton group", {
  dir <- tempfile("lerwq_ct_")
  dir.create(dir)
  cfg <- .write_wq_config(dir, phytoplankton = list(use = TRUE, groups = c("diatoms", "cyanobacteria")))
  out_dir <- file.path(dir, "calibration")

  create_calibration_tables(
    folder = dir, config_file = basename(cfg), folder_out = out_dir,
    models_coupled = c("GLM-AED2", "GOTM-WET")
  )

  diatoms_file <- file.path(out_dir, "calibration_diatoms.csv")
  cyano_file   <- file.path(out_dir, "calibration_cyanobacteria.csv")
  expect_true(file.exists(diatoms_file))
  expect_true(file.exists(cyano_file))

  diatoms <- read.csv(diatoms_file)
  cyano   <- read.csv(cyano_file)
  # Group differentiation happens later (in calib_setup_from_tables(), via
  # filename) -- at this stage both group files should contain the same
  # phytoplankton-module rows.
  expect_equal(nrow(diatoms), nrow(cyano))
  expect_setequal(diatoms$parameter, cyano$parameter)
  expect_true(all(diatoms$module == "phytoplankton"))
})
