# calib_setup_from_tables() reads back the calibration_<module>.csv files
# produced by create_calibration_tables() (after a user has hand-edited
# `include`), filters to include == TRUE, and builds the calib_setup table
# run_lhc_wq()/run_sensitivity() expect. This is the exact mechanism behind
# two real confusions hit in practice: a parameter silently missing because
# its `include` was never flipped to TRUE, and group_name inference for
# per-group modules (phytoplankton/zooplankton/etc.).

# Helper: write one calibration_<name>.csv with the real column set
# create_calibration_tables() produces, minus values the caller overrides.
.write_calib_csv <- function(dir, name, rows) {
  base_cols <- data.frame(
    include       = FALSE,
    module        = NA_character_,
    domain        = "water",
    process       = "growth",
    subprocess    = "growth_rate",
    model_coupled = NA_character_,
    parameter     = NA_character_,
    default       = 1,
    dict_min      = 0.5,
    dict_max      = 1.5,
    lower         = 0.5,
    upper         = 1.5,
    initial       = 1,
    log           = FALSE,
    unit          = "d-1",
    path          = "params/x",
    note          = "",
    stringsAsFactors = FALSE
  )
  df <- do.call(rbind, lapply(seq_len(nrow(rows)), function(i) {
    row <- base_cols
    for (col in names(rows)) row[[col]] <- rows[[col]][i]
    row
  }))
  write.csv(df, file.path(dir, paste0("calibration_", name, ".csv")), row.names = FALSE)
}

test_that("calib_setup_from_tables() includes only include == TRUE rows for the requested model", {
  dir <- tempfile("lerwq_calib_")
  dir.create(dir)

  .write_calib_csv(dir, "carbon", data.frame(
    include       = c(TRUE, FALSE),
    module        = "carbon",
    model_coupled = "GLM-AED2",
    parameter     = c("Rgrowth", "Kc")
  ))

  cs <- calib_setup_from_tables(folder_in = dir, model_coupled = "GLM-AED2")

  expect_equal(nrow(cs), 1)
  expect_equal(cs$pars, "Rgrowth")
})

test_that("calib_setup_from_tables() filters by model_coupled", {
  dir <- tempfile("lerwq_calib_")
  dir.create(dir)

  .write_calib_csv(dir, "carbon", data.frame(
    include       = TRUE,
    module        = "carbon",
    model_coupled = c("GLM-AED2", "GOTM-Selmaprotbas"),
    parameter     = c("Rgrowth", "kc")
  ))

  cs <- calib_setup_from_tables(folder_in = dir, model_coupled = "GOTM-Selmaprotbas")

  expect_equal(nrow(cs), 1)
  expect_equal(cs$pars, "kc")
  expect_equal(cs$model_coupled, "GOTM-Selmaprotbas")
})

test_that("calib_setup_from_tables() errors with a clear message when nothing is include = TRUE", {
  # This is the exact shape of the empty-cs_glm / LHC "n=4 k=0" bug: every row
  # for the requested model exists, but none are flagged include = TRUE.
  dir <- tempfile("lerwq_calib_")
  dir.create(dir)

  .write_calib_csv(dir, "carbon", data.frame(
    include       = FALSE,
    module        = "carbon",
    model_coupled = "GLM-AED2",
    parameter     = "Rgrowth"
  ))

  expect_error(
    calib_setup_from_tables(folder_in = dir, model_coupled = "GLM-AED2"),
    regexp = "include = TRUE"
  )
})

test_that("calib_setup_from_tables() errors when no calibration_<module>.csv files exist", {
  dir <- tempfile("lerwq_calib_")
  dir.create(dir)

  expect_error(
    calib_setup_from_tables(folder_in = dir, model_coupled = "GLM-AED2"),
    regexp = "create_calibration_tables"
  )
})

test_that("calib_setup_from_tables() ignores calibration_master.csv", {
  dir <- tempfile("lerwq_calib_")
  dir.create(dir)

  # A master file with a real row that would otherwise satisfy the request --
  # must NOT be picked up, only calibration_<module>.csv files should be.
  .write_calib_csv(dir, "master", data.frame(
    include       = TRUE,
    module        = "carbon",
    model_coupled = "GLM-AED2",
    parameter     = "from_master"
  ))

  expect_error(
    calib_setup_from_tables(folder_in = dir, model_coupled = "GLM-AED2"),
    regexp = "create_calibration_tables"
  )
})

test_that("calib_setup_from_tables() infers group_name from filename for group-aware modules", {
  dir <- tempfile("lerwq_calib_")
  dir.create(dir)

  .write_calib_csv(dir, "diatoms", data.frame(
    include       = TRUE,
    module        = "phytoplankton",
    model_coupled = "GOTM-Selmaprotbas",
    parameter     = "r0"
  ))
  .write_calib_csv(dir, "cyanobacteria", data.frame(
    include       = TRUE,
    module        = "phytoplankton",
    model_coupled = "GOTM-Selmaprotbas",
    parameter     = "r0"
  ))

  cs <- calib_setup_from_tables(folder_in = dir, model_coupled = "GOTM-Selmaprotbas")

  # Same `pars` value ("r0") for both rows -- only group_name disambiguates them.
  expect_equal(nrow(cs), 2)
  expect_setequal(cs$group_name, c("diatoms", "cyanobacteria"))
})

test_that("calib_setup_from_tables() leaves group_name NA for non-group modules", {
  dir <- tempfile("lerwq_calib_")
  dir.create(dir)

  .write_calib_csv(dir, "carbon", data.frame(
    include       = TRUE,
    module        = "carbon",
    model_coupled = "GLM-AED2",
    parameter     = "Rgrowth"
  ))

  cs <- calib_setup_from_tables(folder_in = dir, model_coupled = "GLM-AED2")

  expect_true(is.na(cs$group_name))
})

test_that("calib_setup_from_tables() an explicit group_name argument overrides inference", {
  dir <- tempfile("lerwq_calib_")
  dir.create(dir)

  .write_calib_csv(dir, "diatoms", data.frame(
    include       = TRUE,
    module        = "phytoplankton",
    model_coupled = "GOTM-Selmaprotbas",
    parameter     = "r0"
  ))

  cs <- calib_setup_from_tables(folder_in = dir, model_coupled = "GOTM-Selmaprotbas",
                                group_name = "some_other_group")

  expect_equal(cs$group_name, "some_other_group")
})

test_that("calib_setup_from_tables() warns when lower >= upper", {
  dir <- tempfile("lerwq_calib_")
  dir.create(dir)

  .write_calib_csv(dir, "carbon", data.frame(
    include       = TRUE,
    module        = "carbon",
    model_coupled = "GLM-AED2",
    parameter     = "Rgrowth",
    lower         = 2,
    upper         = 1
  ))

  expect_warning(
    calib_setup_from_tables(folder_in = dir, model_coupled = "GLM-AED2"),
    regexp = "lower >= upper"
  )
})

test_that("calib_setup_from_tables() warns (not errors) when only some requested models have rows", {
  dir <- tempfile("lerwq_calib_")
  dir.create(dir)

  .write_calib_csv(dir, "carbon", data.frame(
    include       = TRUE,
    module        = "carbon",
    model_coupled = "GLM-AED2",
    parameter     = "Rgrowth"
  ))

  expect_warning(
    cs <- calib_setup_from_tables(folder_in = dir,
                                  model_coupled = c("GLM-AED2", "GOTM-Selmaprotbas")),
    regexp = "GOTM-Selmaprotbas"
  )
  expect_equal(nrow(cs), 1)
})
