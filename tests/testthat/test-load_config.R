# Helper: build a minimal fake project directory with the files load_config()
# requires, plus one or two "model folders" -- returns the temp dir path.
# Kept self-contained (no dependency on any real project's data) so this test
# runs anywhere, including CI.
.make_fake_project <- function(models_exist = c("WET"), models_declared = c("WET", "GLM")) {
  root <- tempfile("lerwq_test_")
  dir.create(root)

  # Required support files -- content doesn't matter, only existence.
  writeLines("dummy bathymetry", file.path(root, "bathy.csv"))
  writeLines("dummy ler config", file.path(root, "LakeEnsemblR.yaml"))

  for (m in models_exist) {
    dir.create(file.path(root, m), recursive = TRUE)
  }

  model_folders_yaml <- paste(
    vapply(models_declared, function(m) paste0("  ", m, ": \"", m, "\""), character(1)),
    collapse = "\n"
  )

  config_path <- file.path(root, "Output.yaml")
  writeLines(c(
    "folder:",
    "files:",
    "  bathy_file: \"bathy.csv\"",
    "  metric_yaml_file: \"Output.yaml\"",
    "  LER_config_file: \"LakeEnsemblR.yaml\"",
    "model_folders:",
    model_folders_yaml
  ), config_path)

  config_path
}

test_that("load_config() succeeds when all files/model folders exist", {
  config_path <- .make_fake_project(models_exist = c("WET", "GLM"), models_declared = c("WET", "GLM"))

  cfg <- load_config(config_path)

  expect_true(dir.exists(cfg$folder))
  expect_true(file.exists(cfg$bathy_file))
  expect_true(file.exists(cfg$LER_config_file))
  # model_folders names are normalized to uppercase
  expect_setequal(names(cfg$model_folders), c("WET", "GLM"))
})

test_that("load_config() defaults 'folder' to the config file's own directory when blank", {
  config_path <- .make_fake_project(models_exist = c("WET"), models_declared = c("WET"))

  cfg <- load_config(config_path, required_models = "WET")

  expect_equal(normalizePath(cfg$folder), normalizePath(dirname(config_path)))
})

test_that("load_config() errors on a missing bathy_file before checking anything else", {
  config_path <- .make_fake_project(models_exist = c("WET"), models_declared = c("WET"))
  # Break the bathymetry file specifically.
  unlink(file.path(dirname(config_path), "bathy.csv"))

  expect_error(load_config(config_path), regexp = "File not found.*bathy")
})

test_that("load_config() requires every model_folders entry by default", {
  # WET exists on disk, GLM does not -- required_models defaults to NULL (all).
  config_path <- .make_fake_project(models_exist = c("WET"), models_declared = c("WET", "GLM"))

  expect_error(load_config(config_path), regexp = "GLM")
})

test_that("load_config() with required_models only validates the named model(s)", {
  # Same broken setup as above, but narrowed to just the model that DOES exist.
  config_path <- .make_fake_project(models_exist = c("WET"), models_declared = c("WET", "GLM"))

  cfg <- load_config(config_path, required_models = "WET")

  # Both entries are still resolved/returned -- only existence-checking is narrowed.
  expect_setequal(names(cfg$model_folders), c("WET", "GLM"))
})

test_that("load_config() required_models matching is case-insensitive", {
  config_path <- .make_fake_project(models_exist = c("WET"), models_declared = c("WET", "GLM"))

  expect_no_error(load_config(config_path, required_models = "wet"))
})
