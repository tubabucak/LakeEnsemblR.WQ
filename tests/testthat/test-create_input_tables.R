.write_cit_config <- function(dir) {
  lines <- c("models:", "  - GLM-AED2", "  - GOTM-WET",
            "carbon:", "  use: true", "oxygen:", "  use: true")
  writeLines(lines, file.path(dir, "LakeEnsemblR_WQ.yaml"))
}

test_that("create_input_tables() warns that it is deprecated", {
  dir <- tempfile("lerwq_cit_")
  dir.create(dir)
  .write_cit_config(dir)

  expect_warning(
    create_input_tables(folder = dir, config_file = "LakeEnsemblR_WQ.yaml",
                        folder_out = dir, input = NULL,
                        models_coupled = "GLM-AED2"),
    "deprecated"
  )
})

test_that("create_input_tables() rejects an over-long 'input' path", {
  dir <- tempfile("lerwq_cit_")
  dir.create(dir)
  .write_cit_config(dir)

  expect_error(
    suppressWarnings(create_input_tables(
      folder = dir, config_file = "LakeEnsemblR_WQ.yaml", folder_out = dir,
      input = "a/b/c/d/e/f/g", models_coupled = "GLM-AED2"
    )),
    "longer than six levels"
  )
})
