test_that("export_config_wq() errors clearly on a missing config_file", {
  expect_error(
    export_config_wq(config_file = "does-not-exist.yaml", folder = tempdir(),
                     convert_from_lakeensemblr = FALSE),
    "config_file not found"
  )
})

test_that("export_config_wq() errors clearly on a missing ler_config_file", {
  dir <- tempfile("lerwq_export_")
  dir.create(dir)
  yaml::write_yaml(list(models = list()), file.path(dir, "LakeEnsemblR_WQ.yaml"))

  expect_error(
    export_config_wq(config_file = "LakeEnsemblR_WQ.yaml", folder = dir,
                     convert_from_lakeensemblr = TRUE,
                     ler_config_file = "does-not-exist.yaml"),
    "ler_config_file not found"
  )
})
