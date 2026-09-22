test_that("set_coupling() warns and skips a model with a missing config_files entry", {
  dir <- tempfile("lerwq_setcoupling_")
  dir.create(dir)
  # config_files has no entry at all for the one coupled model.
  yaml::write_yaml(
    list(models = list("GLM-AED2"), config_files = list()),
    file.path(dir, "LakeEnsemblR_WQ.yaml")
  )

  expect_warning(
    set_coupling(config_file = "LakeEnsemblR_WQ.yaml", folder = dir),
    "Skipping coupling for model 'GLM-AED2'.*missing or NA"
  )
})
