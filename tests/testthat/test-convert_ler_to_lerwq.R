.write_convert_configs <- function(dir) {
  ler_cfg <- list(config_files = list(GLM = "GLM/glm3.nml"))
  wq_cfg <- list(
    models = list("GLM-AED2"),
    config_files = list(`GLM-AED2` = "GLM-AED2/glm3.nml"),
    run_settings = list(`bio-shading` = FALSE, bottom_everywhere = FALSE,
                        repair_state = FALSE, split_factor = 1,
                        ode_method = "Euler")
  )
  yaml::write_yaml(ler_cfg, file.path(dir, "LakeEnsemblR.yaml"))
  yaml::write_yaml(wq_cfg, file.path(dir, "LakeEnsemblR_WQ.yaml"))
}

test_that("convert_ler_to_lerwq() errors clearly when the LakeEnsemblR model folder is missing", {
  dir <- tempfile("lerwq_convert_")
  dir.create(dir)
  .write_convert_configs(dir)
  # Deliberately not creating dir/GLM -- LakeEnsemblR::export_config's output
  # folder is assumed to already exist.

  expect_error(
    convert_ler_to_lerwq(folder = dir),
    "ensure that LakeEnsemblR::export_config has been run beforehand"
  )
})
