test_that(".convert_aed2_nml_to_aed3() renames sections, models and Ranammox only", {
  nml <- tempfile(fileext = ".nml")
  writeLines(c(
    "&aed2_models",
    "   models = 'aed2_oxygen', 'aed2_nitrogen', 'aed2_phytoplankton'",
    "/",
    "&aed2_nitrogen",
    "   Ranammox = 0.001",
    "   Rnitrif = 0.1",
    "/",
    "&aed2_phytoplankton",
    "   dbase = 'aed2_phyto_pars.nml'",
    "/"
  ), nml)

  LakeEnsemblR.WQ:::.convert_aed2_nml_to_aed3(nml)
  out <- readLines(nml)

  expect_equal(out[1], "&aed_models")
  expect_equal(out[2], "   models = 'aed_oxygen', 'aed_nitrogen', 'aed_phytoplankton'")
  expect_equal(out[4], "&aed_nitrogen")
  expect_equal(out[5], "   kanammox = 0.001")
  expect_equal(out[9], "   dbase = 'aed2_phyto_pars.nml'")

  # Idempotent
  LakeEnsemblR.WQ:::.convert_aed2_nml_to_aed3(nml)
  expect_identical(readLines(nml), out)
})

test_that(".find_nml_section() resolves AED2 dictionary paths in AED 3 files", {
  lines <- c("&aed_oxygen", "   Fsed_oxy = -10", "/", "&aed_nitrogen", "   kanammox = 0.001", "/")

  sec <- LakeEnsemblR.WQ:::.find_nml_section(lines, "aed2_nitrogen")
  expect_equal(c(sec$start, sec$end), c(4L, 6L))
  expect_true(sec$aed3)
  expect_equal(LakeEnsemblR.WQ:::.aed3_name("Ranammox"), "kanammox")

  sec <- LakeEnsemblR.WQ:::.find_nml_section(lines, "aed_oxygen")
  expect_false(sec$aed3)
  expect_null(LakeEnsemblR.WQ:::.find_nml_section(lines, "aed2_silica"))
})

test_that("expand_templates() expands GLM 4 zooplankton names by group", {
  cfg <- tempfile(fileext = ".yaml")
  yaml::write_yaml(list(zooplankton = list(groups = list(daphnia = list(), copepods = list()))), cfg)
  row <- data.frame(variable_model_name = "ZOO_{zoo_group}", model = "GLM")

  out <- LakeEnsemblR.WQ:::expand_templates(row, cfg)
  expect_equal(out$variable_model_name, c("ZOO_daphnia", "ZOO_copepods"))
})

test_that(".write_aed2_dict_param() finds GLM's AED file from wq_nml_file without a wq_config_file", {
  dir <- tempfile("lerwq_glmaed_")
  dir.create(dir)
  writeLines(c("&wq_setup", "   wq_lib = 'api'", "   wq_nml_file = 'aed.nml'  ! AED 3", "/"),
             file.path(dir, "glm3.nml"))
  writeLines(c("&aed_oxygen", "   Fsed_oxy = -10", "/"), file.path(dir, "aed.nml"))

  ok <- LakeEnsemblR.WQ:::.write_aed2_dict_param("aed2_oxygen/Fsed_oxy", "Fsed_oxy", -300,
                                                 current_dir = dir, model = "GLM-AED")
  expect_true(ok)
  expect_equal(readLines(file.path(dir, "aed.nml"))[2], "   Fsed_oxy = -300")
})
