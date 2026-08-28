# run_sensitivity() does a substantial amount of upfront validation before it
# ever writes a parameter value or runs a model engine (GLM3r/WETr/etc.) --
# everything up to and including building `param_values` happens first. These
# tests exercise exactly that validation chain, without needing any real
# model binary installed. In particular, the "Parameter not found in
# calib_setup dataframe" test reproduces a real failure hit in practice: a
# parameter left at include = FALSE (so absent from calib_setup) silently
# producing a confusing crash several layers downstream, instead of this
# clear, immediate error.

.fake_calib_setup <- function(...) {
  data.frame(
    pars       = character(0),
    lb         = numeric(0),
    ub         = numeric(0),
    x0         = numeric(0),
    file       = character(0),
    group_name = character(0),
    stringsAsFactors = FALSE
  )
}

test_that("run_sensitivity() rejects an unsupported model", {
  cs <- .fake_calib_setup()

  expect_error(
    run_sensitivity(param_name = "kc", calib_setup = cs, yaml_file = "Output.yaml",
                    model_dir = ".", model = "NotARealModel"),
    regexp = "must be one of"
  )
})

test_that("run_sensitivity() requires wq_config_file when output_mode = 'metrics'", {
  cs <- .fake_calib_setup()

  expect_error(
    run_sensitivity(param_name = "kc", calib_setup = cs, yaml_file = "Output.yaml",
                    model_dir = ".", model = "GLM-AED2", output_mode = "metrics"),
    regexp = "wq_config_file"
  )
})

test_that("run_sensitivity() requires vars when output_mode = 'raw'", {
  cs <- .fake_calib_setup()

  expect_error(
    run_sensitivity(param_name = "kc", calib_setup = cs, yaml_file = "Output.yaml",
                    model_dir = ".", model = "GLM-AED2", output_mode = "raw"),
    regexp = "'vars' is required"
  )
})

test_that("run_sensitivity() rejects an invalid output_mode", {
  cs <- .fake_calib_setup()

  expect_error(
    run_sensitivity(param_name = "kc", calib_setup = cs, yaml_file = "Output.yaml",
                    model_dir = ".", model = "GLM-AED2", output_mode = "not_a_mode",
                    vars = "OXY_oxy"),
    regexp = "should be one of"
  )
})

test_that("run_sensitivity() errors clearly when the GOTM yaml file is missing", {
  cs <- .fake_calib_setup()
  dir <- tempfile("lerwq_sens_")
  dir.create(dir)  # no gotm.yaml inside

  expect_error(
    run_sensitivity(param_name = "kc", calib_setup = cs, yaml_file = "Output.yaml",
                    model_dir = dir, model = "GOTM-WET", output_mode = "raw", vars = "sO2W"),
    regexp = "Could not find GOTM yaml file"
  )
})

test_that("run_sensitivity() errors clearly when the Simstrat par file is missing", {
  cs <- .fake_calib_setup()
  dir <- tempfile("lerwq_sens_")
  dir.create(dir)  # no simstrat.par inside

  expect_error(
    run_sensitivity(param_name = "kc", calib_setup = cs, yaml_file = "Output.yaml",
                    model_dir = dir, model = "Simstrat-AED2", output_mode = "raw", vars = "OXY_oxy"),
    regexp = "Could not find Simstrat par file"
  )
})

test_that("run_sensitivity() reports a missing parameter by name (the kc/r0 failure mode)", {
  # calib_setup with rows, but none named 'kc' -- the exact shape of the real
  # failure when a parameter's `include` was never flipped to TRUE upstream.
  cs <- data.frame(
    pars       = c("frp_initial", "r0"),
    lb         = c(0.01, 1.04),
    ub         = c(0.05, 1.56),
    x0         = c(0.03, 1.3),
    file       = c("aed2.nml", "aed2_phyto_pars.nml"),
    group_name = c(NA, "diatoms"),
    stringsAsFactors = FALSE
  )

  expect_error(
    run_sensitivity(param_name = "kc", calib_setup = cs, yaml_file = "Output.yaml",
                    model_dir = ".", model = "GLM-AED2", output_mode = "raw", vars = "OXY_oxy"),
    regexp = "Parameter not found in calib_setup dataframe: kc"
  )
})

test_that("run_sensitivity() reports a group_name that doesn't match any row for the parameter", {
  cs <- data.frame(
    pars       = c("r0", "r0"),
    lb         = c(1.04, 1.04),
    ub         = c(1.56, 1.56),
    x0         = c(1.3, 1.3),
    file       = c("aed2_phyto_pars.nml", "aed2_phyto_pars.nml"),
    group_name = c("diatoms", "cyanobacteria"),
    stringsAsFactors = FALSE
  )

  expect_error(
    run_sensitivity(param_name = "r0", calib_setup = cs, yaml_file = "Output.yaml",
                    model_dir = ".", model = "GLM-AED2", output_mode = "raw", vars = "OXY_oxy",
                    group_name = "not_a_real_group"),
    regexp = "No matching entry for param 'r0' with group_name 'not_a_real_group'"
  )
})
