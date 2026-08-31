# get_output_wq()'s Simstrat branch reads plain-text `<var>_out.dat` CSV
# files -- no NetCDF, no glmtools/gotmtools reading required, so this is the
# one branch of get_output_wq() that's safe to build a fully self-contained
# fixture for (see NEWS.md / session notes: the GLM/SELMAPROTBAS/WET
# branches read real NetCDF via glmtools::get_var()/gotmtools::get_vari(),
# whose exact dimension/variable conventions aren't verified anywhere
# locally, so those remain untested rather than guessed at).
#
# The LER config fixture below (LakeEnsemblR.yaml) has to match
# gotmtools::get_yaml_value()'s real contract precisely -- it is NOT a real
# YAML parser, it's a line-scanner that requires the key to be indented by
# exactly 3 literal spaces (`key_id <- paste0('   ', key, ':')`, hardcoded
# in its source), regardless of what's otherwise valid YAML. Verified
# against https://github.com/aemon-j/gotmtools/blob/master/R/get_yaml_value.R
# (an initial 2-space-indented fixture failed with "Could not find Simstrat
# entry in LER config" for exactly this reason before this was confirmed).

.make_get_output_simstrat_fixture <- function(values = c(10, 10.5, 11),
                                              conversion_factor_check = FALSE) {
  root <- tempfile("lerwq_gow_")
  sim_root <- file.path(root, "Simstrat-AED2")   # simstrat.par lives here
  sim_out  <- file.path(sim_root, "output")       # <var>_out.dat lives here
  dir.create(sim_out, recursive = TRUE)

  # simstrat.par: JSON-like, same line-scanning contract verified earlier
  # for validate_simstrat()/generate_simstrat_aed2_inflows().
  writeLines(c(
    "{",
    "\"Simulation\": {",
    "\"Timestep s\": 86400,",
    "\"Reference year\": 2020",
    "}",
    "}"
  ), file.path(sim_root, "simstrat.par"))

  # A single depth column ("0", i.e. surface) -- sidesteps needing to
  # predict the function's own multi-column depth-reordering logic, while
  # still exercising time conversion, unit scaling, and Depth_ naming.
  writeLines(c(
    "Time,0",
    paste0("0,", values[1]),
    paste0("1,", values[2]),
    paste0("2,", values[3])
  ), file.path(sim_out, "OXY_oxy_out.dat"))

  # LER config: physical model coupling, points at simstrat.par.
  # gotmtools::get_yaml_value() does NOT use a real YAML parser -- it's a
  # line-scanner requiring the key to be indented by exactly 3 spaces
  # (`key_id <- paste0('   ', key, ':')`, hardcoded), not "however YAML
  # happens to be indented". Confirmed against the real source at
  # https://github.com/aemon-j/gotmtools/blob/master/R/get_yaml_value.R.
  writeLines(c(
    "config_files:",
    "   Simstrat: Simstrat-AED2/simstrat.par"
  ), file.path(root, "LakeEnsemblR.yaml"))

  # Output.yaml-shaped config load_config() reads
  writeLines("dummy", file.path(root, "bathy.csv"))
  config_path <- file.path(root, "Output.yaml")
  writeLines(c(
    "folder:",
    "files:",
    "  bathy_file: \"bathy.csv\"",
    "  metric_yaml_file: \"Output.yaml\"",
    "  LER_config_file: \"LakeEnsemblR.yaml\"",
    "model_folders:",
    paste0("  SIMSTRAT: \"", gsub("\\\\", "/", sim_out), "\"")
  ), config_path)

  config_path
}

test_that("get_output_wq() Simstrat branch converts time, names Depth_ columns, and applies conversion_factor", {
  config_path <- .make_get_output_simstrat_fixture()

  out <- get_output_wq(
    config_file = config_path, model = "SIMSTRAT", vars = "OXY_oxy",
    depth_01 = 1, conversion_factor = 1
  )

  df <- out[[1]]
  expect_setequal(names(df), c("datetime", "Depth_0"))
  expect_equal(df$Depth_0, c(10, 10.5, 11))
  # reference_year = 2020, raw time 0/1/2 (days) -> Jan 1/2/3 2020, UTC
  expect_equal(as.Date(df$datetime), as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")))
})

test_that("get_output_wq() Simstrat branch applies conversion_factor", {
  config_path <- .make_get_output_simstrat_fixture()

  out <- get_output_wq(
    config_file = config_path, model = "SIMSTRAT", vars = "OXY_oxy",
    depth_01 = 1, conversion_factor = 2
  )

  expect_equal(out[[1]]$Depth_0, c(20, 21, 22))
})
