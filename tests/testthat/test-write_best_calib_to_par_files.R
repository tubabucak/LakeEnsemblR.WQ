# write_best_calib_to_par_files() picks its "best" row in this order:
#   1. attr(lhc_results, "best_parameter_set")  (LHC's best, or DE's if DE ran)
#   2. lhc_results$is_best == TRUE
#   3. max/min of the metric column
# These tests build a minimal synthetic fixture (no real model, no real
# LakeEnsemblR_WQ.yaml) and confirm each tier is actually used when present,
# so a future regression here (like the DE-best being silently ignored) fails
# a test instead of only being caught by manual inspection.

make_fixture <- function() {
  folder <- withr::local_tempdir(.local_envir = parent.frame())

  par_df <- data.frame(
    parameter     = "p1",
    model_coupled = "GLM-AED2",
    domain        = "d",
    process       = "pr",
    subprocess    = "sp",
    value         = 0,
    stringsAsFactors = FALSE
  )
  write.csv(par_df, file.path(folder, "par_p1.csv"), row.names = FALSE)

  yaml::write_yaml(list(oxygen = list(par_file = "par_p1.csv")), file.path(folder, "cfg.yaml"))

  calib_setup <- data.frame(
    pars          = "p1",
    model_coupled = "GLM-AED2",
    domain        = "d",
    process       = "pr",
    subprocess    = "sp",
    module        = "oxygen",
    group_name    = NA_character_,
    stringsAsFactors = FALSE
  )

  list(folder = folder, calib_setup = calib_setup)
}

test_that("write_best_calib_to_par_files prefers the best_parameter_set attribute (DE's result) over table rows", {
  fx <- make_fixture()

  lhc_results <- data.frame(
    p1 = c(10, 20),
    sample_index = c(1, 2),
    KGE = c(0.1, 0.9),
    n_pairs = c(5, 5),
    is_best = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  # Simulate DE having refined beyond anything in the LHC table -- this is
  # exactly the situation run_lhc_wq()'s DE phase produces.
  attr(lhc_results, "best_parameter_set") <- data.frame(
    sample_index = NA_integer_,
    p1 = 42,
    stringsAsFactors = FALSE
  )

  written <- write_best_calib_to_par_files(
    lhc_results  = lhc_results,
    calib_setup  = fx$calib_setup,
    config_file  = "cfg.yaml",
    folder       = fx$folder,
    metric       = "KGE",
    minimize     = FALSE,
    write_target = "par_file",
    verbose      = FALSE
  )

  expect_equal(unname(written$p1), 42)

  updated <- read.csv(file.path(fx$folder, "par_p1.csv"), stringsAsFactors = FALSE)
  expect_equal(updated$value[updated$parameter == "p1"], 42)
})

test_that("write_best_calib_to_par_files falls back to is_best when no attribute is present", {
  fx <- make_fixture()

  lhc_results <- data.frame(
    p1 = c(10, 20),
    sample_index = c(1, 2),
    KGE = c(0.9, 0.1),   # deliberately NOT the max-KGE row, to isolate is_best from metric fallback
    n_pairs = c(5, 5),
    is_best = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  written <- write_best_calib_to_par_files(
    lhc_results  = lhc_results,
    calib_setup  = fx$calib_setup,
    config_file  = "cfg.yaml",
    folder       = fx$folder,
    metric       = "KGE",
    minimize     = FALSE,
    write_target = "par_file",
    verbose      = FALSE
  )

  expect_equal(unname(written$p1), 20)
})

test_that("write_best_calib_to_par_files falls back to the metric column when neither attribute nor is_best is present", {
  fx <- make_fixture()

  lhc_results <- data.frame(
    p1 = c(10, 20),
    KGE = c(0.9, 0.1),
    stringsAsFactors = FALSE
  )

  written <- write_best_calib_to_par_files(
    lhc_results  = lhc_results,
    calib_setup  = fx$calib_setup,
    config_file  = "cfg.yaml",
    folder       = fx$folder,
    metric       = "KGE",
    minimize     = FALSE,
    write_target = "par_file",
    verbose      = FALSE
  )

  expect_equal(unname(written$p1), 10)
})
