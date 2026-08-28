# Covers the AED2-model-list/inflow-file parsing helpers in
# R/gen_simstrat_input.r, plus generate_simstrat_aed2_inflows() itself. The
# integration tests directly guard against a real regression fixed earlier:
# inflow_map's `module`/`inflow_var` columns were misaligned by position, so
# organic-matter variables (OGM_pon/OGM_don/OGM_dop/OGM_pop) were incorrectly
# attributed to the nitrogen/phosphorus modules instead of their own, and
# NIT_n2o_inflow/NIT_no2_inflow were missing from the map entirely.

.write_lines <- function(path, ...) writeLines(c(...), path)

test_that("get_active_aed2_modules() parses a simple &aed2_models block", {
  f <- tempfile(fileext = ".nml")
  .write_lines(f,
    "&aed2_models",
    "   models = 'aed2_carbon','aed2_nitrogen','aed2_oxygen'",
    "/"
  )

  expect_equal(get_active_aed2_modules(f),
               c("aed2_carbon", "aed2_nitrogen", "aed2_oxygen"))
})

test_that("get_active_aed2_modules() errors when no &aed2_models block is present", {
  f <- tempfile(fileext = ".nml")
  .write_lines(f, "&some_other_block", "x = 1", "/")

  expect_error(get_active_aed2_modules(f), regexp = "Could not find")
})

test_that("get_phyto_names() extracts and sanitizes group names", {
  f <- tempfile(fileext = ".nml")
  .write_lines(f,
    "&phyto_data",
    "   pd%p_name = 'diatoms','Green Algae'",
    "/"
  )

  expect_equal(get_phyto_names(f, sanitize = TRUE), c("diatoms", "green_algae"))
  expect_equal(get_phyto_names(f, sanitize = FALSE), c("diatoms", "Green Algae"))
})

test_that("get_zoop_names() extracts and sanitizes the zooplankton name", {
  f <- tempfile(fileext = ".nml")
  .write_lines(f,
    "&zoop_data",
    "   zoop_param%zoop_name = 'Daphnia sp.'",
    "/"
  )

  expect_equal(get_zoop_names(f, sanitize = TRUE), "daphnia_sp")
  expect_equal(get_zoop_names(f, sanitize = FALSE), "Daphnia sp.")
})

# ---- generate_simstrat_aed2_inflows() -------------------------------------

.make_simstrat_fixture <- function(active_modules) {
  dir <- tempfile("lerwq_simin_")
  dir.create(dir)

  aed2_file <- file.path(dir, "aed2.nml")
  .write_lines(aed2_file,
    "&aed2_models",
    paste0("   models = ", paste(sprintf("'%s'", active_modules), collapse = ",")),
    "/"
  )

  # InflowMode = 0 keeps format_aed_inflow_simstrat() on its simplest branch.
  sim_par <- file.path(dir, "simstrat.par")
  .write_lines(sim_par,
    "{",
    "\"Simulation\": {",
    "\"Start d\": 0,",
    "\"End d\": 2",
    "},",
    "\"ModelConfig\": {",
    "\"InflowMode\": 0",
    "}",
    "}"
  )

  out_dir <- file.path(dir, "out")

  list(dir = dir, aed2_file = aed2_file, sim_par = sim_par, out_dir = out_dir)
}

test_that("generate_simstrat_aed2_inflows() writes only nitrogen inflows for aed2_nitrogen, including n2o/no2", {
  fx <- .make_simstrat_fixture("aed2_nitrogen")

  written <- generate_simstrat_aed2_inflows(
    aed2_file = fx$aed2_file, sim_par = fx$sim_par, out_dir = fx$out_dir
  )

  expect_setequal(written,
                  c("NIT_amm_inflow", "NIT_nit_inflow", "NIT_no2_inflow", "NIT_n2o_inflow"))
  # None of the organic-matter files should exist -- this is the regression
  # the old misaligned inflow_map would have failed: OGM_pon/OGM_don were
  # previously (incorrectly) tied to the nitrogen module.
  expect_false(file.exists(file.path(fx$out_dir, "OGM_pon_inflow.dat")))
  expect_false(file.exists(file.path(fx$out_dir, "OGM_don_inflow.dat")))
  expect_true(file.exists(file.path(fx$out_dir, "NIT_n2o_inflow.dat")))
  expect_true(file.exists(file.path(fx$out_dir, "NIT_no2_inflow.dat")))
})

test_that("generate_simstrat_aed2_inflows() writes all 6 organic-matter inflows for aed2_organic_matter, and nothing else", {
  fx <- .make_simstrat_fixture("aed2_organic_matter")

  written <- generate_simstrat_aed2_inflows(
    aed2_file = fx$aed2_file, sim_par = fx$sim_par, out_dir = fx$out_dir
  )

  expect_setequal(written, c(
    "OGM_pon_inflow", "OGM_don_inflow", "OGM_doc_inflow",
    "OGM_poc_inflow", "OGM_dop_inflow", "OGM_pop_inflow"
  ))
  expect_false(any(grepl("^NIT_", written)))
  expect_false(any(grepl("^PHS_", written)))
})

test_that("generate_simstrat_aed2_inflows() writes only PHS_frp_inflow for aed2_phosphorus (not the OGM P variables)", {
  fx <- .make_simstrat_fixture("aed2_phosphorus")

  written <- generate_simstrat_aed2_inflows(
    aed2_file = fx$aed2_file, sim_par = fx$sim_par, out_dir = fx$out_dir
  )

  # Regression check: OGM_dop_inflow/OGM_pop_inflow were previously
  # (incorrectly) tied to the phosphorus module instead of organic_matter.
  expect_equal(written, "PHS_frp_inflow")
})

test_that("generate_simstrat_aed2_inflows() does not overwrite an existing file unless overwrite = TRUE", {
  fx <- .make_simstrat_fixture("aed2_oxygen")
  dir.create(fx$out_dir, recursive = TRUE)
  existing <- file.path(fx$out_dir, "OXY_oxy_inflow.dat")
  writeLines("SENTINEL", existing)

  generate_simstrat_aed2_inflows(
    aed2_file = fx$aed2_file, sim_par = fx$sim_par, out_dir = fx$out_dir
  )
  expect_equal(readLines(existing), "SENTINEL")

  generate_simstrat_aed2_inflows(
    aed2_file = fx$aed2_file, sim_par = fx$sim_par, out_dir = fx$out_dir,
    overwrite = TRUE
  )
  expect_false(identical(readLines(existing), "SENTINEL"))
})

test_that("generate_simstrat_aed2_inflows() errors when phytoplankton is active but phyto_pars_file is missing", {
  fx <- .make_simstrat_fixture("aed2_phytoplankton")

  expect_error(
    generate_simstrat_aed2_inflows(
      aed2_file = fx$aed2_file, sim_par = fx$sim_par, out_dir = fx$out_dir
    ),
    regexp = "phyto_pars_file"
  )
})
