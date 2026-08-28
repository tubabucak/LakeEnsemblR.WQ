# cal_epi_depth(), cal_meta_depth(), and cal_strat_date() all delegate the
# actual thermocline/stratification detection to rLakeAnalyzer
# (ts.meta.depths()/ts.thermo.depth()), which implements a real physical
# algorithm (density-gradient-based). These tests deliberately do NOT assert
# exact hand-derived numeric values -- that would require independently
# re-implementing rLakeAnalyzer's own algorithm to know what's "correct".
# Instead they check structural contracts (right shape/columns, no crash,
# sane bounds) against a clearly-stratified synthetic profile, using
# rLakeAnalyzer's expected "wtr_<depth>" column naming convention.

.stratified_profile <- function(n_days = 10) {
  # Warm surface (~22C), cold bottom (~6C), sharp gradient around 4-6m --
  # an unambiguous stratified profile.
  data.frame(
    datetime = seq(as.POSIXct("2020-06-01"), by = "day", length.out = n_days),
    wtr_0    = rep(22, n_days),
    wtr_1    = rep(21.5, n_days),
    wtr_2    = rep(20, n_days),
    wtr_4    = rep(15, n_days),
    wtr_6    = rep(8, n_days),
    wtr_8    = rep(6.5, n_days),
    wtr_10   = rep(6, n_days)
  )
}

.stratified_profile_range <- function(start, end) {
  dates <- seq(as.POSIXct(start), as.POSIXct(end), by = "day")
  n <- length(dates)
  data.frame(
    datetime = dates,
    wtr_0    = rep(22, n),
    wtr_1    = rep(21.5, n),
    wtr_2    = rep(20, n),
    wtr_4    = rep(15, n),
    wtr_6    = rep(8, n),
    wtr_8    = rep(6.5, n),
    wtr_10   = rep(6, n)
  )
}

test_that("cal_epi_depth() returns one non-negative thickness per row", {
  temp_data <- .stratified_profile()

  out <- cal_epi_depth(temp_data)

  expect_equal(nrow(out), nrow(temp_data))
  expect_setequal(names(out), c("datetime", "epi_thickness"))
  expect_true(all(out$epi_thickness >= 0))
  # Epilimnion shouldn't extend past the deepest measured depth (10 m).
  expect_true(all(out$epi_thickness <= 10))
})

test_that("cal_meta_depth() returns one non-negative thickness per row", {
  temp_data <- .stratified_profile()

  out <- cal_meta_depth(temp_data)

  expect_equal(nrow(out), nrow(temp_data))
  expect_setequal(names(out), c("datetime", "meta_thickness"))
  expect_true(all(out$meta_thickness >= 0))
})

test_that("cal_strat_date() runs for both hemispheres and returns the documented columns", {
  # Nov-Jan spans the hemisphere-correction boundary, giving both branches
  # real overlapping data to work with.
  temp_data <- .stratified_profile_range("2020-11-01", "2021-01-15")

  out_n <- cal_strat_date(temp_data, hemisphere = "N")
  out_s <- cal_strat_date(temp_data, hemisphere = "S")

  for (out in list(out_n, out_s)) {
    # NULL/0-row is a legitimate "no stratification detected" outcome for
    # some year/hemisphere combinations -- only check structure when there
    # actually is a result to check.
    if (!is.null(out) && nrow(out) > 0) {
      expect_true(all(c("Year", "Strat_Start_Date", "Consecutive_Strat_Days",
                        "Mixing_Start_Date") %in% names(out)))
      expect_true(all(out$Consecutive_Strat_Days >= 1))
    }
  }
})

test_that("cal_strat_date() finds Southern Hemisphere results for data confined to Jan-Jun", {
  # Regression test: Year_upd (the season-year used to filter S-hemisphere
  # rows) shifts Jan-Jun dates back one calendar year. The loop used to
  # iterate over the raw Year instead of Year_upd, so a dataset confined
  # entirely to Jan-Jun had a raw Year that never matched any row's own
  # Year_upd -- S-hemisphere results came back NULL even with clearly
  # stratified data throughout. Now the loop uses Year_upd for hemisphere
  # = "S", so this must return a real, non-empty result.
  temp_data <- .stratified_profile_range("2021-01-01", "2021-03-31")

  out_s <- cal_strat_date(temp_data, hemisphere = "S")

  expect_false(is.null(out_s))
  expect_gt(nrow(out_s), 0)
  expect_equal(out_s$Year, 2020)  # Jan-Mar 2021 belongs to the 2020 S-hemisphere season
})
