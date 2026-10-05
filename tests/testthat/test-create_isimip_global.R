test_that(".isimip_periods() splits into ISIMIP decades", {
  p <- LakeEnsemblR.WQ:::.isimip_periods(1901, 2019)
  expect_equal(p$start, c(1901, seq(1911, 2011, 10)))
  expect_equal(p$end, c(seq(1910, 2010, 10), 2019))

  p <- LakeEnsemblR.WQ:::.isimip_periods(1850, 2014)
  expect_equal(head(p$start, 2), c(1850, 1851))
  expect_equal(head(p$end, 2), c(1850, 1860))
  expect_equal(tail(p$end, 1), 2014)

  p <- LakeEnsemblR.WQ:::.isimip_periods(2015, 2100)
  expect_equal(head(p$end, 2), c(2020, 2030))
})

test_that(".isimip_grid_index() maps to the ISIMIP 0.5 degree grid", {
  gi <- LakeEnsemblR.WQ:::.isimip_grid_index
  expect_equal(gi(c(89.9, 56.1, -89.9, 90), 90), c(1, 68, 360, 1))   # lat 89.75 first
  expect_equal(gi(c(-179.9, 9.81, 179.9, 180), 180), c(1, 380, 720, 720))
  expect_error(gi(91, 90), "within")
})

test_that(".isimip_epi_hypo() averages above/below the thermocline, whole column when mixed", {
  mat <- matrix(c(1, 2, 3, 4,
                  1, 2, 3, 4,
                  1, 2, 3, 4), nrow = 3, byrow = TRUE,
                dimnames = list(c("2022-07-01", "2022-07-02", "2022-07-03"), c("0", "1", "2", "3")))
  attr(mat, "depth") <- c(0, 1, 2, 3)
  thermo <- c(`2022-07-01` = 1.5, `2022-07-02` = NaN)   # 07-03: no temperature profile

  eh <- LakeEnsemblR.WQ:::.isimip_epi_hypo(mat, thermo)
  expect_equal(unname(eh["2022-07-01", ]), c(1.5, 3.5))
  expect_equal(unname(eh["2022-07-02", ]), c(2.5, 2.5))
  expect_true(all(is.na(eh["2022-07-03", ])))
})

test_that("create_isimip_global() writes each lake into its grid cell", {
  skip_if_not_installed("RNetCDF")
  dir <- tempfile("lerwq_isimip_global_")
  dir.create(dir)

  days <- seq(as.POSIXct("2022-07-01", tz = "UTC"), by = "day", length.out = 3)
  depth_cols <- paste0("Depth_", 0:5)
  prof <- function(v) {
    df <- data.frame(datetime = days, matrix(rep(v, each = 3), nrow = 3))
    names(df)[-1] <- depth_cols
    df
  }
  # stratified temperature (warm top, cold bottom) and a DO profile
  m <- list(Temp_degreeCelcius = list(GLM = list(Temp_degreeCelcius = prof(c(22, 22, 21, 10, 6, 6)))),
            DO_gramsPerCubicMeter = list(GLM = list(DO_gramsPerCubicMeter = prof(c(10, 10, 10, 2, 2, 2)))))
  rds <- file.path(dir, "lake.rds")
  saveRDS(m, rds)
  lakes <- data.frame(id = c("a", "b"), lat = c(56.1, -33.3), lon = c(9.81, 151.2), rds = rds)

  files <- suppressMessages(create_isimip_global(
    lakes, model = "GLM", start_year = 2022, end_year = 2022, out_dir = dir,
    isimip = list(contact = "c", institution = "i"), verbose = FALSE))
  expect_equal(basename(files), "glm-aed_gswp3-w5e5_obsclim_histsoc_default_do_global_daily_2022_2022.nc")

  nc <- ncdf4::nc_open(files)
  on.exit(ncdf4::nc_close(nc))
  expect_equal(rev(vapply(nc$var$do$dim, `[[`, "", "name")), c("time", "levlak", "lat", "lon"))
  expect_equal(nc$dim$time$len, 365)
  x <- ncdf4::ncvar_get(nc, "do")                       # lon x lat x levlak x time
  t1 <- as.numeric(as.Date("2022-07-01") - as.Date("2022-01-01")) + 1
  th <- rLakeAnalyzer::thermo.depth(c(22, 22, 21, 10, 6, 6), 0:5)
  expected <- c(mean(c(10, 10, 10, 2, 2, 2)[0:5 <= th]), mean(c(10, 10, 10, 2, 2, 2)[0:5 > th])) / 31.998
  expect_equal(as.numeric(x[380, 68, , t1]), expected, tolerance = 1e-6)
  expect_equal(as.numeric(x[663, 247, , t1]), expected, tolerance = 1e-6)  # second lake, lat -33.25, lon 151.25
  expect_true(all(is.na(x[380, 68, , 1])))                                 # no data in January
  expect_equal(sum(apply(!is.na(x), c(1, 2), any)), 2)
})

test_that("create_isimip_global() refuses two lakes in one grid cell", {
  skip_if_not_installed("RNetCDF")
  lakes <- data.frame(lat = c(56.1, 56.2), lon = c(9.81, 9.9), rds = "x.rds")
  expect_error(suppressWarnings(create_isimip_global(lakes, model = "GLM", start_year = 2022,
                                                     end_year = 2022, out_dir = tempfile())),
               "same 0.5 degree grid cell")
})
