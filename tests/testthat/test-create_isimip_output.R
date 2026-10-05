.isimip_df <- function(value, days = 3) {
  dt <- as.POSIXct("2022-01-01", tz = "UTC") + (seq_len(days) - 1) * 86400
  data.frame(datetime = dt, Depth_0 = value, Depth_1 = value * 2)
}

test_that("create_netcdf_output(format = 'isimip') writes ISIMIP-structured files", {
  dir <- tempfile("lerwq_isimip_")
  dir.create(dir)
  wq_cfg <- file.path(dir, "LakeEnsemblR_WQ.yaml")
  yaml::write_yaml(list(phytoplankton = list(groups = list(diatoms = list(), greens = list()))), wq_cfg)

  m <- list(
    TP_gramsPerCubicMeter = list(GLM = list(TP_gramsPerCubicMeter = .isimip_df(30.974))),
    PO4_gramsPerCubicMeter = list(GLM = list(PO4_gramsPerCubicMeter = .isimip_df(10))),
    DOP_gramsPerCubicMeter = list(GLM = list(DOP_gramsPerCubicMeter = .isimip_df(5))),
    Phyto_C_miligramsPerCubicMeter = list(GLM = list(
      Phyto_C_miligramsPerCubicMeter_PHY_diatoms = .isimip_df(12011),
      Phyto_C_miligramsPerCubicMeter_PHY_greens = .isimip_df(24022)))
  )

  files <- suppressWarnings(suppressMessages(create_netcdf_output(
    m, folder = dir, model = "GLM", longitude = 9.8, latitude = 56.1,
    wq_config_file = wq_cfg, format = "isimip",
    isimip = list(lake = "Lake Test", contact = "c", institution = "i"))))

  expect_setequal(basename(files), paste0(
    "glm-aed_gswp3-w5e5_obsclim_histsoc_default_",
    c("phytobio", "phytobio-diatoms", "phytobio-greens", "tp", "pp", "tpd"),
    "_lake-test_daily_2022_2022.nc"))

  read_var <- function(var) {
    nc <- ncdf4::nc_open(grep(paste0("_", var, "_"), files, value = TRUE))
    on.exit(ncdf4::nc_close(nc))
    list(x = ncdf4::ncvar_get(nc, var), nc_dims = rev(vapply(nc$var[[var]]$dim, `[[`, "", "name")),
         depth = ncdf4::ncvar_get(nc, "depth"), time = nc$dim$time$vals,
         units = nc$var[[var]]$units, time_units = nc$dim$time$units)
  }

  tp <- read_var("tp")
  expect_equal(tp$nc_dims, c("time", "levlak", "lat", "lon"))
  expect_equal(as.numeric(tp$depth), c(0, 1))
  expect_equal(tp$time_units, "days since 1901-01-01 00:00:00")
  expect_equal(tp$time[1], as.numeric(as.Date("2022-01-01") - as.Date("1901-01-01")))
  expect_equal(tp$units, "mol m-3")
  expect_equal(tp$x[1, ], rep(1, 3), tolerance = 1e-6)          # 30.974 g P/m3 = 1 mol/m3

  expect_equal(read_var("tpd")$x[1, ], rep(15 / 30.974, 3), tolerance = 1e-6)
  expect_equal(read_var("pp")$x[1, ], rep(15.974 / 30.974, 3), tolerance = 1e-6)
  expect_equal(read_var("phytobio")$x[2, ], rep(6, 3), tolerance = 1e-6)  # (1 + 2) mol C x 2 at 1 m
  expect_equal(read_var("phytobio-greens")$x[1, ], rep(2, 3), tolerance = 1e-6)
})

test_that("ISIMIP output rejects unknown settings", {
  m <- list(TP_gramsPerCubicMeter = list(GLM = list(TP_gramsPerCubicMeter = .isimip_df(1))))
  expect_error(create_netcdf_output(m, model = "GLM", longitude = 1, latitude = 1,
                                    format = "isimip", isimip = list(lake = "x", forcng = "y")),
               "Unknown 'isimip' setting")
})
