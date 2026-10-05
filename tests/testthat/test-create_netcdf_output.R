test_that("create_netcdf_output() writes one variable per phytoplankton group", {
  dir <- tempfile("lerwq_nc_")
  dir.create(dir)
  wq_cfg <- file.path(dir, "LakeEnsemblR_WQ.yaml")
  yaml::write_yaml(list(phytoplankton = list(groups = list(diatoms = list(), cyanobacteria = list()))), wq_cfg)

  dt <- as.POSIXct("2022-01-01", tz = "UTC") + (0:2) * 86400
  prof <- function(v) data.frame(datetime = dt, Depth_0 = v, Depth_1 = v)
  m <- list(Phyto_C_miligramsPerCubicMeter = list(
    GLM = list(Phyto_C_miligramsPerCubicMeter_PHY_diatoms = prof(1),
               Phyto_C_miligramsPerCubicMeter_PHY_cyanobacteria = prof(2)),
    WET = list(Phyto_C_miligramsPerCubicMeter_diatoms_sDW = prof(3),
               Phyto_C_miligramsPerCubicMeter_cyanobacteria_sDW = prof(4))))

  f <- suppressMessages(create_netcdf_output(m, folder = dir, model = c("GLM", "WET"),
                                             longitude = 9.8, latitude = 56.1,
                                             wq_config_file = wq_cfg, members = 1))
  nc <- ncdf4::nc_open(f)
  on.exit(ncdf4::nc_close(nc))
  expect_setequal(names(nc$var), c("Phyto_C_miligramsPerCubicMeter_diatoms",
                                   "Phyto_C_miligramsPerCubicMeter_cyanobacteria"))
  dia <- ncdf4::ncvar_get(nc, "Phyto_C_miligramsPerCubicMeter_diatoms", collapse_degen = FALSE)
  cya <- ncdf4::ncvar_get(nc, "Phyto_C_miligramsPerCubicMeter_cyanobacteria", collapse_degen = FALSE)
  # dims: lon, lat, member, model (GLM, WET, Obs), time, z
  expect_equal(unique(as.numeric(dia[1, 1, 1, 1, , ])), 1)
  expect_equal(unique(as.numeric(cya[1, 1, 1, 1, , ])), 2)
  expect_equal(unique(as.numeric(dia[1, 1, 1, 2, , ])), 3)
  expect_equal(unique(as.numeric(cya[1, 1, 1, 2, , ])), 4)
})
