# LakeEnsemblR.WQ 0.0.1

Initial development version. Substantially extends the upstream
`aemon-j/LakeEnsemblR.WQ` codebase -- at the point this fork diverged,
upstream had only the physical/WQ configuration export functions in place
(`export_config_wq()` and related setup functions). This version adds:

* **Validation and run**: `validate_glm_aed()`, `validate_gotm_wet()`,
  `validate_simstrat()`, `run_ensemble_wq()`.
* **Calibration and sensitivity**: `create_calibration_tables()`,
  `calib_setup_from_tables()`, `run_lhc_wq()` (with an optional parallel
  backend, `run_lhc_wq_parallel()`), `run_sensitivity()`,
  `run_multi_param_sensitivity()`, `cali_ensemble_wq()` (multi-model
  calibration wrapper with optional Differential Evolution refinement via
  `DEoptim`), `write_best_calib_to_par_files()`.
* **Extraction and metrics**: `get_output_wq()`, `extract_variable_list()`,
  `integrate_depths()`, `cal_metrics()` and the harmonized-metric family
  (`cal_stats()`, `cal_anoxic_date()`, `cal_bot_surf_temp_dif()`,
  `cal_DO_exceedance()`, `cal_epi_depth()`, `cal_ice_duration()`,
  `cal_meta_depth()`, `cal_nitrif_selma()`, `cal_strat_date()`).
* **Post-processing and comparison**: `compare_plot()`, `scat_plot()`,
  `compare_models_metric()`, `compare_models_metric_netcdf()`.
* **Visualization**: `plot_model_vs_obs_wq()`, `plot_heatmap_wq()`,
  `plot_strat_metrics()`, `plot_anoxic_metrics()`, `plot_ice_metrics()`.
* **NetCDF output**: `create_netcdf_output()`, `create_netcdf_wq()`.

## Documentation

* Added the "Full Workflow" vignette (`vignette("full-workflow")`), an
  end-to-end walkthrough covering configuration export, validation,
  ensemble runs, metric computation, the calibration workflow (LHC
  sampling, sensitivity analysis, optional Differential Evolution
  refinement, multi-model calibration via `cali_ensemble_wq()`), and
  comparison/visualization of model outputs.
* Added a new vignette, "Configuration File Reference"
  (`vignette("config-reference")`), documenting `LakeEnsemblR_WQ.yaml` and
  `Output.yaml` field by field, plus the WQ-specific input file formats
  (nutrient inflow CSV, parameter override CSV, observed-data CSV)
* `README.Rmd`: added links to the built documentation site and to the
  example test-case projects (`LERWQ_testcases`); the calibration workflow
  section now points to the full-workflow vignette with a clickable link
  instead of only an R command reference.

## Testing

* Added an initial test suite (`testthat`), including coverage for the
  config-loading and calibration-table logic: `load_config()`,
  `calib_setup_from_tables()`, `create_calibration_tables()`,
  `run_sensitivity()`'s validation chain,
  `generate_simstrat_aed2_inflows()` and its parsing helpers, the
  `validate_glm_aed()`/`validate_gotm_wet()`/`validate_simstrat()` family,
  and all 8 `cal_*` metric functions (`cal_DO_exceedance`, `cal_anoxic_date`,
  `cal_nitrif_selma`, `cal_bot_surf_temp_dif`, `cal_ice_duration`,
  `cal_epi_depth`, `cal_meta_depth`, `cal_strat_date`).

## Infrastructure

* Added `pkgdown` and `R-CMD-check` GitHub Actions workflows.
