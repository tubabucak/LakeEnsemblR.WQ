# Changelog

## LakeEnsemblR.WQ 0.0.1

Initial development version. Substantially extends the upstream
`aemon-j/LakeEnsemblR.WQ` codebase – at the point this fork diverged,
upstream had only the physical/WQ configuration export functions in
place
([`export_config_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/export_config_wq.md)
and related setup functions). This version adds:

- **Validation and run**:
  [`validate_glm_aed()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/validate_glm_aed.md),
  [`validate_gotm_wet()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/validate_gotm_wet.md),
  [`validate_simstrat()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/validate_simstrat.md),
  [`run_ensemble_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_ensemble_wq.md).
- **Calibration and sensitivity**:
  [`create_calibration_tables()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/create_calibration_tables.md),
  [`calib_setup_from_tables()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/calib_setup_from_tables.md),
  [`run_lhc_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)
  (with an optional parallel backend,
  [`run_lhc_wq_parallel()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq_parallel.md)),
  [`run_sensitivity()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_sensitivity.md),
  [`run_multi_param_sensitivity()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_multi_param_sensitivity.md),
  [`cali_ensemble_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cali_ensemble_wq.md)
  (multi-model calibration wrapper with optional Differential Evolution
  refinement via `DEoptim`),
  [`write_best_calib_to_par_files()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/write_best_calib_to_par_files.md).
- **Extraction and metrics**:
  [`get_output_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/get_output_wq.md),
  [`extract_variable_list()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/extract_variable_list.md),
  [`integrate_depths()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/integrate_depths.md),
  [`cal_metrics()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
  and the harmonized-metric family
  ([`cal_stats()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_stats.md),
  [`cal_anoxic_date()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_anoxic_date.md),
  [`cal_bot_surf_temp_dif()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_bot_surf_temp_dif.md),
  [`cal_DO_exceedance()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_DO_exceedance.md),
  [`cal_epi_depth()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_epi_depth.md),
  [`cal_ice_duration()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_ice_duration.md),
  [`cal_meta_depth()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_meta_depth.md),
  [`cal_nitrif_selma()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_nitrif_selma.md),
  [`cal_strat_date()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_strat_date.md)).
- **Post-processing and comparison**:
  [`compare_plot()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/compare_plot.md),
  [`scat_plot()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/scat_plot.md),
  [`compare_models_metric()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/compare_models_metric.md),
  [`compare_models_metric_netcdf()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/compare_models_metric_netcdf.md).
- **Visualization**:
  [`plot_model_vs_obs_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/plot_model_vs_obs_wq.md),
  [`plot_heatmap_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/plot_heatmap_wq.md),
  [`plot_strat_metrics()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/plot_strat_metrics.md),
  [`plot_anoxic_metrics()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/plot_anoxic_metrics.md),
  [`plot_ice_metrics()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/plot_ice_metrics.md).
- **NetCDF output**:
  [`create_netcdf_output()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/create_netcdf_output.md),
  [`create_netcdf_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/create_netcdf_wq.md).

### Documentation

- Added the “Full Workflow” vignette
  ([`vignette("full-workflow")`](https://tubabucak.github.io/LakeEnsemblR.WQ/articles/full-workflow.md)),
  an end-to-end walkthrough covering configuration export, validation,
  ensemble runs, metric computation, the calibration workflow (LHC
  sampling, sensitivity analysis, optional Differential Evolution
  refinement, multi-model calibration via
  [`cali_ensemble_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cali_ensemble_wq.md)),
  and comparison/visualization of model outputs.
- Added a new vignette, “Configuration File Reference”
  ([`vignette("config-reference")`](https://tubabucak.github.io/LakeEnsemblR.WQ/articles/config-reference.md)),
  documenting `LakeEnsemblR_WQ.yaml` and `Output.yaml` field by field,
  plus the WQ-specific input file formats (nutrient inflow CSV,
  parameter override CSV, observed-data CSV)
- `README.Rmd`: added links to the built documentation site and to the
  example test-case projects (`LERWQ_testcases`); the calibration
  workflow section now points to the full-workflow vignette with a
  clickable link instead of only an R command reference.

### Testing

- Added an initial test suite (`testthat`), including coverage for the
  config-loading and calibration-table logic:
  [`load_config()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/load_config-colon.md),
  [`calib_setup_from_tables()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/calib_setup_from_tables.md),
  [`create_calibration_tables()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/create_calibration_tables.md),
  [`run_sensitivity()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_sensitivity.md)’s
  validation chain,
  [`generate_simstrat_aed2_inflows()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/generate_simstrat_aed2_inflows.md)
  and its parsing helpers, the
  [`validate_glm_aed()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/validate_glm_aed.md)/[`validate_gotm_wet()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/validate_gotm_wet.md)/[`validate_simstrat()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/validate_simstrat.md)
  family, and all 8 `cal_*` metric functions (`cal_DO_exceedance`,
  `cal_anoxic_date`, `cal_nitrif_selma`, `cal_bot_surf_temp_dif`,
  `cal_ice_duration`, `cal_epi_depth`, `cal_meta_depth`,
  `cal_strat_date`).

### Infrastructure

- Added `pkgdown` and `R-CMD-check` GitHub Actions workflows.
