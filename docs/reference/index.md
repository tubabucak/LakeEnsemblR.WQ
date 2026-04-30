# Package index

## Setup and Configuration

Build model-specific configuration and inputs.

- [`export_config_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/export_config_wq.md)
  : Export settings to model-specific configuration files for LER.WQ
- [`export_inputs()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/export_inputs.md)
  : Reads and sets nutrient inputs for all models
- [`create_input_tables()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/create_input_tables.md)
  : Create input tables (Deprecated)
- [`set_up_configs()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/set_up_configs.md)
  : Sets up the model-specific configuration files
- [`set_value_config()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/set_value_config.md)
  : Set value in model-specific config file
- [`set_coupling()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/set_coupling.md)
  : Set coupling settings for each model
- [`disable_module()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/disable_module.md)
  : Disables a module
- [`input_pclakeconfig()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/input_pclakeconfig.md)
  : Inputs values into a PCLake config file
- [`export_pclake_physics()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/export_pclake_physics.md)
  : Sets PCLake physics settings
- [`generate_simstrat_aed2_inflows()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/generate_simstrat_aed2_inflows.md)
  : Generate Simstrat–AED2 inflow files for active modules

## Validation and Run

Validate setups and execute coupled simulations.

- [`validate_glm_aed()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/validate_glm_aed.md)
  : Validate a GLM-AED model simulation folder
- [`validate_gotm_wet()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/validate_gotm_wet.md)
  : Validate a GOTM-WET model simulation folder
- [`validate_simstrat()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/validate_simstrat.md)
  : Validate a Simstrat-AED2 model simulation folder
- [`run_ensemble_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_ensemble_wq.md)
  : Run an ensemble of LakeEnsemblR.WQ models selected

## Calibration and Sensitivity

Create calibration tables and run calibration/sensitivity analyses.

- [`create_calibration_tables()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/create_calibration_tables.md)
  : Create calibration tables
- [`calib_setup_from_tables()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/calib_setup_from_tables.md)
  : Build calib_setup from edited calibration CSVs
- [`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)
  : Run Latin Hypercube Calibration for Water Quality Models
- [`run_sensitivity()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_sensitivity.md)
  : Run Sensitivity Analysis for a Model Parameter
- [`run_multi_param_sensitivity()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_multi_param_sensitivity.md)
  : Run Multi-Parameter Sensitivity Analysis Using Latin Hypercube
  Sampling

## Extraction and Metrics

Extract model output and compute harmonized metrics/statistics.

- [`get_output_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/get_output_wq.md)
  : Get lake model outputs
- [`extract_variable_list()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/extract_variable_list.md)
  : Create variable list for calculating the metrics
- [`export_all_stats()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/integrate_depths.md)
  [`integrate_depths()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/integrate_depths.md)
  : Calculate & export performance metrics for integrated samples - so
  far only for specific depths
- [`cal_metrics()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
  : Calculate the Metrics
- [`cal_stats()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_stats.md)
  : Calculate Model Performance Statistics
- [`cal_anoxic_date()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_anoxic_date.md)
  : Calculate Anoxic Days
- [`cal_bot_surf_temp_dif()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_bot_surf_temp_dif.md)
  : Calculate surface-bottom temperature difference
- [`cal_DO_exceedance()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_DO_exceedance.md)
  : Calculate DO exceedance
- [`cal_epi_depth()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_epi_depth.md)
  : Calculate Epilimnion Thickness
- [`cal_ice_duration()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_ice_duration.md)
  : Calculate Ice Duration & Thickness
- [`cal_meta_depth()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_meta_depth.md)
  : Calculate Metalimnion Thickness
- [`cal_nitrif_selma()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_nitrif_selma.md)
  : Nitrification rate in SELMAPROTBAS
- [`cal_strat_date()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_strat_date.md)
  : Calculate Stratified Days
- [`compare_plot()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/compare_plot.md)
  : Comparing observed/predicted data and calculate the stats
- [`scat_plot()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/scat_plot.md)
  : Scatter plot modeled vs observed

## Helpers

- [`load_config()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/load_config-colon.md)
  : Load and validate LakeEnsemblR.WQ configuration file
- [`visualise_dictionary()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/visualise_dictionary.md)
  [`visualize_dictionary()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/visualise_dictionary.md)
  : Visualise LakeEnsemblR.WQ dictionary
- [`convert_ler_to_lerwq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/convert_ler_to_lerwq.md)
  : Convert LakeEnsemblR configuration to LakeEnsemblR.WQ
