# Function Reference Draft

This page is a draft reference map for LakeEnsemblR.WQ functions. It can
be used as a starting point for a pkgdown reference index.

## Configuration and setup

- create_input_tables()
- create_calib_tables()
- convert_ler_to_lerwq()
- set_up_configs()
- set_value_config()
- set_coupling()
- disable_module()
- input_pclakeconfig()
- export_config_wq()
- export_inputs()
- export_pclake_physics()

## Validation and execution

- validate_glm_aed()
- validate_gotm_wet()
- validate_simstrat()
- run_ensemble_wq()
- run_sensitivity()
- run_multi_param_sensitivity()
- run_lhc_wq()

## Simstrat-AED2 helpers

- generate_simstrat_aed2_inflows()

## Output extraction and harmonization

- load_config()
- check_the_metrics()
- extract_variable_list()
- get_output_wq()
- integrate_depths()

## Metric calculations

- cal_metrics()
- cal_stats()
- export_all_stats()

## Metric helper functions

- cal_anoxic_date()
- cal_bot_surf_temp_dif()
- cal_DO_exceedance()
- cal_epi_depth()
- cal_ice_duration()
- cal_meta_depth()
- cal_nitrif_selma()
- cal_strat_date()

## Visualization and comparison

- compare_plot()
- scat_plot()
- visualise_dictionary()
- visualize_dictionary()

## Suggested pkgdown grouping

If you use \_pkgdown.yml, map the reference sections to the groups above
so setup, execution, extraction, and metrics are clearly separated for
users.
