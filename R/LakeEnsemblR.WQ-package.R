#' @keywords internal
#' @importFrom stats aggregate approx setNames time
#' @importFrom utils flush.console head modifyList read.table write.csv write.table
"_PACKAGE"

# Column/variable names referenced via non-standard evaluation (dplyr's
# tidy-eval verbs, ggplot2's aes()) rather than as actual R objects in scope
# -- R CMD check's static analysis can't tell these apart from genuine
# missing globals, so it flags every one of them. Declaring them here is the
# standard base-R way to silence that false positive (see ?globalVariables);
# it has no effect on runtime behavior. LakeEnsemblR_WQ_dictionary is the one
# real exception in this list: it's the package's own lazy-loaded data
# object (see data.R), referenced by bare name rather than via
# LakeEnsemblR.WQ::LakeEnsemblR_WQ_dictionary or data() -- same false
# positive, different cause.
utils::globalVariables(c(
  "AF_total", "Consecutive_Strat_Days", "Depth", "LakeEnsemblR_WQ_dictionary",
  "Mixing_DOY", "Mixing_Start_Date", "Model", "Month", "Observed", "Predicted",
  "Strat_DOY", "Strat_Start_Date", "Value", "Value_obs", "Value_pred", "Year",
  "Year_upd", "anoxic_depth", "conversion_factor", "depth", "depth_01",
  "depth_value", "function_name", "ice_duration_days", "ice_thickness",
  "metric_mod", "model", "num_anoxic_days", "thermo.depth", "top",
  "variable_model_name", "wq_conv", "wq_var_dic"
))
