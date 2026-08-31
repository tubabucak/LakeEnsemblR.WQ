#' LakeEnsemblR.WQ parameter dictionary
#'
#' The master dictionary of biogeochemical model parameters used throughout
#' the calibration workflow (\code{\link{create_calibration_tables}},
#' \code{\link{set_value_config}}, \code{\link{export_config_wq}}, and
#' related functions). Each row maps one parameter, for one coupled model,
#' to its default value, physical location in that model's native config
#' file, and (for module/domain/process/subprocess) its place in the
#' package's calibration-table hierarchy.
#'
#' @format A data frame with (at least) the following columns:
#' \describe{
#'   \item{module}{Biogeochemical module the parameter belongs to (e.g.
#'     \code{"carbon"}, \code{"nitrogen"}, \code{"phytoplankton"}).}
#'   \item{domain}{Where the process occurs (e.g. \code{"water"},
#'     \code{"sediment"}).}
#'   \item{process}{Higher-level process category (e.g. \code{"growth"},
#'     \code{"nitrification"}).}
#'   \item{subprocess}{More specific process label within \code{process}.}
#'   \item{model}{Short model key the parameter applies to (e.g.
#'     \code{"aed2"}, \code{"wet"}, \code{"selmaprotbas"}, \code{"pclake"}).}
#'   \item{parameter}{The parameter's native name in that model.}
#'   \item{path}{Location of the parameter within the model's native config
#'     file (e.g. \code{"aed2_carbon/ionic"}), used by
#'     \code{\link{set_value_config}} to write values in place.}
#'   \item{unit}{Parameter's unit, or a type marker such as
#'     \code{"(integer)"}/\code{"(boolean)"} for non-continuous parameters
#'     -- see \code{\link{create_calibration_tables}}, which excludes
#'     these from percentage-based calibration bounds.}
#'   \item{default}{Default value shipped with the model/dictionary.}
#'   \item{version}{Dictionary/model version this row applies to.}
#'   \item{note}{Free-text description of the parameter.}
#' }
#'
#' @source \code{data-raw/LakeEnsemblR_WQ_dictionary.csv}
"LakeEnsemblR_WQ_dictionary"

#' Harmonized metrics dictionary
#'
#' Maps each model's native output variable to a standardized, harmonized
#' metric name and unit, used by \code{\link{cal_metrics}} and
#' \code{\link{get_output_wq}} to produce comparable output across coupled
#' models. This is the same dictionary referenced by the
#' \code{Level1}/\code{Level2}/\code{Level3} blocks in \code{Output.yaml}
#' (see \code{vignette("config-reference")}).
#'
#' @format A data frame with (at least) the following columns:
#' \describe{
#'   \item{metric_name}{Full metric name as it appears in \code{Output.yaml}
#'     (e.g. \code{"Duration_of_Stratification"}).}
#'   \item{metric_short_name}{Short label for the metric.}
#'   \item{domain}{Where the metric applies (e.g. \code{"Water"}).}
#'   \item{module}{Metric category (e.g. \code{"LER"}, \code{"Oxygen"}).}
#'   \item{level}{Complexity tier: \code{"Level1"} (direct variables),
#'     \code{"Level2"}/\code{"Level3"} (derived metrics).}
#'   \item{variable_global_name}{Harmonized variable name shared across
#'     models (e.g. \code{"DO_gramsPerCubicMeter"}) -- the same name used in
#'     observed-data CSVs' \code{variable_global_name} column.}
#'   \item{unit_global}{Unit of \code{variable_global_name}.}
#'   \item{variable_model_name_old, variable_model_name}{The model-native
#'     variable name this harmonized metric is derived from.}
#'   \item{depth_01}{Whether the variable has a depth dimension (\code{1})
#'     or not (\code{0}).}
#'   \item{model}{Which coupled model this row applies to (e.g.
#'     \code{"GLM"}, \code{"WET"}, \code{"SELMAPROTBAS"}, \code{"SIMSTRAT"}).}
#'   \item{unit_model}{Native unit of the model variable.}
#'   \item{conversion_factor}{Factor applied to convert from
#'     \code{unit_model} to \code{unit_global}.}
#'   \item{function_name}{Name of the internal function used to compute this
#'     metric, when it's a derived (Level2/Level3) metric.}
#' }
#'
#' @source \code{data-raw/Metrics_dict_v2_add.csv}
"Metrics_dict"

#' Molar mass conversion factors
#'
#' Molar masses (g/mol) for the nutrient elements tracked across coupled
#' models, used to convert between mass-based and mole-based units (e.g.
#' grams per cubic meter to millimoles per cubic meter for GOTM-based
#' models) in \code{\link{export_inputs}}.
#'
#' @format A data frame (single row) with columns \code{mol_mass_N},
#'   \code{mol_mass_P}, \code{mol_mass_Si}, \code{mol_mass_O2}, and
#'   \code{mol_mass_C}, giving the molar mass of nitrogen, phosphorus,
#'   silicon, oxygen (as O2), and carbon respectively.
#'
#' @source \code{data-raw/wq_conv.csv}
"wq_conv"

#' Nutrient inflow variable dictionary
#'
#' Maps each standardized nutrient inflow variable name (as used in the
#' nutrient inflow CSV read by \code{\link{export_inputs}} -- see
#' \code{vignette("config-reference")}) to its model-native equivalent for
#' each coupled model.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{standard_name}{Standardized variable name expected in the
#'     nutrient inflow CSV (e.g. \code{"wq_NO3_gramsPerCubicMeter"}),
#'     validated by \code{chk_names_nutr_flow()}.}
#'   \item{short_name}{Short label for the variable.}
#'   \item{nutrient}{Which nutrient element this variable represents
#'     (\code{"N"}, \code{"P"}, or \code{"Si"}).}
#'   \item{unit}{Unit of \code{standard_name}.}
#'   \item{aed2, selmaprotbas, wet, mylake, pclake}{The equivalent
#'     model-native variable name for each coupled model (\code{"-"} if not
#'     applicable to that model).}
#' }
#'
#' @source \code{data-raw/wq_var_dic.csv}
"wq_var_dic"
