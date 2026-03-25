#' Export settings to model-specific configuration files for LER.WQ
#'
#' Takes the high-level settings from a LakeEnsemblR_WQ configuration file and 
#' distributes them into the model-specific configuration files (e.g., FABM, AED) 
#' It handles module activation, parameter mapping, and 
#' ensures YAML boolean consistency. If Simstrat 
#' is used with AED2, it also generates the necessary inflow \code{.dat} files.
#'
#' @param config_file character; name of the LakeEnsemblR_WQ master configuration file (e.g., "LakeEnsemblR_WQ.yaml").
#' @param folder character; path to the directory containing the configuration files. Defaults to the current working directory (".").
#' @param verbose logical; if TRUE, prints detailed information about changed parameters to the console.
#' @param convert_from_lakeensemblr logical; if TRUE, runs a conversion step to sync settings from the physical LakeEnsemblR configuration before exporting WQ settings.
#' @param ler_config_file character; name of the base LakeEnsemblR (physical) config file.
#' @param overwrite logical; if TRUE, overwrites existing inflow files. Defaults to FALSE.
#' @details 
#' The function automates the setup of water quality modules. For Simstrat-AED2 
#' setups, it automatically calls \code{generate_simstrat_aed2_inflows} to 
#' create template boundary condition files for all active AED2 modules, 
#' including expanded phytoplankton groups.

#'
#' @examples
#' \dontrun{
#' export_config_wq(config_file = "LakeEnsemblR_WQ.yaml", 
#'                  folder = "/Model_setup", 
#'                  verbose = TRUE)
#' }
#'
#' @importFrom configr read.config
#' @export

export_config_wq <- function(config_file, folder = ".", verbose = FALSE,
                          convert_from_lakeensemblr = TRUE,
                          ler_config_file = "LakeEnsemblR.yaml",
                          overwrite = FALSE){
  
  if(convert_from_lakeensemblr){
    # LakeEnsemblR::export_config has been run beforehand
    # Convert folders and activate wq settings
    convert_ler_to_lerwq(ler_config_file = ler_config_file,
                         lerwq_config_file = config_file,
                         folder = folder,
                         verbose = verbose)
  }
  
  # Read config file as a list
  lst_config <- read.config(file.path(folder, config_file))
  
  modules <- names(lst_config)
  modules <- modules[!(modules %in% c("models", "config_files", "run_settings",
                                      "input", "output"))]
  
  # Set up the model-specific config files, with right amount
  # of groups for phytoplankton etc., and default values.
  set_up_configs(config_file, folder = folder)
  
  # Set PCLake physical settings
  if("PCLake" %in% lst_config[["models"]]){
    export_pclake_physics(config_file, folder = folder,
                          ler_config_file = ler_config_file,
                          verbose = verbose)
  }
  
  # Reads the nutrient inputs - through inflows and additional sources
  export_inputs(config_file, folder = folder, verbose = verbose,
                ler_config_file = ler_config_file)
  
  # Loop through the modules
  for(i in modules){
    
    if(!lst_config[[i]][["use"]]){
      disable_module(config_file = config_file, folder = folder,
                     module = i)
    }else{
      if(!(i %in% c("phytoplankton", "zooplankton", "fish"))){
        input_file_paths <- file.path(folder, lst_config[[i]][["par_file"]])
      }else{
        input_file_paths <- sapply(names(lst_config[[i]][["groups"]]), function(x){
          lst_config[[i]][["groups"]][[x]][["par_file"]]
        })
        input_file_paths <- file.path(folder, input_file_paths)
        names(input_file_paths) <- names(lst_config[[i]][["groups"]])
      }
      
      # Read the file(s)
      for(j in seq_len(length(input_file_paths))){
        input_file <- read.csv(input_file_paths[j], stringsAsFactors = FALSE)
        
        for (x in seq_len(nrow(input_file))){
          set_value_config(config_file = config_file,
                           module = i,
                           group_name = names(input_file_paths)[j],
                           group_position = j,
                           domain = input_file[x, "domain"],
                           process = input_file[x, "process"],
                           subprocess = input_file[x, "subprocess"],
                           model_coupled = input_file[x, "model_coupled"],
                           parameter = input_file[x, "parameter"],
                           value = input_file[x, "value"],
                           folder = folder,
                           verbose = verbose)
        }
        
      }
    }
  }
  
  set_coupling(config_file, folder = folder)

# --- SIMSTRAT-AED2 INFLOW GENERATION ---
  # If Simstrat is a target model, we likely need these files
  if("Simstrat-AED2" %in% lst_config[["models"]]) {
    if(verbose) message("Generating Simstrat-AED2 inflow files...")
    
    # Define paths based on standard LER naming/folder conventions
    aed2_nml <- file.path(folder, "Simstrat-AED2/aed2.nml")
    phyto_nml <- file.path(folder, "Simstrat-AED2/aed2_phyto_pars.nml")
    sim_json <- file.path(folder, "Simstrat-AED2/simstrat.par") # Or the relevant .json config
    
    # Check if files exist before trying to generate inflows
    if(file.exists(aed2_nml) && file.exists(sim_json)) {
       generate_simstrat_aed2_inflows(
         aed2_file = aed2_nml,
         phyto_pars_file = if(file.exists(phyto_nml)) phyto_nml else NULL,
         sim_par = sim_json,
         out_dir = file.path(folder, "Simstrat-AED2"),
         overwrite = overwrite
       )
    }
  }
# --- FINAL STEP: normalize YAML booleans (FABM configs) to yes/no to true/false ---
lst_config2 <- configr::read.config(file.path(folder, config_file))

# Which model config files are YAML?
cfgs <- lst_config2[["config_files"]]
yaml_models <- names(cfgs)[grepl("\\.ya?ml$", cfgs, ignore.case = TRUE)]

for (m in yaml_models) {
  p <- file.path(folder, cfgs[[m]])
  if (file.exists(p)) 
  normalize_yaml_bools(p)
  apply_selma_default_comments(p)
}
}