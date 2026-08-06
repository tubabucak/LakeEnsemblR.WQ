#' @title Update a (possibly group-indexed) value inside an AED2-style namelist
#'
#' @description AED2 namelist parameters like \code{pd\%R_growth} in
#'   \code{aed2_phyto_pars.nml} or \code{zoop_param\%Rgrz_zoo} in
#'   \code{aed2_zoop_pars.nml} are comma-separated arrays, one value per
#'   group (e.g. \code{pd\%R_growth = 2.3, 1.25} for \code{diatoms},
#'   \code{cyanobacteria}). Editing these blindly (e.g. always touching the
#'   first token) silently miscalibrates every group but the first, and can
#'   corrupt the array if multiple group rows for the same parameter are
#'   written in sequence. This locates \code{target_var}'s line within
#'   \code{nml_lines[sec_start:sec_end]}, finds the group's index by matching
#'   \code{group_name} against the section's own name array (e.g.
#'   \code{pd\%p_name = 'diatoms','cyanobacteria'}), and replaces only that
#'   token -- preserving the rest of the array and any trailing inline
#'   comment. Falls back to the first token when \code{group_name} is
#'   \code{NA}/not found (matching the previous single-group behavior).
#'
#' @param nml_lines character vector; full file content from \code{readLines()}.
#' @param sec_start,sec_end integer; 1-based line range of the namelist
#'   section (\code{&section ... /}) to search within.
#' @param target_var character; variable name to match (e.g.
#'   \code{"pd\%R_growth"}), matched at the start of a line up to \code{=}.
#' @param value the replacement value for the matched group's token.
#' @param group_name character or \code{NA}; group to target (e.g.
#'   \code{"diatoms"}). \code{NA} or no match falls back to the first token.
#'
#' @return A list with \code{lines} (the possibly-modified \code{nml_lines})
#'   and \code{found} (logical; whether \code{target_var} was located).
#'
#' @noRd
.update_nml_group_value <- function(nml_lines, sec_start, sec_end, target_var,
                                    value, group_name = NA_character_) {
  section_lines <- nml_lines[sec_start:sec_end]

  group_idx <- NA_integer_
  if (!is.na(group_name) && nzchar(group_name)) {
    name_line_rel <- which(grepl("%[A-Za-z_]*name\\s*=", section_lines, ignore.case = TRUE))
    if (length(name_line_rel) > 0) {
      name_vals <- regmatches(section_lines[name_line_rel[1]],
                              gregexpr("'([^']*)'", section_lines[name_line_rel[1]]))[[1]]
      name_vals <- gsub("'", "", name_vals)
      hit <- which(tolower(trimws(name_vals)) == tolower(trimws(group_name)))
      if (length(hit) > 0) group_idx <- hit[1]
    }
  }

  var_pattern <- paste0("^(\\s*", target_var, "\\s*=\\s*)(.*)$")
  var_idx_rel <- which(grepl(var_pattern, section_lines, ignore.case = TRUE))
  if (length(var_idx_rel) == 0) {
    return(list(lines = nml_lines, found = FALSE))
  }

  actual_line <- sec_start + var_idx_rel[1] - 1
  m <- regmatches(nml_lines[actual_line],
                  regexec(var_pattern, nml_lines[actual_line], ignore.case = TRUE))[[1]]
  prefix <- m[2]
  rest <- m[3]

  comment <- ""
  if (grepl("!", rest, fixed = TRUE)) {
    parts <- strsplit(rest, "!", fixed = TRUE)[[1]]
    rest <- parts[1]
    comment <- paste0(" !", paste(parts[-1], collapse = "!"))
  }

  tokens <- strsplit(rest, ",", fixed = TRUE)[[1]]
  idx_to_replace <- if (!is.na(group_idx) && group_idx <= length(tokens)) group_idx else 1L
  tokens[idx_to_replace] <- paste0(" ", value)
  new_rest <- paste(trimws(tokens), collapse = ", ")

  nml_lines[actual_line] <- paste0(prefix, new_rest, comment)
  list(lines = nml_lines, found = TRUE)
}

#' @title Derive a model config filename from a LakeEnsemblR config
#'
#' @description Looks up \code{phys_model} (e.g. \code{"GOTM"},
#'   \code{"Simstrat"}) in the \code{config_files} section of a
#'   \code{LakeEnsemblR.yaml}-style config, case-insensitively, and returns
#'   the basename of the matched path (e.g. \code{"gotm.yaml"}). Returns
#'   \code{NULL} if \code{ler_config_file} is \code{NULL}, the file can't be
#'   read, or no matching entry is found -- callers should fall back to their
#'   own hardcoded default in that case.
#'
#' @param ler_config_file character or \code{NULL}; path to the LakeEnsemblR
#'   config file. If relative, resolved against \code{base_dir}.
#' @param phys_model character; physical model key to look up (e.g.
#'   \code{"GOTM"}, \code{"Simstrat"}).
#' @param base_dir character; directory used to resolve \code{ler_config_file}
#'   when it is a relative path. Defaults to \code{"."}.
#'
#' @noRd
.derive_ler_config_filename <- function(ler_config_file, phys_model, base_dir = ".") {
  if (is.null(ler_config_file) || !nzchar(ler_config_file)) return(NULL)

  ler_path <- if (grepl("^([A-Za-z]:|/)", ler_config_file)) {
    ler_config_file
  } else {
    file.path(base_dir, ler_config_file)
  }
  if (!file.exists(ler_path)) return(NULL)

  ler_cfg <- tryCatch(yaml::read_yaml(ler_path), error = function(e) NULL)
  cfg_files <- ler_cfg[["config_files"]]
  if (is.null(cfg_files) || length(cfg_files) == 0) return(NULL)

  key <- names(cfg_files)[toupper(names(cfg_files)) == toupper(phys_model)][1]
  if (is.na(key)) return(NULL)

  path_val <- cfg_files[[key]]
  if (is.null(path_val) || !nzchar(path_val)) return(NULL)

  basename(path_val)
}

#' @title Adds an AED2 section to the Simstrat config file
#'
#' @description Checks for existence of and then adds a AED2Config section
#'  in the Simstrat configuration file (JSON format). Takes into account
#'  information in LER.WQ config file, e.g. on shading. 
#'
#' @param folder path; to the location of the config files
#' @param simstrat_par character; name of the Simstrat config file
#' @param verbose logical; whether to show messages
#' @param settings_section list; corresponding section from LER.WQ config
#' 
#' @importFrom LakeEnsemblR get_yaml_multiple input_json
#' 
#' @noRd
add_aed2_section_simstrat <- function(folder = ".",
                                      simstrat_par = "simstrat.par",
                                      verbose = TRUE,
                                      settings_section = NULL){
  # This function will interpret a commented-out AED2Config as present
  # and not create a new section. 
  
  # configr was not able to read sim_par. Non-conformity to the Simstrat-
  # format as present in e.g. SimstratR, might lead to errors. This is not a
  # json-parser.
  sim_par <- readLines(file.path(folder, simstrat_par))
  
  if(is.null(settings_section)){
    stop("settings_section must be provided to add aed2 section to Simstrat")
  }
  shading <- ifelse(settings_section[["bio-shading"]], 1, 0)
  benthic <- ifelse(settings_section[["bottom_everywhere"]], 1, 0)
  
  aed_section_present <- any(grepl("AED2Config", sim_par))
  if(aed_section_present){
    input_json(file.path(folder, simstrat_par), label = "AED2Config",
               key = "BioshadeFeedback", value = shading)
    input_json(file.path(folder, simstrat_par), label = "AED2Config",
               key = "BenthicMode", value = benthic)
    
    return()
  }
  
  # Grab settings and information, to be used in writing the aed2config section
  num_spaces <- attr(regexpr("\\s+", sim_par[2]), "match.length")
  s1 <- paste0(rep(" ", num_spaces), collapse = "")
  s2 <- paste0(rep(" ", num_spaces * 2), collapse = "")
  folder_simstrat <- dirname(simstrat_par)
  aed_nml <- "aed2.nml"
  
  
  ### Create the AED2Config section
  aed2config <- c(paste0(s1, "\"AED2Config\" : {"),
                  paste0(s2, "\"AED2ConfigFile\" :  \"", aed_nml, "\","),
                  paste0(s2, "\"PathAED2initial\" :  \"","\","),
                  paste0(s2, "\"PathAED2inflow\" :  \"","\","),
                  paste0(s2, "\"ParticleMobility\" : 0,"),
                  paste0(s2, "\"BioshadeFeedback\" : ", shading, ","),
                  paste0(s2, "\"BackgroundExtinction\" : 0.2,"),
                  paste0(s2, "\"BenthicMode\" : ", benthic,","),
                  paste0(s2, "\"OutputDiagnosticVars\" : false,"),
                  paste0(s1, "},"))
  
  ### Add AED2Config after ModelConfig
  ind_modelconfig <- grep("ModelConfig", sim_par)
  for(i in ind_modelconfig:length(sim_par)){
    if(grepl("},", sim_par[i])){
      ind_modelconfig_end <- i
      break
    }
    if(i == length(sim_par)){
      stop("Could not find end of ModelConfig section in sim_par!")
    }
  }
  
  ### Write file
  writeLines(text = c(sim_par[1:ind_modelconfig_end],
                      aed2config,
                      sim_par[(ind_modelconfig_end + 1):length(sim_par)]),
             con = file.path(folder, simstrat_par))
  
  
}

#' @title Modifies FABM section in gotm.yaml
#'
#' @description Activates WQ settings in the gotm.yaml file,
#'  and adds a numerics section if not present. 
#'
#' @param folder path; to the location of the config files
#' @param gotmyaml character; name of the Simstrat config file
#' @param verbose logical; whether to show messages
#' @param settings_section list; corresponding section from LER.WQ config
#' 
#' @importFrom LakeEnsemblR get_yaml_multiple input_yaml_multiple
#' 
#' @noRd
add_fabm_settings_gotm <- function(folder = ".",
                                   gotmyaml = "gotm.yaml",
                                   verbose = TRUE,
                                   settings_section = NULL){
  
  bottom <- tolower(as.character(settings_section[["bottom_everywhere"]]))
  shading <- tolower(as.character(settings_section[["bio-shading"]]))
  split <- settings_section[["split_factor"]]
  repair <- tolower(as.character(settings_section[["repair_state"]]))
  
  ode_method <- settings_section[["ode_method"]]
  valid_ode <- c("Euler", "RK2", "RK4", "Pat1", "PatRK2", "PatRK4", "ModPat1",
                 "ModPatRK2", "ModPatRK4", "ExtModPat1", "ExtModPatRK2")
  
  if(!(ode_method %in% valid_ode)){
    stop(ode_method, " is not a valid entry for GOTM!")
  }else{
    ode_num <- which(valid_ode == ode_method)
  }
  
  numerics_section_present <- tryCatch(get_yaml_multiple(file.path(folder,
                                                                   gotmyaml),
                                                         key1 = "fabm",
                                                         key2 = "numerics",
                                                         key3 = "ode_method"),
                                       error = function(e){FALSE})
  
  if(isFALSE(numerics_section_present)){
    # configr can read the yaml file, but here readLines is used to
    # conserve comments if present.
    yml <- readLines(file.path(folder, gotmyaml))
    
    num_spaces <- attr(regexpr("\\s+", yml[3]), "match.length")
    s1 <- paste0(rep(" ", num_spaces), collapse = "")
    s2 <- paste0(rep(" ", num_spaces * 2), collapse = "")
    
    numerics_section <- c(paste0(s1, "numerics:"),
                          paste0(s2, "ode_method: 1"),
                          paste0(s2, "split_factor: 1"))
    
    # Add after repair_state line
    ind_repairstate <- grep("repair_state:", yml)
    
    writeLines(text = c(yml[1:ind_repairstate],
                        numerics_section,
                        yml[(ind_repairstate + 1):length(yml)]),
               con = file.path(folder, gotmyaml))
  }
  
  # Now enter the values
  input_yaml_multiple(file.path(folder, gotmyaml),
                      bottom,
                      key1 = "fabm", key2 = "feedbacks",
                      key3 = "bottom_everywhere", verbose = verbose)
  input_yaml_multiple(file.path(folder, gotmyaml),
                      shading,
                      key1 = "fabm", key2 = "feedbacks",
                      key3 = "shade", verbose = verbose)
  input_yaml_multiple(file.path(folder, gotmyaml),
                      repair,
                      key1 = "fabm", key2 = "repair_state", verbose = verbose)
  input_yaml_multiple(file.path(folder, gotmyaml),
                      ode_num,
                      key1 = "fabm", key2 = "numerics",
                      key3 = "ode_method", verbose = verbose)
  input_yaml_multiple(file.path(folder, gotmyaml),
                      split,
                      key1 = "fabm", key2 = "numerics",
                      key3 = "split_factor", verbose = verbose)
}


#' @title Get the phytoplankton group to be used in MyLake
#'
#' @description MyLake only uses one phytoplankton group, so it is needed
#'  to determine one of the groups used in the config_file to be the group
#'  used in MyLake. By default the 1st group, if nothing is specified.  
#'
#' @param config_file character; name of the config file
#' @param module character; name of the module
#' @param folder path; to the location of the config file
#' 
#' @importFrom configr read.config
#' 
#' @noRd

get_mylake_group <- function(config_file, module, folder = "."){
  
  if(module != "phytoplankton"){
    stop("The get_mylake_group function only works for phytoplankton!")
  }
  
  lst_config <- read.config(file.path(folder, config_file))
  if(!lst_config[[module]][["use"]]){
    return("")
  }
  
  groups <- names(lst_config[[module]][["groups"]])
  
  # See if a group has been specified with "mylake_group: true"
  use_mylake <- lapply(lst_config[[module]][["groups"]],
                              "[[",
                              "mylake_group")
  use_mylake <- sapply(use_mylake, function(x) ifelse(is.null(x), FALSE, x))
  
  if(class(use_mylake) != "logical"){
    stop("An entry of mylake_group in the config_file is not 'true' or 'false'")
  }
  
  if(sum(use_mylake) > 1L){
    stop("Multiple phytoplankton groups are marked to be used in MyLake!")
  }else if(sum(use_mylake) == 0L){
    return(groups[1L])
  }else{
    return(groups[use_mylake])
  }
}


#' @title Get the groups to be used in PCLake
#'
#' @description PCLake has a fixed number of groups for phytoplankton,
#'  zooplankton, macrophytes, and fish. Therefore it is needed to determine
#'  which groups in the config_file belong to which PCLake group. 
#'  This can either be specified in the config_file, or this function
#'  tries to deduce it from the group names.  
#'
#' @param config_file character; name of the config file
#' @param module character; name of the module
#' @param folder path; to the location of the config file
#' @param auto_recognisition logical; in absence of user input, try to
#'  identify groups by their names?
#' 
#' @importFrom configr read.config
#' 
#' @noRd

get_pclake_groups <- function(config_file, module, folder = ".",
                              auto_recognisition = TRUE){
  
  lst_config <- read.config(file.path(folder, config_file))
  if(!lst_config[[module]][["use"]]){
    return("")
  }
  
  groups <- names(lst_config[[module]][["groups"]])
  
  # See if groups have been specified with "pclake_group"
  pclake_groups <- lapply(lst_config[[module]][["groups"]],
                       "[[",
                       "pclake_group")
  pclake_groups <- sapply(pclake_groups, function(x) ifelse(is.null(x),
                                                            "", x))
  pclake_groups <- tolower(pclake_groups)
  
  # Define what standard_groups PCLake uses and the pattern to search for them
  # If there's only one group, no pattern is needed
  if(module == "phytoplankton"){
    standard_groups <- c(Blue = "cyano|blue",
                         Gren = "(green|gren|chloro)^blue", # No "blue", to avoid detecting "bluegreen"
                         Diat = "diat")
  }else if(module == "zooplankton"){
    standard_groups <- "Zoo"
  }else if(module == "zoobenthos"){
    standard_groups <- "Bent"
  }else if(module == "fish"){
    standard_groups <- c(FiAd = "ad|benthiv",
                         FiJv = "jv|juv",
                         Pisc = "pisc|pred")
  }else if(module == "macrophytes"){
    standard_groups <- c(Veg = "plant|phyt",
                         Phra = "phrag|reed")
  }
  
  group_division <- rep(as.character(NA), length(groups))
  names(group_division) <- groups
  
  for(i in seq_len(length(pclake_groups))){
    rgx <- sapply(standard_groups, function(x) regexpr(x, pclake_groups[i]))
    if(sum(rgx > 0L) > 1L){
      stop("pclake_group user input identified same group multiple times. ",
           "Maximum one group of ", paste(names(standard_groups),
                                          collapse = ", "))
    }else if(sum(rgx > 0L) == 1L){
      if(!is.na(group_division[i])){
        stop(names(group_division)[i], " is identified double by pclake_group",
             " user input")
      }
      group_division[i] <- names(rgx)[rgx > 0L]
    }else if(pclake_groups[i] == "true" & length(standard_groups) == 1L){
      group_division[i] <- names(rgx)
    }
  }
  
  # Now loop over group_division again to recognise names
  if(auto_recognisition){
    if(length(standard_groups) == 1L & all(is.na(group_division))){
      # If there is only one group, just take the first group
      message("Autorecognition PCLake: identifying ",
              names(group_division)[1L], " as ",
              standard_groups, ".")
      group_division[1L] <- standard_groups
      
    }else{
      for(i in seq_len(length(group_division))){
        if(!is.na(group_division[i])) next
        
        rgx <- sapply(standard_groups,
                      function(x) regexpr(x, names(group_division)[i]))
        # Instead of throwing an error, the first hit is used
        # e.g. if someone makes groups diatoms1 and diatoms2, diatoms1 is used
        ind <- which(rgx > 0L)[1L]
        if(!is.na(ind)){
          message("Autorecognition PCLake: identifying ",
                  names(group_division)[i], " as ",
                  names(rgx)[ind], ".")
          group_division[i] <- names(rgx)[ind]
        }
      }
    }
  }
  
  return(group_division)
}

#' check naming convention for inflow nutrients
#'@description
#'check if the header in in files follow the naming convention
#'
#' @name chk_names_nutr_flow
#' @param headers vector of column headers
#' @noRd
chk_names_nutr_flow <- function(headers){
  
  # remove numbers if multiple in/outflows are there
  headers <- gsub("_\\d+$", "", headers)
  
  allowed_names <- c("datetime", wq_var_dic$standard_name)
  if(isTRUE(requireNamespace("LakeEnsemblR", quietly = TRUE))){
    ler_dic_names <- LakeEnsemblR::lake_var_dic$standard_name
    ler_dic_names <- ler_dic_names[!(ler_dic_names %in% c("Ice_Thickness_meter",
                                                          "Density_kiloGramPerCubedMeter",
                                                          "Water_Level_meter"))]
    allowed_names <- c(allowed_names, ler_dic_names)
  }
  
  # test if names are right
  chck_flow <- sapply(headers, function(x) x %in% allowed_names)
  if(any(!chck_flow)){
    stop("The following headers of the inflow nutrients files are not correct: ",
         headers[!chck_flow], "! They should be one of:\n",
         paste(allowed_names, collapse = "\n"))
  }
}

#'write yaml file in list-format
#'@description
#'write yaml file in GOTM yaml format
#'
#' @name lerwq_write_yaml_file
#' @param yml list; yaml file in list format, as read by configr
#' @param filepath character; path to file location
#' @param is_gotm_yaml logical; if unspecified, it try to detect gotm.yaml
#' @noRd
lerwq_write_yaml_file <- function(yml, filepath, is_gotm_yaml = NULL){
  # Method is very cumbersome, hence the separate function
  
  write.config(yml,
               filepath,
               write.type = "yaml",
               indent = 3L,
               handlers = list(logical = function(x){
                 result = ifelse(x, "true", "false")
                 class(result) = "verbatim"
                 return(result)
               },
               NULL = function(x){
                 result = ""
                 class(result) = "verbatim"
                 return(result)
               }))
  
  # Only for gotm.yaml:
  # The function writes two spaces between "-" and "source", and this should be one
  # GOTM will crash if this doesn't happen
  if(is.null(is_gotm_yaml)){
    if(all(c("title", "location", "time") %in% names(yml))){
      is_gotm_yaml <- TRUE
    }else{
      is_gotm_yaml <- FALSE
    }
  }
  
  if(is_gotm_yaml){
    yml_txt <- readLines(con = filepath)
    the_lines <- grep("-  source:", yml_txt)
    
    for(i in the_lines){
      yml_txt[i] <- gsub("-  source:", "- source:",
                         yml_txt[i])
    }
    
    writeLines(yml_txt, con = filepath)
  }
}


#'Sets a value in a PCLake par data.frame
#'@description
#'Sets a value in a PCLake parameter or initial states file
#' that has been read into R as a data.frame
#'
#'@param file data.frame; 
#'@param par_list list; parameter names without underscores and corresponding
#' value to enter
#'@param column character; column name to change in file. defaults to sSet1
#'@param verbose logical; print changed parameters to screen
#'
#' @keywords internal


set_pclake_r <- function(file, par_list,
                         column = "sSet1", verbose = FALSE){
  
  for(i in names(par_list)){
    ind <- which(file[["sName"]] == paste0("_", i, "_"))
    
    if(length(ind) == 0L){
      stop("Could not find parameter ", i, " in pclake par file!")
    }else if(length(ind) > 1L){
      stop("Parameter ", i, " found multiple times in pclake par file!")
    }
    
    old_val <- file[ind, column]
    file[ind, column] <- par_list[[i]]
    
    if(verbose & !identical(old_val, par_list[[i]])){
      message("PCLake: replaced ", i, ": ", old_val, " by ", par_list[[i]])
    }
  }
  
  return(file)
}
add_selma_prey_to_scaffold <- function(wq_config, lst_config, zoo_instance = "zooplankton") {

  # ---- Find SELMA zooplankton instance key ------
  zoo_inst_keys <- names(Filter(
    function(inst) identical(inst[["model"]], "selmaprotbas/zooplankton"),
    wq_config[["instances"]]
  ))
  if (length(zoo_inst_keys) == 0) return(wq_config)

  zoo_key <- if (!is.null(zoo_instance) && zoo_instance %in% zoo_inst_keys) {
    zoo_instance
  } else {
    zoo_inst_keys[1]
  }

  # ---- Get prey list from master config  ----
  zoo_groups <- lst_config[["zooplankton"]][["groups"]]
  if (is.null(zoo_groups) || length(zoo_groups) == 0) return(wq_config)

  # Try to use a group with same name as the SELMA instance key, otherwise first group
  if (!is.null(zoo_groups[[zoo_key]][["prey"]])) {
    prey_paths <- zoo_groups[[zoo_key]][["prey"]]
  } else if (!is.null(zoo_groups[[zoo_instance]][["prey"]])) {
    prey_paths <- zoo_groups[[zoo_instance]][["prey"]]
  } else {
    prey_paths <- zoo_groups[[1]][["prey"]]
  }

  if (is.null(prey_paths) || length(prey_paths) == 0) return(wq_config)

  prey_groups <- tolower(sub("^.*/", "", prey_paths))  # "phytoplankton/diatoms" -> "diatoms"

  # ---- Resolve to phyto instance keys that exist in SELMA config ----
  phy_inst_keys <- names(Filter(
    function(inst) identical(inst[["model"]], "selmaprotbas/phytoplankton"),
    wq_config[["instances"]]
  ))

  resolved <- phy_inst_keys[tolower(phy_inst_keys) %in% prey_groups]
  if (length(resolved) == 0) return(wq_config)

  # ---- Ensure coupling exists and write prey1..preyN as '<instance>/c' ----
  if (is.null(wq_config[["instances"]][[zoo_key]][["coupling"]])) {
    wq_config[["instances"]][[zoo_key]][["coupling"]] <- list()
  }

  for (k in seq_along(resolved)) {
    wq_config[["instances"]][[zoo_key]][["coupling"]][[paste0("prey", k)]] <- resolved[k]
  }

  # ---- Keep nprey consistent ----
  if (is.null(wq_config[["instances"]][[zoo_key]][["parameters"]])) {
    wq_config[["instances"]][[zoo_key]][["parameters"]] <- list()
  }
  wq_config[["instances"]][[zoo_key]][["parameters"]][["nprey"]] <- length(resolved)

  wq_config
}



add_wet_prey_to_scaffold <- function(wq_config, lst_config, zoo_instance = "zooplankton") {

  # ---- Find SELMA zooplankton instance key ------
  zoo_inst_keys <- names(Filter(
    function(inst) identical(inst[["model"]], "wet/zooplankton"),
    wq_config[["instances"]]
  ))
  if (length(zoo_inst_keys) == 0) return(wq_config)

  zoo_key <- if (!is.null(zoo_instance) && zoo_instance %in% zoo_inst_keys) {
    zoo_instance
  } else {
    zoo_inst_keys[1]
  }

  # ---- Get prey list from master config  ----
  zoo_groups <- lst_config[["zooplankton"]][["groups"]]
  if (is.null(zoo_groups) || length(zoo_groups) == 0) return(wq_config)

  # Try to use a group with same name as the WET instance key, otherwise first group
  if (!is.null(zoo_groups[[zoo_key]][["prey"]])) {
    prey_paths <- zoo_groups[[zoo_key]][["prey"]]
  } else if (!is.null(zoo_groups[[zoo_instance]][["prey"]])) {
    prey_paths <- zoo_groups[[zoo_instance]][["prey"]]
  } else {
    prey_paths <- zoo_groups[[1]][["prey"]]
  }

  if (is.null(prey_paths) || length(prey_paths) == 0) return(wq_config)

  prey_groups <- tolower(sub("^.*/", "", prey_paths))  # "phytoplankton/diatoms" -> "diatoms"

  # ---- Resolve to phyto instance keys that exist in SELMA config ----
  phy_inst_keys <- names(Filter(
    function(inst) identical(inst[["model"]], "wet/phytoplankton"),
    wq_config[["instances"]]
  ))

  resolved <- phy_inst_keys[tolower(phy_inst_keys) %in% prey_groups]
  if (length(resolved) == 0) return(wq_config)

  # ---- Ensure coupling exists and write prey1..preyN as '<instance>/c' ----
  if (is.null(wq_config[["instances"]][[zoo_key]][["coupling"]])) {
    wq_config[["instances"]][[zoo_key]][["coupling"]] <- list()
  }

  for (k in seq_along(resolved)) {
    wq_config[["instances"]][[zoo_key]][["coupling"]][[paste0("prey_model", k)]] <- paste0(resolved[k])
  }

  # ---- Keep nprey consistent ----
  if (is.null(wq_config[["instances"]][[zoo_key]][["parameters"]])) {
    wq_config[["instances"]][[zoo_key]][["parameters"]] <- list()
  }
  wq_config[["instances"]][[zoo_key]][["parameters"]][["nPrey"]] <- length(resolved)

  wq_config
}



normalize_yaml_bools <- function(filepath) {
  x <- readLines(filepath, warn = FALSE)

  # mapping scalars: key: yes/no/on/off (quoted or unquoted)
  x <- gsub('(^\\s*[^#]+?:\\s*)"(yes|no|on|off)"(\\s*(#.*)?$)', '\\1\\2\\3', x, ignore.case = TRUE)
  x <- gsub("(^\\s*[^#]+?:\\s*)(yes|on)(\\s*(#.*)?$)", "\\1true\\3", x, ignore.case = TRUE)
  x <- gsub("(^\\s*[^#]+?:\\s*)(no|off)(\\s*(#.*)?$)",  "\\1false\\3", x, ignore.case = TRUE)

  # sequence scalars: - yes/no/on/off (quoted or unquoted)
  x <- gsub('(^\\s*-\\s*)"(yes|no|on|off)"(\\s*(#.*)?$)', '\\1\\2\\3', x, ignore.case = TRUE)
  x <- gsub("(^\\s*-\\s*)(yes|on)(\\s*(#.*)?$)", "\\1true\\3", x, ignore.case = TRUE)
  x <- gsub("(^\\s*-\\s*)(no|off)(\\s*(#.*)?$)",  "\\1false\\3", x, ignore.case = TRUE)

  writeLines(x, filepath, useBytes = TRUE)
  invisible(TRUE)
}


comment_out_yaml_parameter <- function(filepath, param_names) {
  lines <- readLines(filepath, warn = FALSE)

  for (p in param_names) {
    # match lines like:   alpha_light: 0.1
    pattern <- paste0("^([[:space:]]*)(", p, ":[[:space:]].*)$")
    lines <- gsub(pattern, "\\1# \\2", lines)
  }

  writeLines(lines, filepath, useBytes = TRUE)
  invisible(TRUE)
}


apply_selma_default_comments <- function(filepath) {

  # Comment these parameters globally in SELMA phyto instances
  comment_out_yaml_parameter(filepath, c("tll", "imin", "tau_crit", "beta", "alpha"))

}

# Dynamic phytoplankton input
expand_templates <- function(sel_metric, wq_config_file) {

  cfg <- yaml::read_yaml(wq_config_file)

  # get phyto groups
  phyto_groups <- character()
  if (!is.null(cfg$phytoplankton$groups)) {
    phyto_groups <- names(cfg$phytoplankton$groups)
  }

  # get zoo count (for GLM-style zoo)
  zoo_n <- NA_integer_
  if (!is.null(cfg$zooplankton$groups)) {
    zoo_n <- length(cfg$zooplankton$groups)
  }

  out <- list()

  for (i in seq_len(nrow(sel_metric))) {

    row <- sel_metric[i, , drop = FALSE]
    varname <- row$variable_model_name

    # ---- expand {group} ----
    if (grepl("\\{group\\}", varname)) {

      for (g in phyto_groups) {
        rr <- row
        rr$variable_model_name <- gsub("\\{group\\}", g, varname)
        out[[length(out) + 1]] <- rr
      }
      next
    }

    # ---- expand zoo index ----
    if (grepl("\\{idx:02d\\}", varname) && !is.na(zoo_n)) {

      for (k in seq_len(zoo_n)) {
        rr <- row
        rr$variable_model_name <- gsub("\\{idx:02d\\}",
                                       sprintf("%02d", k),
                                       varname)
        out[[length(out) + 1]] <- rr
      }
      next
    }

    # ---- static row ----
    out[[length(out) + 1]] <- row
  }

  dplyr::bind_rows(out)
}