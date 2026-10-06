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

#' @title Write an AED2 parameter given as a dictionary path
#'
#' @description Writes \code{value} for a GLM-AED/Simstrat-AED2 parameter whose
#'   \code{calib_setup$file} entry is a dictionary path such as
#'   \code{"aed2_oxygen/Fsed_oxy"} (namelist section / variable) rather than a
#'   file name. Resolves the target namelist from \code{wq_config_file}'s
#'   \code{config_files} entry for \code{model} (the phyto-/zooplankton file
#'   from \code{.aed_par_file()} for those modules), or
#'   falls back to the standard file names in \code{current_dir} when
#'   \code{wq_config_file} is not supplied. Shared by \code{calib_wq()},
#'   \code{run_sensitivity()} and \code{run_multi_param_sensitivity()}.
#'
#' @param file_or_path character; the dictionary path from \code{calib_setup$file}.
#' @param p character; parameter name (used when the path has no variable part).
#' @param value the value to write.
#' @param current_dir character; the model directory being modified.
#' @param model character; coupled model name (\code{"GLM-AED"} or
#'   \code{"Simstrat-AED2"}).
#' @param wq_config_file character or \code{NULL}; LakeEnsemblR_WQ config file.
#' @param module character or \code{NA}; the parameter's module.
#' @param group_name character or \code{NA}; group to target within the namelist.
#'
#' @return Invisibly, \code{TRUE} if the parameter was found and written,
#'   \code{FALSE} otherwise.
#'
#' @noRd
.write_aed2_dict_param <- function(file_or_path, p, value, current_dir, model,
                                   wq_config_file = NULL, module = NA_character_,
                                   group_name = NA_character_) {
  model_upper <- toupper(model)
  model_cfg <- NULL

  if (!is.null(wq_config_file) && nzchar(wq_config_file)) {
    # wq_config_file may live directly in current_dir (DE-worker sandbox,
    # where root *.yaml files are copied alongside the model folder) or one
    # level up in the real project folder (plain calls, where current_dir is
    # model_dir itself). Try both, plus the path as given.
    wq_yaml_candidates <- c(
      file.path(current_dir, basename(wq_config_file)),
      wq_config_file,
      file.path(dirname(current_dir), basename(wq_config_file))
    )
    wq_yaml_candidates <- unique(normalizePath(wq_yaml_candidates, winslash = "/", mustWork = FALSE))
    wq_yaml <- wq_yaml_candidates[file.exists(wq_yaml_candidates)][1]
    if (is.na(wq_yaml) || !nzchar(wq_yaml)) {
      stop("Could not find wq_config_file '", wq_config_file, "' (looked in ",
           current_dir, " and ", dirname(current_dir), ")")
    }
    cfg_files <- configr::read.config(wq_yaml)[["config_files"]]
    model_cfg <- cfg_files[[model]]
    if (is.null(model_cfg) || !nzchar(model_cfg)) {
      cfg_idx <- which(toupper(names(cfg_files)) == model_upper)[1]
      if (!is.na(cfg_idx)) model_cfg <- cfg_files[[cfg_idx]]
    }
  }
  if (is.null(model_cfg) || !nzchar(model_cfg)) {
    # No wq_config_file: for GLM take the AED file GLM itself is set up to
    # read (wq_setup/wq_nml_file, e.g. 'aed.nml'), else the standard name
    model_cfg <- "aed2.nml"
    if (model_upper == "GLM-AED") {
      glm_nml <- tryCatch(file.path(current_dir, .glm_nml_file(current_dir)),
                          error = function(e) NA_character_)
      if (!is.na(glm_nml)) {
        hit <- grep("^\\s*wq_nml_file\\s*=", readLines(glm_nml, warn = FALSE), value = TRUE)
        wq_nml <- gsub("^[^=]*=\\s*|['\"[:space:]]", "", sub("!.*$", "", hit[1]))
        if (!is.na(wq_nml) && nzchar(wq_nml)) model_cfg <- wq_nml
      }
    }
  }

  if (model_upper == "SIMSTRAT-AED2" && grepl("\\.par$", model_cfg, ignore.case = TRUE)) {
    model_cfg <- file.path(dirname(model_cfg), "aed2.nml")
  }

  if (isTRUE(module %in% c("phytoplankton", "zooplankton"))) {
    model_cfg <- file.path(dirname(model_cfg),
                           .aed_par_file(model, module))
  }

  nml_candidates <- c(file.path(current_dir, model_cfg), file.path(current_dir, basename(model_cfg)))
  nml_candidates <- unique(normalizePath(nml_candidates, winslash = "/", mustWork = FALSE))
  nml_path <- nml_candidates[file.exists(nml_candidates)][1]
  if (is.na(nml_path) || !nzchar(nml_path)) stop("NML file not found for '", file_or_path, "'.")

  path_parts <- strsplit(file_or_path, "/", fixed = TRUE)[[1]]
  nml_lines <- readLines(nml_path, warn = FALSE)
  target_var <- if (length(path_parts) == 2L) trimws(path_parts[2]) else p

  # GLM 4 files use AED 3 names (aed_oxygen, kanammox); see .find_nml_section()
  sec <- .find_nml_section(nml_lines, path_parts[1])
  if (is.null(sec)) return(invisible(FALSE))
  if (sec$aed3) target_var <- .aed3_name(target_var)

  upd <- .update_nml_group_value(nml_lines, sec$start, sec$end, target_var,
                                 value, group_name = group_name)
  if (isTRUE(upd$found)) writeLines(upd$lines, nml_path)
  invisible(isTRUE(upd$found))
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
    if(any(grepl("OutputDiagnosticVars", sim_par))){
      input_json(file.path(folder, simstrat_par), label = "AED2Config",
                 key = "OutputDiagnosticVars", value = "true")
    }
    # Self-heal an AED2Config section written before this fix (blank, or
    # separator-less ".", PathAED2inflow -- see the comment below for why
    # "./" is required). input_json() substitutes 'value' verbatim (no JSON
    # quoting of its own -- see its source), so the literal quotes have to
    # be supplied here.
    if(any(grepl("\"PathAED2inflow\"\\s*:\\s*\"\\.?\"", sim_par))){
      input_json(file.path(folder, simstrat_par), label = "AED2Config",
                 key = "PathAED2inflow", value = "\"./\"")
    }

    return()
  }
  
  # Grab settings and information, to be used in writing the aed2config section
  num_spaces <- attr(regexpr("\\s+", sim_par[2]), "match.length")
  s1 <- paste0(rep(" ", num_spaces), collapse = "")
  s2 <- paste0(rep(" ", num_spaces * 2), collapse = "")
  folder_simstrat <- dirname(simstrat_par)
  aed_nml <- "aed2.nml"
  
  
  ### Create the AED2Config section
  # PathAED2inflow: generate_simstrat_aed2_inflows() (see export_config.R)
  # always writes the AED2 *_inflow.dat files into the same folder as
  # simstrat.par itself. Simstrat's Fortran side builds the file path by
  # plain string concatenation (trim(PathAED2inflow) // filename, no
  # separator inserted) -- so the path here MUST end in a slash. "" silently
  # skips loading any AED2 inflow file (no error, but no external WQ/
  # nutrient loading either); "." (no trailing slash) produces a broken
  # concatenated filename like ".NCS_ss1_inflow.dat" and crashes with a
  # Fortran "No such file or directory" error. "./" is the only form that
  # works.
  # PathAED2initial is left blank: nothing in this package currently
  # generates a separate AED2 initial-conditions file for Simstrat (initial
  # values come from aed2.nml itself).
  aed2config <- c(paste0(s1, "\"AED2Config\" : {"),
                  paste0(s2, "\"AED2ConfigFile\" :  \"", aed_nml, "\","),
                  paste0(s2, "\"PathAED2initial\" :  \"","\","),
                  paste0(s2, "\"PathAED2inflow\" :  \"./\","),
                  paste0(s2, "\"ParticleMobility\" : 0,"),
                  paste0(s2, "\"BioshadeFeedback\" : ", shading, ","),
                  paste0(s2, "\"BackgroundExtinction\" : 0.2,"),
                  paste0(s2, "\"BenthicMode\" : ", benthic,","),
                  paste0(s2, "\"OutputDiagnosticVars\" : true,"),
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
  
  if(!is.logical(use_mylake)){
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

  # ---- Find every SELMA zooplankton instance (one per configured group) ----
  zoo_inst_keys <- names(Filter(
    function(inst) identical(inst[["model"]], "selmaprotbas/zooplankton"),
    wq_config[["instances"]]
  ))
  if (length(zoo_inst_keys) == 0) return(wq_config)

  zoo_groups <- lst_config[["zooplankton"]][["groups"]]
  if (is.null(zoo_groups) || length(zoo_groups) == 0) return(wq_config)

  # ---- Resolve to phyto instance keys that exist in SELMA config ----
  phy_inst_keys <- names(Filter(
    function(inst) identical(inst[["model"]], "selmaprotbas/phytoplankton"),
    wq_config[["instances"]]
  ))

  # Every zooplankton instance needs its own prey coupling written -
  # looking up only the first one silently left the rest of a
  # multi-group setup without any prey1..preyN coupling at all.
  for (zoo_key in zoo_inst_keys) {

    # Prey list from master config, matched by group/instance name
    prey_paths <- zoo_groups[[zoo_key]][["prey"]]
    if (is.null(prey_paths) || length(prey_paths) == 0) next

    prey_groups <- tolower(sub("^.*/", "", prey_paths))  # "phytoplankton/diatoms" -> "diatoms"

    resolved <- phy_inst_keys[tolower(phy_inst_keys) %in% prey_groups]
    if (length(resolved) == 0) next

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
  }

  wq_config
}



add_wet_prey_to_scaffold <- function(wq_config, lst_config, zoo_instance = "zooplankton") {

  # ---- Find every WET zooplankton instance (one per configured group) ----
  zoo_inst_keys <- names(Filter(
    function(inst) identical(inst[["model"]], "wet/zooplankton"),
    wq_config[["instances"]]
  ))
  if (length(zoo_inst_keys) == 0) return(wq_config)

  zoo_groups <- lst_config[["zooplankton"]][["groups"]]
  if (is.null(zoo_groups) || length(zoo_groups) == 0) return(wq_config)

  # ---- Resolve to phyto instance keys that exist in WET config ----
  phy_inst_keys <- names(Filter(
    function(inst) identical(inst[["model"]], "wet/phytoplankton"),
    wq_config[["instances"]]
  ))

  # Every zooplankton instance needs its own prey coupling written -
  # looking up only the first one silently left the rest of a
  # multi-group setup without any prey_model1..preyN coupling at all.
  for (zoo_key in zoo_inst_keys) {

    # Prey list from master config, matched by group/instance name
    prey_paths <- zoo_groups[[zoo_key]][["prey"]]
    if (is.null(prey_paths) || length(prey_paths) == 0) next

    prey_groups <- tolower(sub("^.*/", "", prey_paths))  # "phytoplankton/diatoms" -> "diatoms"

    resolved <- phy_inst_keys[tolower(phy_inst_keys) %in% prey_groups]
    if (length(resolved) == 0) next

    # ---- Ensure coupling exists and write prey_model1..preyN as '<instance>/c' ----
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
  }

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

    # ---- expand zoo group name (GLM 4 / AED 3, e.g. ZOO_daphnia) ----
    if (grepl("\\{zoo_group\\}", varname) && !is.na(zoo_n)) {

      for (g in names(cfg$zooplankton$groups)) {
        rr <- row
        rr$variable_model_name <- gsub("\\{zoo_group\\}", g, varname)
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

#' @title Quote a text array for glmtools::write_nml()
#'
#' @description write_nml() quotes a single text value but not an array, so
#'   e.g. two group names would be written as \code{NAME,NAME}, which
#'   read_nml() then tries to read as numbers ("NAs introduced by coercion").
#'   Arrays of text are quoted here; single values and numbers are returned
#'   unchanged (write_nml() quotes a single value itself).
#'
#' @noRd
.quote_nml_text <- function(x) {
  if (is.character(x) && length(x) > 1L) sprintf("'%s'", x) else x
}

#' @title Ratio of two metric tables, with NA where undefined
#'
#' @description Divides \code{num} by \code{den} (data frames, or vectors for
#'   a single depth) and sets non-finite results to \code{NA}, so a zero
#'   denominator (e.g. DOC or TP reaching 0) gives \code{NA} instead of
#'   \code{Inf}/\code{NaN}. \code{Inf} cannot be written to the float NetCDF
#'   variables of \code{create_netcdf_output()}.
#'
#' @param num,den data.frame or numeric; numerator and denominator.
#'
#' @noRd
.safe_ratio <- function(num, den) {
  r <- num / den
  if (is.data.frame(r)) {
    r[] <- lapply(r, function(x) { x[!is.finite(x)] <- NA; x })
  } else {
    r[!is.finite(r)] <- NA
  }
  r
}

#' @title File name of the AED phyto-/zooplankton parameter namelist
#'
#' @description \code{aed_phyto_pars.nml} / \code{aed_zoop_pars.nml} for
#'   GLM-AED (AED 3), \code{aed2_phyto_pars.nml} / \code{aed2_zoop_pars.nml}
#'   for Simstrat-AED2. The file sits next to the model's AED namelist.
#'
#' @param model character; coupled model name, e.g. \code{"GLM-AED"}.
#' @param module character; \code{"phytoplankton"} or \code{"zooplankton"}.
#'
#' @noRd
.aed_par_file <- function(model, module) {
  prefix <- if (toupper(model) == "GLM-AED") "aed" else "aed2"
  paste0(prefix, if (module == "phytoplankton") "_phyto_pars.nml" else "_zoop_pars.nml")
}

#' @title Biogeochemical model key of a coupled model name
#'
#' @description Returns the part after the last "-" in lower case (e.g.
#'   \code{"GOTM-WET"} -> \code{"wet"}), as used in the \code{model} column of
#'   \code{LakeEnsemblR_WQ_dictionary} and the columns of \code{wq_var_dic}.
#'   \code{"GLM-AED"} maps to \code{"aed2"}: GLM 4 runs AED 3, but shares the
#'   AED2-named dictionary entries with Simstrat-AED2.
#'
#' @param models_coupled character; coupled model name(s).
#'
#' @noRd
.wq_model_key <- function(models_coupled) {
  key <- vapply(strsplit(as.character(models_coupled), "-"),
                function(x) tolower(x[length(x)]), character(1))
  key[key == "aed"] <- "aed2"
  key
}

#' @title Find the GLM namelist in a simulation folder
#'
#' @description Returns the name of the GLM namelist in \code{sim_folder}:
#'   \code{glm4.nml} if present, otherwise \code{glm3.nml} (the name
#'   LakeEnsemblR::export_config() writes; GLM 4 reads it fine when passed
#'   via \code{--nml}).
#'
#' @param sim_folder character; GLM simulation folder.
#'
#' @noRd
.glm_nml_file <- function(sim_folder) {
  for (f in c("glm4.nml", "glm3.nml")) {
    if (file.exists(file.path(sim_folder, f))) return(f)
  }
  stop("No GLM namelist (glm4.nml or glm3.nml) found in ", sim_folder, call. = FALSE)
}

#' @title Run GLM 4 (GLMr)
#'
#' @description Runs GLM 4 with AED in \code{sim_folder} via
#'   \code{GLMr::run_glm()}. GLM's exit status is not reliable (a \code{STOP}
#'   inside AED still returns 0), so any existing output file is removed first
#'   and the run is treated as failed if no new output file appears.
#'
#' @param sim_folder character; GLM simulation folder.
#' @param verbose logical; passed to \code{GLMr::run_glm()}.
#' @param ... further arguments passed to \code{GLMr::run_glm()}.
#'
#' @return The value returned by \code{GLMr::run_glm()}.
#'
#' @noRd
.run_glm_engine <- function(sim_folder, verbose = TRUE, ...) {
  if (!requireNamespace("GLMr", quietly = TRUE)) {
    stop("Package 'GLMr' is required to run GLM-AED.", call. = FALSE)
  }
  nml_file <- .glm_nml_file(sim_folder)

  out_nml <- tryCatch(glmtools::read_nml(file.path(sim_folder, nml_file))[["output"]],
                      error = function(e) NULL)
  out_dir <- gsub("^['\"]|['\"]$", "", if (is.null(out_nml$out_dir)) "output" else out_nml$out_dir)
  out_fn <- gsub("^['\"]|['\"]$", "", if (is.null(out_nml$out_fn)) "output" else out_nml$out_fn)
  nc_path <- file.path(sim_folder, out_dir, paste0(out_fn, ".nc"))
  if (file.exists(nc_path)) unlink(nc_path)

  res <- GLMr::run_glm(sim_folder = sim_folder, nml_file = nml_file,
                       verbose = verbose, ...)

  if (!file.exists(nc_path)) {
    stop("GLM did not produce ", nc_path, " -- the simulation failed. ",
         "Run with verbose = TRUE to see the GLM/AED messages.", call. = FALSE)
  }
  res
}

#' @title Translate an AED2 section/parameter name to its AED 3 equivalent
#'
#' @description GLM 4 runs AED 3, whose namelist sections are named
#'   \code{aed_*} instead of \code{aed2_*}, and which replaced a few
#'   parameters. The dictionary keeps the AED2 names because Simstrat-AED2
#'   still uses them, so GLM paths are translated with this.
#'
#' @param x character; section or parameter name(s).
#'
#' @noRd
.aed3_name <- function(x) {
  x <- sub("^aed2_", "aed_", x)
  renamed <- c(Ranammox = "kanammox")
  hit <- x %in% names(renamed)
  x[hit] <- renamed[x[hit]]
  x
}

#' @title Convert an AED2 namelist to AED 3 naming for GLM 4
#'
#' @description Rewrites \code{nml_path} in place: \code{&aed2_*} section
#'   headers and quoted model names in \code{models = ...} become
#'   \code{aed_*}, and renamed parameters (see \code{.aed3_name()}) are
#'   renamed. Quoted file names (e.g. \code{'aed2_phyto_pars.nml'}) are left
#'   alone. Safe to run on a file that is already converted.
#'
#' @param nml_path character; path to the AED namelist.
#'
#' @noRd
.convert_aed2_nml_to_aed3 <- function(nml_path) {
  lines <- readLines(nml_path, warn = FALSE)
  lines <- gsub("^(\\s*&)aed2_", "\\1aed_", lines)
  lines <- gsub("'aed2_([A-Za-z_]+)'", "'aed_\\1'", lines)
  lines <- gsub("^(\\s*)Ranammox(\\s*=)", "\\1kanammox\\2", lines)
  writeLines(lines, nml_path)
  invisible(nml_path)
}

#' @title Locate a namelist section, accepting AED2 or AED 3 names
#'
#' @description Returns the line range of \code{&section ... /} in
#'   \code{nml_lines}, trying the AED 3 name (\code{.aed3_name()}) when
#'   \code{section} itself is not present, so dictionary paths like
#'   \code{"aed2_oxygen/Fsed_oxy"} also work on GLM 4's \code{aed_oxygen}.
#'
#' @param nml_lines character vector; file content from \code{readLines()}.
#' @param section character; section name without the leading \code{&}.
#'
#' @return A list with \code{start}, \code{end} and \code{aed3} (whether the
#'   AED 3 name matched), or \code{NULL} if neither name is found.
#'
#' @noRd
.find_nml_section <- function(nml_lines, section) {
  section <- trimws(section)
  for (s in unique(c(section, .aed3_name(section)))) {
    sec_idx <- which(grepl(paste0("^\\s*&", s, "\\b"), nml_lines, ignore.case = TRUE))
    if (length(sec_idx) > 0) {
      start <- sec_idx[1]
      slashes <- which(grepl("^\\s*/\\s*$", nml_lines))
      end <- slashes[slashes > start][1]
      if (is.na(end)) end <- length(nml_lines)
      return(list(start = start, end = end, aed3 = s != section))
    }
  }
  NULL
}
