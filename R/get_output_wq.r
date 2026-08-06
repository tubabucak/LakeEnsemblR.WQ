#'@title Get lake model outputs

#'@description
#' Get output data for each model (so far GLM-AED, SELMAPROTBAS-GOTM, WET-GOTM) that is specified in the output.yaml
#'
#' @name get_output_wq
##' @param LER_config_file character:filepath; To LER config yaml file. Only used if model = 'GOTM'
#' @param config_file character:filepath; to Output yaml.
#' @param model character; Model for which scaling parameters will be applied. So far options include
#'    c('GLM', 'SELMAPROTBAS', 'WET')
#' @param vars character vector; variables to be extracted to calculate the metric
#' @param obs_depths numeric vector; Observation depths. Its required if we need to interpolate the modelled output with observation depths. Defaults to NULL
#' @param folder character: main filepath; where all the model files are stored.
#' @param depth_01 integer; Indicates if the variable has 'z' dimension. 0: The variable has no depth component, 1: variable has depth component
#' @param conversion_factor numeric; unit conversion factors for common metric unit for each variable

#' @return A list or dataframe of extracted variables from the specified model. If only one variable is extracted, a dataframe is returned. Otherwise, a list of dataframes is returned.
#' 
#' @return dataframe or list of output variables
#' @importFrom reshape2 dcast
#' @importFrom gotmtools get_vari setmodDepths get_yaml_value
#' @importFrom glmtools get_ice get_var get_surface_height get_nml_value

#' @export
get_output_wq <- function(config_file,
                          model,
                          vars,
                          obs_depths = NULL,
                          depth_01 = 1,
                          conversion_factor = 1) {


  # Load configuration file
  cfg <- load_config(config_file)
  model_upper <- toupper(model)
  # cfg$model_folders$GLM, cfg$model_folders$WET, cfg$model_folders$SELMAPROTBAS

  ##------------------------- GLM ---------------------------------------
  if ("GLM" %in% model_upper) {
    glm_out <- list()
    
    if (depth_01 == 1) {
      for (variable_model_name in vars) {
      glm_nml_rel <- NULL
      for (glm_cfg_key in c("GLM", "glm", "Glm")) {
        glm_nml_rel <- tryCatch({
          gotmtools::get_yaml_value(cfg$LER_config_file, "config_files", glm_cfg_key)
        }, error = function(e) {
          NULL
        })
        if (!is.null(glm_nml_rel)) {
          break
        }
      }
      if (is.null(glm_nml_rel)) {
        stop("Could not find GLM entry in LER config 'config_files'.")
      }
      # glm_nml_rel (e.g. "GLM/glm3.nml") is used here as-is, assuming the
      # standard LakeEnsemblR layout (a "GLM/" folder next to the LER
      # config file). Projects with a differently-named model folder and
      # no such nesting (e.g. glm3.nml living directly in "GLM-AED2/",
      # this project's actual layout) get a nonexistent path here -- and
      # it breaks worse inside a parallel worker sandbox, which has no
      # project-root "GLM/" folder to fall back into at all. Derive from
      # cfg$model_folders$GLM instead (already known-correct -- it's what
      # this same function successfully reads output.nc from, including
      # inside worker sandboxes) rather than re-deriving a second,
      # independent (and convention-dependent) path from the LER config.
      nml_file <- if (!grepl("^([A-Za-z]:|/)", glm_nml_rel)) {
        nml_candidates <- c(file.path(dirname(cfg$model_folders$GLM), basename(glm_nml_rel)),
                            file.path(cfg$model_folders$GLM, basename(glm_nml_rel)))
        found <- nml_candidates[file.exists(nml_candidates)][1]
        if (is.na(found)) {
          stop("Could not find GLM nml file '", basename(glm_nml_rel),
               "' near model_folders entry: ", cfg$model_folders$GLM)
        }
        found
      } else {
        glm_nml_rel
      }

depth <- suppressWarnings(get_nml_value(
  nml_file = nml_file,
  arg_name = "lake_depth"
))

        depths <- seq(0, depth, by = gotmtools::get_yaml_value(cfg$LER_config_file, "output", "depths"))
        add_deps <- obs_depths[!(obs_depths %in% depths)]
        depths <- sort(c(add_deps, depths))

        glm_var_out <- tryCatch({
          glmtools::get_var(
            file = file.path(cfg$model_folders$GLM, "output.nc"),
            var_name = variable_model_name,
            reference = "surface",
            z_out = depths
          )
        }, error = function(e) {
          cat("Error extracting variable:", variable_model_name, "\n")
          print(e)
          return(NULL)
        })

        glm_var_out[, -1] <- glm_var_out[, -1] * conversion_factor
        colnames(glm_var_out) <- c("datetime", paste0("Depth_", depths))
        glm_var_out$datetime <- as.POSIXct(glm_var_out$datetime, tz = "UTC")
        glm_out[[variable_model_name]] <- glm_var_out
      }
    }

    if (depth_01 == 0) {
      for (variable_model_name in vars) {
        glm_var_out <- glmtools::get_var(
          file = file.path(cfg$model_folders$GLM,  "output.nc"),
          var_name = variable_model_name
        )
        glm_var_out[, -1] <- glm_var_out[, -1] * conversion_factor
        glm_out[[variable_model_name]] <- glm_var_out
      }
    }

    return(if (length(glm_out) == 1) glm_out[1] else glm_out)
  }


##--------------------------- SELMAPROTBAS ------------------------------------------------

  if ("SELMAPROTBAS" %in% model_upper) {
    return(.get_output_wq_gotm_fabm(cfg$model_folders$SELMAPROTBAS, cfg, vars,
                                    obs_depths, depth_01, conversion_factor))
  }

  ##------------------- WET ----------------------------------------------------

  if ("WET" %in% model_upper) {
    return(.get_output_wq_gotm_fabm(cfg$model_folders$WET, cfg, vars,
                                    obs_depths, depth_01, conversion_factor))
  }

  ##------------------- Simstrat ------------------------------------------------

  if("SIMSTRAT" %in% model_upper){

    sim_cfg_rel <- NULL
    for (sim_cfg_key in c("Simstrat", "SIMSTRAT", "simstrat")) {
      sim_cfg_rel <- tryCatch({
        gotmtools::get_yaml_value(cfg$LER_config_file, "config_files", sim_cfg_key)
      }, error = function(e) {
        NULL
      })
      if (!is.null(sim_cfg_rel)) {
        break
      }
    }
    if (is.null(sim_cfg_rel)) {
      stop("Could not find Simstrat entry in LER config 'config_files'.")
    }

    sim_folder_key <- names(cfg$model_folders)[toupper(names(cfg$model_folders)) == "SIMSTRAT"][1]
    if (is.na(sim_folder_key)) {
      stop("Could not find Simstrat entry in Output config 'model_folders'.")
    }
    sim_folder <- cfg$model_folders[[sim_folder_key]]

    # Get reference year and timestep from Simstrat par (JSON) file.
    # sim_cfg_rel (from LER config's config_files.Simstrat, e.g.
    # "Simstrat/simstrat.par") is used here WITHOUT basename() -- unlike
    # .derive_ler_config_filename(), which strips it -- so combining it
    # with dirname(cfg$LER_config_file) assumes the standard LakeEnsemblR
    # layout (a "Simstrat/" folder next to the LER config file). Projects
    # that don't follow that convention (e.g. par file living directly in
    # a differently-named model folder, with no "Simstrat/" nesting at
    # all) get a nonexistent path here, and it silently breaks worse
    # inside a parallel worker sandbox where there's no project-root
    # "Simstrat/" folder to fall back into at all. model_folders$SIMSTRAT
    # (sim_folder) is already known-correct (it's what everything else in
    # this function successfully reads output from, including inside
    # worker sandboxes) and the par file lives directly alongside it, one
    # level up from wherever model_folders points -- so derive sim_par
    # from THAT instead of re-deriving a second, independent (and
    # convention-dependent) path from the LER config.
    sim_par_candidates <- c(file.path(dirname(sim_folder), basename(sim_cfg_rel)),
                            file.path(sim_folder, basename(sim_cfg_rel)))
    sim_par <- sim_par_candidates[file.exists(sim_par_candidates)][1]
    if (is.na(sim_par)) {
      stop("Could not find Simstrat par file '", basename(sim_cfg_rel),
           "' near model_folders entry: ", sim_folder)
    }
    timestep <- get_json_value(sim_par, "Simulation", "Timestep s")
    reference_year <- get_json_value(sim_par, "Simulation", "Reference year")

    sim_out <- list()

    if(depth_01 == 1){
      for(variable_model_name in vars){

        dat_file <- file.path(sim_folder,
                              paste0(variable_model_name, "_out.dat"))
        var_dat <- read.table(dat_file, header = TRUE, sep = ",", check.names = FALSE)

        # Convert decimal days to POSIXct
        var_dat[, 1] <- as.POSIXct(var_dat[, 1] * 3600 * 24,
                                    origin = paste0(reference_year, "-01-01"), tz = "UTC")
        var_dat[, 1] <- lubridate::round_date(var_dat[, 1],
                                              unit = lubridate::seconds_to_period(timestep))

        # Reorder columns: datetime, then depth from shallow to deep
        var_dat <- var_dat[, c(1, ncol(var_dat):2)]

        mod_depths <- as.numeric(colnames(var_dat)[-1])
        if(is.null(obs_depths)){
          obs_dep_neg <- NULL
        } else {
          obs_dep_neg <- -obs_depths
        }
        add_deps <- obs_dep_neg[!(obs_dep_neg %in% mod_depths)]
        depths <- c(add_deps, mod_depths)
        depths <- depths[order(-depths)]

        if(length(depths) != (ncol(var_dat) - 1)){
          message("Interpolating Simstrat ", variable_model_name,
                  " to include obs depths... ", paste0("[", Sys.time(), "]"))
          wat_mat <- matrix(NA, nrow = nrow(var_dat), ncol = length(depths))
          for(i in seq_len(nrow(var_dat))){
            y <- as.vector(unlist(var_dat[i, -1]))
            wat_mat[i, ] <- approx(mod_depths, y, depths, rule = 2)$y
            if(any(is.na(y))){
              min_depth_na <- mod_depths[min(which(is.na(y)))]
              min_ind_na <- min(which(depths <= min_depth_na))
              wat_mat[i, (min_ind_na:length(wat_mat[i, ]))] <- NA
            }
          }
          message("Finished interpolating! ", paste0("[", Sys.time(), "]"))
          df <- data.frame(wat_mat)
          df$datetime <- var_dat[, 1]
          df <- df[, c(ncol(df), 1:(ncol(df) - 1))]
          colnames(df) <- c("datetime", paste0("Depth_", abs(depths)))
          var_dat <- df
        } else {
          str_depths <- abs(as.numeric(colnames(var_dat)[2:ncol(var_dat)]))
          colnames(var_dat) <- c("datetime", paste0("Depth_", str_depths))
        }

        var_dat[, -1] <- var_dat[, -1] * conversion_factor
        sim_out[[variable_model_name]] <- var_dat
      }
    }

    if(depth_01 == 0){
      for(variable_model_name in vars){

        dat_file <- file.path(sim_folder,
                              paste0(variable_model_name, "_out.dat"))
        var_dat <- read.table(dat_file, header = TRUE, sep = ",", check.names = FALSE)

        # Convert decimal days to POSIXct
        var_dat[, 1] <- as.POSIXct(var_dat[, 1] * 3600 * 24,
                                    origin = paste0(reference_year, "-01-01"), tz = "UTC")
        var_dat[, 1] <- lubridate::round_date(var_dat[, 1],
                                              unit = lubridate::seconds_to_period(timestep))
        colnames(var_dat)[1] <- "datetime"

        var_dat[, -1] <- var_dat[, -1] * conversion_factor
        sim_out[[variable_model_name]] <- var_dat
      }
    }

    if(length(sim_out) == 1){
      sim_out <- sim_out[1]
    }

    return(sim_out)
  }

}

# ---------------------------------------------------------------------------
# Internal helper: SELMAPROTBAS and WET are both GOTM-coupled FABM models and
# read their output the same way (get_vari() + setmodDepths() + dcast(),
# followed by the same water-level-fluctuation masking) -- the only
# difference between the two call sites was which cfg$model_folders$<model>
# entry to read from. Shared here instead of duplicated per model.
# ---------------------------------------------------------------------------
.get_output_wq_gotm_fabm <- function(model_folder, cfg, vars, obs_depths,
                                     depth_01, conversion_factor) {
  out <- list()

  if (depth_01 == 1) {
    for (variable_model_name in vars) {
      var_out <- get_vari(ncdf = file.path(model_folder, "output.nc"), var = variable_model_name,
                          print = FALSE)
      z <- gotmtools::get_vari(ncdf = file.path(model_folder, "output.nc"), var = "z",
                               print = FALSE)

      z[, 2:ncol(z)] <- t(apply(z[, 2:ncol(z)], 1,
                                function(x) as.numeric(x) - max(as.numeric(x))))

      # Add in obs depths which are not in depths and less than mean depth
      depths <- seq(0, min(z[, -1]), by = -1 * gotmtools::get_yaml_value(cfg$LER_config_file, "output", "depths"))
      if (is.null(obs_depths)) {
        obs_dep_neg <- NULL
      } else {
        obs_dep_neg <- -obs_depths
      }
      add_deps <- obs_dep_neg[!(obs_dep_neg %in% depths)]
      depths <- c(add_deps, depths)
      depths <- depths[order(-depths)]

      message("Interpolating GOTM temp to include obs depths... ",
              paste0("[", Sys.time(), "]"))
      var_out <- setmodDepths(var_out, z, depths = depths, print = T)
      message("Finished interpolating! ",
              paste0("[", Sys.time(), "]"))

      var_out <- dcast(var_out, date ~ depths)

      # check water level fluctuations
      got_wlvl <- as.matrix(t(apply(z, 1, function(x) (as.numeric(x[length(x)]) >
                                                         (as.numeric(colnames(var_out)[-1]))))))

      var_out <- as.data.frame(var_out)
      idz <- which(got_wlvl == T, arr.ind = T)
      idz[, 2] <- idz[, 2] + 1
      var_out[idz] <- NA
      var_out <- var_out[, c(1, (ncol(var_out):2))]
      str_depths <- abs(as.numeric(colnames(var_out)[2:ncol(var_out)]))
      colnames(var_out) <- c("datetime", paste("Depth_", str_depths, sep = ""))
      var_out$datetime <- as.POSIXct(var_out$datetime, tz = "UTC")

      var_out[, -1] <- var_out[, -1] * conversion_factor
      out[[length(out) + 1]] <- var_out
      names(out)[length(out)] <- variable_model_name
    }
  }

  if (depth_01 == 0) {
    for (variable_model_name in vars) {
      var_out <- get_vari(ncdf = file.path(model_folder, "output.nc"), var = variable_model_name,
                          print = FALSE)

      var_out[, -1] <- var_out[, -1] * conversion_factor
      out[[length(out) + 1]] <- var_out
      names(out)[length(out)] <- variable_model_name
    }
  }

  if (length(out) == 1) {
    out <- out[1]
  }

  out
}

