#'@description
#' Get output data for each model (so far GLM-AED, SELMAPROTBAS-GOTM, WET-GOTM) that is specified in the output.yaml
#'
#' @name get_output_wq
#' @param config_file character:filepath; To LER config yaml file. Only used if model = 'GOTM'
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
#' @importFrom glmtools get_ice get_var get_surface_height

#' @export


get_output_wq <- function(config_file, model, vars, obs_depths = NULL, depth_01 = 1, conversion_factor =1, folder = "."){

  
  ##--------------------------------- GLM ---------------------------------------
  
  if("GLM" %in% model){
  #  print("GLM")
    glm_out <- list()
    if (depth_01 == 1){
      # Extract output
      
      for (variable_model_name in vars) {  # Loop through each variable in vars
      #  print(variable_model_name)
        # Add in obs depths which are not in depths and less than mean depth
        depth <- suppressWarnings(get_nml_value(nml_file = file.path(folder,
                                                                     gotmtools::get_yaml_value(config_file,
                                                                                               "config_files",
                                                                                               "GLM")),
                                                arg_name = "lake_depth"))
       # print(depth)
        depths <- seq(0, depth, by = gotmtools::get_yaml_value(config_file, "output", "depths"))
        add_deps <- obs_depths[!(obs_depths %in% depths)]
        depths <- c(add_deps, depths)
        depths <- depths[order(depths)]
        
        glm_var_out <- glmtools::get_var(file = file.path(folder, "GLM", "Output",
                                                                             "Output.nc"),
                                                            var_name = variable_model_name, reference = "surface",
                                                            z_out = depths)
        # Unit conversion for the columns (excluding the Datetime)

        glm_var_out[, -1] <- glm_var_out[,-1] * conversion_factor
        glm_out[[length(glm_out) + 1]]  <- glm_var_out

        colnames(glm_out[[length(glm_out)]]) <- c("datetime", paste("Depth_", depths, sep = ""))
        names(glm_out)[length(glm_out)] <- variable_model_name
      }
      
    }
     if (depth_01 == 0){
      for (variable_model_name in vars) {  # Loop through each variable in vars
        
        glm_var_out <- glmtools::get_var(file = file.path(folder, "GLM", "Output",
                                                                             "Output.nc"), var_name = variable_model_name)

         # Unit conversion for the columns (excluding the Datetime)

        glm_var_out[, -1] <- glm_var_out[,-1] * conversion_factor
        glm_out[[length(glm_out) + 1]]  <- glm_var_out
        names(glm_out)[length(glm_out)] <- variable_model_name
    
      }
      
     } 
      # If only one variable return a dataframe
      if(length(glm_out) == 1){
        glm_out <- glm_out[1]
      }
      
      
    # }
    return(glm_out)
    
  }
  ##--------------------------- SELMAPROTBAS ------------------------------------------------
  
  if("SELMAPROTBAS" %in% model){
    
    selma_out <- list()
    if (depth_01 == 1){
      
      # Extract output
      
      for (variable_model_name in vars) {  # Loop through each variable in vars
        
        var_out <- get_vari(ncdf = file.path(folder, "SELMAPROTBAS", "Output", "Output.nc"), var = variable_model_name,
                            print = FALSE)
        z <- gotmtools::get_vari(ncdf = file.path(folder, "SELMAPROTBAS", "Output", "Output.nc"), var = "z",
                                 print = FALSE)
        
        z[, 2:ncol(z)] <- t(apply(z[, 2:ncol(z)], 1, 
                                  function(x) as.numeric(x) - max(as.numeric(x))))
        
        # Add in obs depths which are not in depths and less than mean depth
        depths <- seq(0, min(z[, -1]), by = -1 * gotmtools::get_yaml_value(config_file, "output", "depths"))
        if(is.null(obs_depths)) {
          obs_dep_neg <- NULL
        } else {
          obs_dep_neg <- -obs_depths
        }
        add_deps <- obs_dep_neg[!(obs_dep_neg %in% depths)]
        depths <- c(add_deps, depths)
        depths <- depths[order(-depths)]
        
        message("Interpolating GOTM temp to include obs depths... ",
                paste0("[", Sys.time(), "]"))
        selma_var_out <- setmodDepths(var_out, z, depths = depths, print = T)
        message("Finished interpolating! ",
                paste0("[", Sys.time(), "]"))
        
        selma_var_out <- dcast(selma_var_out, date ~ depths)
        
        # check water level fluctuations
        got_wlvl <- as.matrix(t(apply(z, 1, function(x) (as.numeric(x[length(x)]) > 
                                                           (as.numeric(colnames(selma_var_out)[-1]))))))

        selma_var_out <- as.data.frame(selma_var_out)
        idz <- which(got_wlvl == T, arr.ind = T)
        idz[, 2] <- idz[, 2] + 1
        selma_var_out[idz] <- NA
        selma_var_out <- selma_var_out[, c(1, (ncol(selma_var_out):2))]
        str_depths <- abs(as.numeric(colnames(selma_var_out)[2:ncol(selma_var_out)]))
        colnames(selma_var_out) <- c("datetime", paste("Depth_", str_depths, sep = ""))
        
        selma_var_out[,-1] <- selma_var_out[,-1]* conversion_factor
        selma_out[[length(selma_out) + 1]] <- selma_var_out
        names(selma_out)[length(selma_out)] <- variable_model_name
      }
    }


    if (depth_01 == 0){
      for (variable_model_name in vars) {  # Loop through each variable in vars
      selma_var_out <- get_vari(ncdf = file.path(folder, "SELMAPROTBAS", "Output", "Output.nc"), var = variable_model_name,
                          print = FALSE)
      # ice_frazil <- get_vari(ncdf = file.path(folder, "GOTM", "output", "output.nc"),
      #                        var = "Hfrazil", print = FALSE)
      # ice_height[,2] <- ice_height[,2] + ice_frazil[,2]
      
      # Unit conversion
      selma_var_out[,-1] <- selma_var_out[,-1]* conversion_factor
      selma_out[[length(selma_out) + 1]] <- selma_var_out

      names(selma_out)[length(selma_out)] <- variable_model_name
      } 
    }
    if(length(selma_out) == 1){
      selma_out <- selma_out[1]
    }
    
    return(selma_out)
  }
  
  ##------------------- WET ----------------------------------------------------
  
  if("WET" %in% model){
    
    wet_out <- list()
     if (depth_01 == 1){
      # Extract output
      
      for (variable_model_name in vars) {  # Loop through each variable in vars
        
        var_out <- get_vari(ncdf = file.path(folder, "WET", "Output", "Output.nc"), var = variable_model_name,
                            print = FALSE)
       
        z <- gotmtools::get_vari(ncdf = file.path(folder, "WET", "Output", "Output.nc"), var = "z",
                                 print = FALSE)

        z[, 2:ncol(z)] <- t(apply(z[, 2:ncol(z)], 1, 
                                  function(x) as.numeric(x) - max(as.numeric(x))))
        
        # Add in obs depths which are not in depths and less than mean depth
        depths <- seq(0, min(z[, -1]), by = -1 * gotmtools::get_yaml_value(config_file, "output", "depths"))
        if(is.null(obs_depths)) {
          obs_dep_neg <- NULL
        } else {
          obs_dep_neg <- -obs_depths
        }
        add_deps <- obs_dep_neg[!(obs_dep_neg %in% depths)]
        depths <- c(add_deps, depths)
        depths <- depths[order(-depths)]



        message("Interpolating GOTM temp to include obs depths... ",
                paste0("[", Sys.time(), "]"))
        wet_var_out <- setmodDepths(var_out, z, depths = depths, print = T)
        message("Finished interpolating! ",
                paste0("[", Sys.time(), "]"))
        
        wet_var_out <- dcast(wet_var_out, date ~ depths)
        message("Wet_var_out")
        
        # check water level fluctuations
        got_wlvl <- as.matrix(t(apply(z, 1, function(x) (as.numeric(x[length(x)]) > 
                                                           (as.numeric(colnames(wet_var_out)[-1]))))))

        wet_var_out <- as.data.frame(wet_var_out)
        idz <- which(got_wlvl == T, arr.ind = T)

        idz[, 2] <- idz[, 2] + 1
        wet_var_out[idz] <- NA
        wet_var_out <- wet_var_out[, c(1, (ncol(wet_var_out):2))]
        str_depths <- abs(as.numeric(colnames(wet_var_out)[2:ncol(wet_var_out)]))
        colnames(wet_var_out) <- c("datetime", paste("Depth_", str_depths, sep = ""))
        
        wet_var_out[,-1] <- wet_var_out[,-1]* conversion_factor
        wet_out[[length(wet_out) + 1]] <- wet_var_out
        names(wet_out)[length(wet_out)] <- variable_model_name
      }
     }
     if (depth_01 == 0){
      for (variable_model_name in vars) { 
       wet_var_out <- get_vari(ncdf = file.path(folder, "WET", "Output", "Output.nc"), var = variable_model_name,
                               print = FALSE)
       
       wet_var_out[,-1] <- wet_var_out[,-1]* conversion_factor
       wet_out[[length(wet_out) + 1]] <- wet_var_out
       names(wet_out)[length(wet_out)] <- variable_model_name
       
     }
     }
    
    if(length(wet_out) == 1){
      wet_out <- wet_out[1]
    }
    
    return(wet_out)
    

  }
  
}


