#'@title Calculate Ice Duration & Thickness
#'@description
#' Calculates Ice Duration Days in a year & Ice Thickness for each day

#' @name cal_ice_duration
#' @param ice_sum, dataframe; Includes datetime and ice thickness
#' @param white_ice, dataframe; Includes datetime and white_ice thickness. Only for GLM, this is white_ice_thickness variable in GLM
#' @return ice_duration_period: dataframe with number of ice days in a year & ice_thickness: for each simulated day.
#' @importFrom lubridate year
#' @ImportFrom lubridate year

#' @export


cal_ice_duration <- function(ice_sum){

ice_duration_day<- numeric(0)


colnames(ice_sum)<- c("datetime", "ice_thickness")

ice_sum$year <- lubridate::year(ice_sum$datetime)


# Extract the unique years
unique_years <- unique(ice_sum$year)


for (i in 1:length(unique_years)){
    ice_sum_sub <- ice_sum %>%
    filter(year == unique_years[i] & ice_thickness >0)
    ice_day <- nrow(ice_sum_sub)
    ice_duration_day[i]<- ice_day
}

ice_duration_period <- data.frame(unique_years, ice_duration_day)


 return(list(ice_duration_period = ice_duration_period, ice_thickness = ice_sum))


}

















cal_ice_duration <- function(blue_ice, white_ice, model_name){

ice_duration_day<- numeric(0)

 if (model_name == 'GLM'){
    ice_sum <- data.frame(blue_ice[,1], blue_ice[,2]+ white_ice[,2])
    colnames(ice_sum)<- c("datetime", "ice_thickness")
}

 else if (model_name == 'SELMAPROTBAS'){
    ice_sum <- data.frame(blue_ice[,1], blue_ice[,2])
    colnames(ice_sum)<- c("datetime", "ice_thickness")
}
 else if (model_name == 'WET'){
    ice_sum <- data.frame(blue_ice[,1], blue_ice[,2])
    colnames(ice_sum)<- c("datetime", "ice_thickness")
}


ice_sum$year <- lubridate::year(ice_sum$datetime)


# Extract the unique years
unique_years <- unique(ice_sum$year)


for (i in 1:length(unique_years)){
    ice_sum_sub <- ice_sum %>%
    filter(year == unique_years[i] & ice_thickness >0)
    ice_day <- nrow(ice_sum_sub)
    ice_duration_day[i]<- ice_day
}

ice_duration_period <- data.frame(unique_years, ice_duration_day)


 return(list(ice_duration_period = ice_duration_period, ice_thickness = ice_sum))


}
# glm_ice <- cal_ice_duration(All_Temp_DO, "GLM")
