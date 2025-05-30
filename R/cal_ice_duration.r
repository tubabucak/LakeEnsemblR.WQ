#'@title Calculate Ice Duration & Thickness
#'@description
#' Calculates Ice Duration Days in a year & Ice Thickness for each day

#' @name cal_ice_duration
#' @param ice_sum, dataframe; Includes datetime and ice thickness
#' @return ice_duration_period: dataframe with number of ice days in a year & ice_thickness: for each simulated day.
#' @importFrom lubridate year


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
