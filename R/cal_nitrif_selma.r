#'@title Nitrification rate in SELMAPROTBAS

#'@description
#' Calculate the nitrification rate using Dissolved Oxygen, NH4 and Temperature

#' @name cal_nitrif_selma

#' @param Temp dataframe; The output of modeled temperature (C) - datetime and the temperature for each depth as seperate column
#' @param DO dataframe; The output of modeled oxygen (gramsPerCubicMeter)- datetime and the temperature for each depth as seperate column
#' @param NH4 dataframe; The output of modeled ammonia (NH4_gramsPerCubicMeter) - datetime and the temperature for each depth as seperate column


#' @return Nitrification rate in gramsPerCubicMeterPerDay

#' @importFrom dplyr select

#' @export

cal_nitrif_selma <- function(DO, Temp, NH4){

 DO_s <- DO %>% select(-datetime)
  Temp_s <- Temp %>% select(-datetime)
  NH4_s <- NH4 %>% select(-datetime)
  nf <- ((DO_s/(0.01 + DO_s))* 0.1* exp(0.11* Temp_s)  )
  nitrification <- nf* NH4_s *14/1000
  nitrification$datetime = DO$datetime

 # Reorder columns to have datetime first
  nitrification <- nitrification[, c("datetime", setdiff(names(nitrification), "datetime"))]
  return(nitrification)
  
}
