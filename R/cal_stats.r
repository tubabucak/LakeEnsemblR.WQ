#'@title Calculate statistical metrics

#'@description
#' The aim is calculate statistiscal measures like residual, NSE, RMSE, PBIAS, lnlikelihood, KGE

#' @name cal_stats
#' @param observed: dataframe or numeric vector of observed outputs
#' @param predicted: dataframe or numeric vector of oprecited outputs

#' 
#' @return A list with statistical metrics.
#' 
#' @importFrom hydroGOF KGE
#' @examples
#' Example usage:
#' cal_stats(Observed, Predicted)


#' @export


cal_stats <- function(observed, predicted) {
  


# Helper function to clean NA values
  remove_na <- function(obs, pred) {
    valid_indices <- which(!is.na(obs) & !is.na(pred))
    list(obs = obs[valid_indices], pred = pred[valid_indices])
  }


 data <- remove_na(observed, predicted)
  observed <- data$obs
  predicted <- data$pred


# NSE Function
calculate_nse <- function(observed, predicted) {

  NSE <- 1 - (sum((observed - predicted)^2) / sum((observed - mean(observed))^2))
  
  return(NSE)
}

calculate_rmse <- function(observed, predicted) {


  residual <- observed-predicted
  RMSE <- sqrt(sum(residual^2)/length(observed))

  
  return(RMSE)
}

calculate_nrmse <- function(observed, predicted) {

  residual <- observed-predicted
  RMSE <- sqrt(sum(residual^2)/length(observed))
  NRMSE <- RMSE/(max(observed)- min(observed))
  
  return(NRMSE)
}

calculate_pbias <- function(observed, predicted) {
  
  adjusted_observed <- ifelse(abs(observed) < 1e-3, 1e-3, observed)
  adjusted_predicted <- ifelse(abs(predicted) < 1e-3, 1e-3, predicted)
  pbias <- mean((adjusted_observed - adjusted_predicted) / abs(adjusted_observed)) * 100
  
  return(pbias)

}

calculate_lnlikelihood <- function(observed, predicted) {
  
  return(sum(dnorm(observed, mean = predicted, log = TRUE)))

}

NSE<- calculate_nse (observed, predicted)
RMSE <- calculate_rmse (observed, predicted)
NRMSE<-  calculate_nrmse (observed, predicted)
PBIAS <-   calculate_pbias (observed, predicted)
lnlikelihood<- calculate_lnlikelihood (observed, predicted)
KGE <- hydroGOF::KGE(observed, predicted)
residual <- observed-predicted
  return(list(residual =residual, NSE = NSE, RMSE = RMSE, NRMSE = NRMSE, PBIAS =PBIAS, lnlikelihood = lnlikelihood, KGE = KGE))

}
