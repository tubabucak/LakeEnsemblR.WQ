#' @title Calculate Model Performance Statistics
#'
#' @description
#' Computes statistical performance metrics to evaluate model predictions against observations.
#' @name cal_stats
#' @param observed Numeric vector of observed values (e.g., observed DO concentrations in mg/L).
#' @param predicted Numeric vector of predicted values (e.g., modeled DO concentrations in mg/L).
#'
#' @details
#' The function first removes any NA values and then calculates the following metrics:
#' \itemize{
#'   \item \strong{NSE}: Nash-Sutcliffe Efficiency
#'   \item \strong{RMSE}: Root Mean Squared Error
#'   \item \strong{NRMSE}: Normalized Root Mean Squared Error (normalized by range of observed values)
#'   \item \strong{PBIAS}: Percent Bias
#'   \item \strong{lnlikelihood}: Log-likelihood assuming normal distribution
#'   \item \strong{KGE}: Kling-Gupta Efficiency (from \code{hydroGOF::KGE})
#'   \item \strong{residual}: Vector of observed - predicted residuals
#' }
#'
#' For very small observed or predicted values (< 1e-3), a minimum threshold is applied to avoid division by near-zero values in the PBIAS calculation.
#'
#' @return A list containing:
#' \item{residual}{Residuals (observed - predicted)}
#' \item{NSE}{Nash-Sutcliffe Efficiency}
#' \item{RMSE}{Root Mean Squared Error}
#' \item{NRMSE}{Normalized RMSE}
#' \item{PBIAS}{Percent Bias}
#' \item{lnlikelihood}{Log-likelihood}
#' \item{KGE}{Kling-Gupta Efficiency}
#'
#' @importFrom hydroGOF KGE
#' @importFrom stats dnorm
#'

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