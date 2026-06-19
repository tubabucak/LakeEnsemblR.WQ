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

  if (length(observed) == 0L) {
    return(list(
      residual = numeric(0),
      NSE = NA_real_,
      RMSE = NA_real_,
      NRMSE = NA_real_,
      PBIAS = NA_real_,
      lnlikelihood = NA_real_,
      KGE = NA_real_
    ))
  }
  
  
  # NSE Function
  calculate_nse <- function(observed, predicted) {
    denom <- sum((observed - mean(observed))^2)
    if (!is.finite(denom) || denom <= 0) {
      return(NA_real_)
    }
    NSE <- 1 - (sum((observed - predicted)^2) / denom)
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
    rng <- max(observed) - min(observed)
    if (!is.finite(rng) || rng == 0) {
      return(NA_real_)
    }
    NRMSE <- RMSE/rng
    
    return(NRMSE)
  }
  
  calculate_pbias <- function(observed, predicted) {
    
    adjusted_observed <- ifelse(abs(observed) < 1e-3, 1e-3, observed)
    adjusted_predicted <- ifelse(abs(predicted) < 1e-3, 1e-3, predicted)
    pbias <- mean((adjusted_observed - adjusted_predicted) / abs(adjusted_observed)) * 100
    
    return(pbias)
    
  }
  
  calculate_lnlikelihood <- function(observed, predicted) {
    if (length(observed) < 2L) {
      return(NA_real_)
    }
    sd_obs <- stats::sd(observed)
    if (!is.finite(sd_obs) || sd_obs <= 0) {
      return(NA_real_)
    }
    return(sum(stats::dnorm(observed, mean = predicted, sd = sd_obs, log = TRUE)))
    
  }
  
  NSE<- calculate_nse (observed, predicted)
  RMSE <- calculate_rmse (observed, predicted)
  NRMSE<-  calculate_nrmse (observed, predicted)
  PBIAS <-   calculate_pbias (observed, predicted)
  lnlikelihood<- calculate_lnlikelihood (observed, predicted)
  KGE <- if (length(observed) >= 2L) {
    suppressWarnings(hydroGOF::KGE(observed, predicted))
  } else {
    NA_real_
  }
  residual <- observed-predicted
  return(list(residual =residual, NSE = NSE, RMSE = RMSE, NRMSE = NRMSE, PBIAS =PBIAS, lnlikelihood = lnlikelihood, KGE = KGE))
  
}