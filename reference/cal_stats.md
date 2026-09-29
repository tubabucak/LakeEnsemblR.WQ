# Calculate Model Performance Statistics

Computes statistical performance metrics to evaluate model predictions
against observations.

## Usage

``` r
cal_stats(observed, predicted)
```

## Arguments

- observed:

  Numeric vector of observed values (e.g., observed DO concentrations in
  mg/L).

- predicted:

  Numeric vector of predicted values (e.g., modeled DO concentrations in
  mg/L).

## Value

A list containing:

- residual:

  Residuals (observed - predicted)

- NSE:

  Nash-Sutcliffe Efficiency

- RMSE:

  Root Mean Squared Error

- NRMSE:

  Normalized RMSE

- PBIAS:

  Percent Bias

- lnlikelihood:

  Log-likelihood

- KGE:

  Kling-Gupta Efficiency

## Details

The function first removes any NA values and then calculates the
following metrics:

- **NSE**: Nash-Sutcliffe Efficiency

- **RMSE**: Root Mean Squared Error

- **NRMSE**: Normalized Root Mean Squared Error (normalized by range of
  observed values)

- **PBIAS**: Percent Bias,
  `100 * sum(observed - predicted) / sum(observed)`. Positive values
  indicate underestimation. Returns `NA` when `sum(observed)` is zero.
  Because it is a ratio to the observed total, it is unreliable when
  observations are mostly near zero.

- **lnlikelihood**: Log-likelihood assuming normal distribution

- **KGE**: Kling-Gupta Efficiency (from
  [`hydroGOF::KGE`](https://hzambran.github.io/hydroGOF/reference/KGE.html))

- **residual**: Vector of observed - predicted residuals

## Examples

``` r
obs  <- c(8.1, 9.4, 10.2, 7.5, 3.1, 1.2)
pred <- c(7.8, 9.9, 9.6, 6.9, 4.0, 0.8)
st <- cal_stats(obs, pred)
st[c("NSE", "KGE", "RMSE", "PBIAS")]
#> $NSE
#> [1] 0.9688976
#> 
#> $KGE
#> [1] 0.9656147
#> 
#> $RMSE
#> [1] 0.5816643
#> 
#> $PBIAS
#> [1] 1.265823
#> 
```
