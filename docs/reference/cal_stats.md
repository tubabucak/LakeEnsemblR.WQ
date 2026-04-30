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

- **PBIAS**: Percent Bias

- **lnlikelihood**: Log-likelihood assuming normal distribution

- **KGE**: Kling-Gupta Efficiency (from
  [`hydroGOF::KGE`](https://rdrr.io/pkg/hydroGOF/man/KGE.html))

- **residual**: Vector of observed - predicted residuals

For very small observed or predicted values (\< 1e-3), a minimum
threshold is applied to avoid division by near-zero values in the PBIAS
calculation.
