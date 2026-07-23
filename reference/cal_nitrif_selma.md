# Nitrification rate in SELMAPROTBAS

Calculate the nitrification rate using Dissolved Oxygen, NH4 and
Temperature

## Usage

``` r
cal_nitrif_selma(DO, Temp, NH4)
```

## Arguments

- DO:

  dataframe; The output of modeled oxygen (gramsPerCubicMeter)- datetime
  and the temperature for each depth as seperate column

- Temp:

  dataframe; The output of modeled temperature (C) - datetime and the
  temperature for each depth as seperate column

- NH4:

  dataframe; The output of modeled ammonia (NH4_gramsPerCubicMeter) -
  datetime and the temperature for each depth as seperate column

## Value

Nitrification rate in gramsPerCubicMeterPerDay
