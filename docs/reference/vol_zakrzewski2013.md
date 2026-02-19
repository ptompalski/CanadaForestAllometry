# Estimate tree volume using Zakrzewski (2013) taper model (Ontario)

Estimate tree volume using Zakrzewski (2013) taper model (Ontario)

## Usage

``` r
vol_zakrzewski2013(DBH, height, species)
```

## Source

Zakrzewski, W. T., Penner, M. (2013). *A Comparison of Tree Stem Taper
Models for Use in Ontario*. Ontario Forest Research Institute, Report
176.

## Arguments

- DBH:

  Numeric vector of diameter at breast height (cm).

- height:

  Numeric vector of total tree height (m).

- species:

  Character vector of species codes

## Value

A tibble with volumes (m^3): total and merchantable.

## Note

Implementation is based on an original R script provided by Margaret
Penner, refactored for CanadaForestAllometry (vectorized inputs,
standardized species codes, and merchantability criteria via
[`get_merch_criteria`](https://ptompalski.github.io/CanadaForestAllometry/reference/get_merch_criteria.md)).

## Examples

``` r
# Single tree
vol_zakrzewski2013(
  DBH = 20,
  height = 22,
  species = "PICE.MAR"
)
#> # A tibble: 1 × 2
#>   vol_total vol_merchantable
#>       <dbl>            <dbl>
#> 1     0.304            0.278

# Vectorized input (multiple trees)
vol_zakrzewski2013(
  DBH = c(20, 30, 40),
  height = c(18, 22, 28),
  species = c("PICE.MAR", "PINU.STR", "THUJ.OCC")
)
#> # A tibble: 3 × 2
#>   vol_total vol_merchantable
#>       <dbl>            <dbl>
#> 1     0.254            0.229
#> 2     0.672            0.616
#> 3     1.22             1.16 
```
