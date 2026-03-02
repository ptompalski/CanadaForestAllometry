# Nigh (2000) years-to-breast-height model for interior western redcedar

Iimplementation of the Nigh (2000) years-to-breast-height model for
interior western redcedar (`THUJ.PLI`).

## Usage

``` r
ytbh_nigh2000(si)
```

## Arguments

- si:

  Numeric vector. Site index (m, base age 50 years at breast height).

## Value

A tibble with one column:

- ytbh:

  Predicted years to breast height (years).

## Details

The source equation is: \$\$YTBH = 18.18 - 0.5526 \times SI\$\$

## References

Nigh, G. D. (2000). Western redcedar site index models for the interior
of British Columbia. British Columbia Ministry of Forests, Research
Report 18.

## Examples

``` r
ytbh_nigh2000(
  si = c(10, 15, 20)
)
#> # A tibble: 3 × 1
#>    ytbh
#>   <dbl>
#> 1 12.7 
#> 2  9.89
#> 3  7.13
```
