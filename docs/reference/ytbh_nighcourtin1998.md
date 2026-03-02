# Nigh and Courtin (1998) years-to-breast-height model for red alder

Implementation of the Nigh and Courtin (1998) years-to-breast-height
model for red alder (`ALNU.RUB`) in coastal British Columbia.

## Usage

``` r
ytbh_nighcourtin1998(si)
```

## Arguments

- si:

  Numeric vector. Site index SI25 (m at breast-height age 25 years).

## Value

A tibble with one column:

- ytbh:

  Predicted years to breast height (years).

## Details

The source relation is piecewise: \$\$ YTBH = 5.494 - 0.1789 \times
SI25, \quad SI25 \le 25 \$\$ \$\$ YTBH = 1.0, \quad SI25 \> 25 \$\$

## References

Nigh, G. D., & Courtin, P. J. (1998). Height models for Red Alder
(*Alnus rubra* Bong.) in British Columbia. *New Forests*, 16, 59-70.

## Examples

``` r
ytbh_nighcourtin1998(
  si = c(12, 20, 28)
)
#> # A tibble: 3 × 1
#>    ytbh
#>   <dbl>
#> 1  3.35
#> 2  1.92
#> 3  1   
```
