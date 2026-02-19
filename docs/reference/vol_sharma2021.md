# Sharma (2021) stem volume using DBH and total height

Estimates total (inside-bark) and merchantable (inside-bark) stem volume
using the Sharma (2021) dimensionally compatible model developed for
commercial tree species in central/eastern Canada and the northeastern
US.

## Usage

``` r
vol_sharma2021(DBH, height, species)
```

## Arguments

- DBH:

  Tree diameter at breast height (cm)

- height:

  Total height (m)

- species:

  Species code (e.g., "PICE.MAR")

## Value

tibble with columns `vol_total` and `vol_merchantable` (m3)

## Details

Merchantable volume parameters correspond to Sharma's fixed
merchantability definition (0.3 m stump, 7 cm inside-bark top diameter),
and are not user-configurable.

## References

Sharma, M. 2021. Total and Merchantable Volume Equations for 25
Commercial Tree Species Grown in Canada and the Northeastern United
States. Forests 12:1270.

## Examples

``` r
# Single tree
vol_sharma2021(
  DBH = 30,
  height = 22,
  species = "PICE.MAR"
)
#> # A tibble: 1 × 2
#>   vol_total vol_merchantable
#>       <dbl>            <dbl>
#> 1     0.683            0.659

# Multiple trees, mixed species
vol_sharma2021(
  DBH = c(25, 32, 18),
  height = c(20, 24, 15),
  species = c("PICE.GLA", "ABIE.BAL", "PINU.BAN")
)
#> # A tibble: 3 × 2
#>   vol_total vol_merchantable
#>       <dbl>            <dbl>
#> 1     0.433            0.412
#> 2     0.851            0.808
#> 3     0.181            0.165

# Works with the pipe operator:
trees <- tibble::tibble(
  tree_id = 1:4,
  dbh = c(22, 30, 18, 35),
  ht  = c(18, 24, 15, 27),
  species = c("PICE.GLA", "PICE.GLA", "ABIE.BAL", "PINU.BAN")
)
trees |>
  dplyr::mutate(vol = vol_sharma2021(dbh, ht, species)) |>
  tidyr::unnest(vol)
#> # A tibble: 4 × 6
#>   tree_id   dbh    ht species  vol_total vol_merchantable
#>     <int> <dbl> <dbl> <chr>        <dbl>            <dbl>
#> 1       1    22    18 PICE.GLA     0.303            0.287
#> 2       2    30    24 PICE.GLA     0.748            0.714
#> 3       3    18    15 ABIE.BAL     0.169            0.156
#> 4       4    35    27 PINU.BAN     1.18             1.15 
```
