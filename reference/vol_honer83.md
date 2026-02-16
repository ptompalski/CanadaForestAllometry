# Honer stem volume using DBH and total height

Estimates total and merchantable stem volume using the Honer et al.
(1983) model, developed for central and eastern Canada.

## Usage

``` r
vol_honer83(DBH, height, species)
```

## Arguments

- DBH:

  Tree diameter at breast height (cm)

- height:

  Total height (m)

- species:

  Species code (e.g., "PINU.CON")

## Value

data.frame with vol_total and vol_merchantable

## References

Honer, T.G.; Ker, M.F.; Alemdag, I.S. 1983. Metric timber tables for the
commercial tree species of central and eastern Canada. Environ. Can.,
Can. For. Serv., Maritimes For. Res. Cent., Fredericton, NB. Inf. Rep.
M-X-140.

## Examples

``` r
# Single tree
vol_honer83(
  DBH = 30,
  height = 22,
  species = "PICE.GLA"
)
#> # A tibble: 1 × 2
#>   vol_total vol_merchantable
#>       <dbl>            <dbl>
#> 1     0.629            0.582

# Multiple trees, mixed species
vol_honer83(
  DBH = c(25, 32, 18),
  height = c(20, 24, 15),
  species = c("PICE.GLA", "ABIE.BAL", "PINU.BAN")
)
#> # A tibble: 3 × 2
#>   vol_total vol_merchantable
#>       <dbl>            <dbl>
#> 1     0.406            0.366
#> 2     0.743            0.686
#> 3     0.176            0.147

# Works with the pipe operator:
trees <- tibble::tibble(
  tree_id = 1:4,
  dbh = c(22, 30, 18, 35),
  ht  = c(18, 24, 15, 27),
  species = c("PICE.GLA", "PICE.GLA", "ABIE.BAL", "PINU.BAN")
)
trees |> dplyr::mutate(vol=vol_honer83(dbh, ht, species)) |> tidyr::unnest(vol)
#> # A tibble: 4 × 6
#>   tree_id   dbh    ht species  vol_total vol_merchantable
#>     <int> <dbl> <dbl> <chr>        <dbl>            <dbl>
#> 1       1    22    18 PICE.GLA     0.289            0.254
#> 2       2    30    24 PICE.GLA     0.672            0.621
#> 3       3    18    15 ABIE.BAL     0.170            0.142
#> 4       4    35    27 PINU.BAN     1.10             1.04 
```
