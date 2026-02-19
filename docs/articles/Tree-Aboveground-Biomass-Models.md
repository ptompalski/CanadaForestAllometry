# Tree Aboveground Biomass Models

The `CanadaForestAllometry` package implements the national aboveground
biomass (AGB) equations originally developed by Lambert et al.
([2005](#ref-Lambert2005)) and subsequently revised by Ung et al.
([2008](#ref-Ung2008)). The revision by Ung et al.
([2008](#ref-Ung2008)) incorporated previously unavailable British
Columbia data, expanding species coverage and updating parameter
estimates while preserving the original additive modelling framework.

These equations were derived from destructively sampled trees across
Canada and estimate dry biomass for four aboveground components: stem
wood, stem bark, branches, and foliage. Two model formulations were
developed: one using DBH alone, and a second using DBH combined with
total tree height.

The
[`agb_lambert_ung()`](https://ptompalski.github.io/CanadaForestAllometry/reference/agb_lambert_ung.md)
function implements both equation systems within a single interface.
When a valid height is supplied, the DBH–height formulation is used;
otherwise, the DBH-only formulation is applied.

### Example:

``` r
 trees <- tibble::tibble(
  tree_id = 1:4,
  DBH = c(22, 30, 18, 35),
  height = c(18, 24, NA, 27),
  species = c("PICE.GLA", "PICE.GLA", "ABIE.BAL", "PINU.BAN")
 )

trees |>
  dplyr::mutate(
    agb = agb_lambert_ung(
      DBH = DBH,
      height = height,
      species = species,
      keep_model_id = TRUE
    )
  ) |>
  unnest(agb)
#> # A tibble: 4 × 12
#>   tree_id   DBH height species  Bwood Bbark Bstem Bfoliage Bbranches Bcrown
#>     <int> <dbl>  <dbl> <chr>    <dbl> <dbl> <dbl>    <dbl>     <dbl>  <dbl>
#> 1       1    22     18 PICE.GLA 113.   15.1 128.      13.5      17.4   30.9
#> 2       2    30     24 PICE.GLA 261.   31.6 293.      20.8      32.8   53.6
#> 3       3    18     NA ABIE.BAL  55.5  10.2  65.7     10.5      10.8   21.3
#> 4       4    35     27 PINU.BAN 488.   29.6 518.      18.7      38.4   57.1
#> # ℹ 2 more variables: Btotal <dbl>, model_id <chr>
```

## References

Lambert, M.C., Ung, C.H., Raulier, F., 2005. Canadian national tree
aboveground biomass equations. Canadian Journal of Forest Research 35,
1996–2018. <https://doi.org/10.1139/x05-112>

Ung, C.-H., Bernier, P., Guo, X.-J., 2008. Canadian national biomass
equations: New parameter estimates that include British Columbia data.
Canadian Journal of Forest Research 38, 1123–1132.
<https://doi.org/10.1139/X07-224>
