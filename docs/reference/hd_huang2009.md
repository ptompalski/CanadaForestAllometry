# Huang, Meng and Yang (2009) DBH–height models for Alberta black spruce

Vectorized implementation of the population-level Chapman–Richards
height–diameter models of Huang, Meng and Yang (2009) for black spruce
(`PICE.MAR`) in Alberta. Predicts total tree height (m) from DBH (cm)
alone (base model 9), or from DBH and top height (model 10), or from
DBH, top height and stand basal area (model 11).

## Usage

``` r
hd_huang2009(DBH, species, HTOP = NULL, BA = NULL, keep_model_id = FALSE)
```

## Arguments

- DBH:

  Numeric vector. Tree level. Diameter at breast height (1.3 m above
  ground), in cm. Must be `>= 0`.

- species:

  Character vector. Tree level. NFI species code; only `"PICE.MAR"` is
  supported (compact `"PICEMAR"` is accepted).

- HTOP:

  Optional numeric vector. Plot level. Top height (m): "the average
  height of the 100 largest DBH trees per ha" (source "TopH", p. 5).
  This is not the mean height of dominant and codominant trees (`HDOM`).
  Must be `> 0`. Supply to use model 10, or with `BA` for model 11.

- BA:

  Optional numeric vector. Plot level. Stand basal area per ha (m²/ha)
  (source "BA", p. 5). The source does not state a minimum DBH or
  species basis for this total. Must be `> 0`. Requires `HTOP`.

- keep_model_id:

  Logical. If `TRUE`, add column `model_id` (`"huang2009_m9"`,
  `"huang2009_m10"` or `"huang2009_m11"`).

## Value

A tibble with column `height` (m) and, if `keep_model_id = TRUE`,
`model_id`.

## Details

**Model scope:** black spruce only. Fitted to 93 Alberta permanent
sample plots (917 trees) in the Lower Foothills natural region and
validated on 71 plots in the Boreal Mixedwood and Upper Foothills (p.
4).

**Model forms** (eqs. 9–11, pp. 4–5): \$\$(9): H = 1.3 + \beta_1 \[1 -
\exp(-\beta_2 DBH)\]^{\beta_3};\$\$ \$\$(10): H = 1.3 + (\beta_1 +
\beta_4 HTOP) \[1 - \exp(-\beta_2 DBH)\]^{\beta_3};\$\$ \$\$(11): H =
1.3 + (\beta_1 + \beta_4 HTOP + \beta_5 BA) \[1 - \exp(-\beta_2
DBH)\]^{\beta_3}.\$\$

**Height convention:** total tree height; DBH measured at 1.3 m, so
predicted height tends to 1.3 m as DBH tends to 0.

**Population-level prediction:** coefficients are the fixed effects of
the nonlinear mixed models (Table 2, p. 13), evaluated with the plot
random effects on \\\beta_1\\ and \\\beta_3\\ set to zero (the source's
"fixed only" predictions, Table 3). Plot-specific calibration with
measured heights is not implemented. The authors report that fixed-only
predictions are biased for individual plots and on average (Table 4:
mean bias −7.5%, −2.1% and −3.3% of mean height on the validation data
for models 9, 10 and 11), and that the base model (9) gave the most
accurate calibrated predictions despite ranking last by AIC/BIC. The
ratio correction they describe (eqs. 29–30) has no published value and
is not applied.

**Variants:** if `HTOP` and `BA` are `NULL` (default), model 9 is used;
if only `HTOP` is supplied, model 10; if both are supplied, model 11.
Supplying `BA` without `HTOP` is an error. A one-time message reports
the variant used.

Rows with `NA` in any input return `NA` height. Inputs outside the range
of the fitting data (Table 1: DBH and, where used, `HTOP` and `BA`) are
computed and reported in a single warning.

## References

Huang, S., Meng, S. X., and Yang, Y. (2009). Using nonlinear mixed model
technique to determine the optimal tree height prediction model for
black spruce. Modern Applied Science, 3(4), 3–18.
[doi:10.5539/mas.v3n4p3](https://doi.org/10.5539/mas.v3n4p3)

## See also

[`hd_model_registry()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_model_registry.md),
[`get_hd_model_info()`](https://ptompalski.github.io/CanadaForestAllometry/reference/get_hd_model_info.md),
[`hd_huang2016()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_huang2016.md)

## Examples

``` r
# Model 9: DBH only
hd_huang2009(DBH = c(7.9, 10.7, 20.3), species = "PICE.MAR")
#> hd_huang2009(): using model 9 (DBH only).
#> This message is displayed once per session.
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   8.58
#> 2  10.7 
#> 3  16.5 

# Model 10: with top height
hd_huang2009(DBH = c(7.9, 10.7, 20.3), species = "PICE.MAR", HTOP = 14.5)
#> hd_huang2009(): using model 10 (DBH and top height `HTOP`).
#> This message is displayed once per session.
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   8.50
#> 2  10.6 
#> 3  15.5 

# Model 11: with top height and basal area
hd_huang2009(
  DBH = c(7.9, 10.7, 20.3), species = "PICE.MAR",
  HTOP = 14.5, BA = 7.75, keep_model_id = TRUE
)
#> hd_huang2009(): using model 11 (DBH, top height `HTOP` and basal area `BA`).
#> This message is displayed once per session.
#> # A tibble: 3 × 2
#>   height model_id     
#>    <dbl> <chr>        
#> 1   8.45 huang2009_m11
#> 2  10.5  huang2009_m11
#> 3  15.2  huang2009_m11

# Inside dplyr::mutate() with tree and plot data
trees <- data.frame(
  DBH = c(8, 15, 22),
  species = "PICE.MAR",
  HTOP = c(14, 14, 19), BA = c(10, 10, 25)
)
dplyr::mutate(trees, hd_huang2009(DBH, species, HTOP, BA))
#>   DBH  species HTOP BA    height
#> 1   8 PICE.MAR   14 10  8.433155
#> 2  15 PICE.MAR   14 10 12.822909
#> 3  22 PICE.MAR   19 25 18.575681
```
