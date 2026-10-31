# Sharma and Parton (2007) DBH–height models for Ontario boreal tree species

Vectorized implementation of the population-level individual-tree
height–diameter models of Sharma and Parton (2007) for boreal tree
species in northern Ontario. Predicts total tree height (m) from DBH
(cm) and three stand-level predictors: dominant stand height, basal area
and stand density.

## Usage

``` r
hd_sharmaparton2007(DBH, species, HDOM, BA, TPH, keep_model_id = FALSE)
```

## Arguments

- DBH:

  Numeric vector. Tree level. Diameter at breast height (1.3 m above
  ground), in cm. Must be `>= 0`.

- species:

  Character vector. Tree level. NFI species codes (e.g. `"PICE.MAR"`;
  compact `"PICEMAR"` is accepted).

- HDOM:

  Numeric vector. Plot level. Dominant stand height (m): the "mean
  height of sampled dominant and codominant trees in a plot,
  irrespective of species" (source "SHT", p. 190). Must be `> 0`.

- BA:

  Numeric vector. Plot level. Total stand basal area (m²/ha) of all
  trees in the plot (source "BA/ha", p. 188 and p. 190). The source does
  not state a minimum DBH for this total. Must be `> 0`.

- TPH:

  Numeric vector. Plot level. Stand density (stems/ha), "calculated from
  the trees with DBH greater than or equal to 2.5 cm" (source "N/ha", p.
  188). Must be `> 0`.

- keep_model_id:

  Logical. If `TRUE`, add column `model_id` (`"sharmaparton2007"`).

## Value

A tibble with column `height` (m) and, if `keep_model_id = TRUE`,
`model_id`.

## Details

**Model scope (species coverage):** balsam fir (`ABIE.BAL`), balsam
poplar (`POPU.BAL`), black spruce (`PICE.MAR`), jack pine (`PINU.BAN`),
red pine (`PINU.RES`), trembling aspen (`POPU.TRE`), white birch
(`BETU.PAP`) and white spruce (`PICE.GLA`). Fitted to permanent growth
study plots in northern Ontario; tree origin (natural or planted) was
recorded (p. 188), but the source does not state whether planted trees
were excluded.

**Model form** (eqs. 6 and 8, p. 190 and p. 192): \$\$H = 1.3 + \theta
\\ HDOM^{\delta} \[1 - \exp(-\beta (TPH / BA)^{\phi}
DBH)\]^{\gamma}.\$\$

**Height convention:** total tree height; DBH measured at 1.3 m, so
predicted height tends to 1.3 m as DBH tends to 0.

**Population-level prediction:** coefficients are the fixed effects of
the mixed-effects fit (Method 2, Table 6, p. 195), evaluated with the
plot-level random effect on the asymptote set to zero (\\u = 0\\). This
gives the curve of a "typical" plot with the supplied stand
characteristics, as shown in Fig. 2 of the source. Plot-specific
calibration with measured heights (eq. 7) is not implemented.

Rows with `NA` in any input return `NA` height. Inputs outside the range
of the fitting data (per-species DBH, Table 1; pooled plot BA, TPH and
HDOM ranges, p. 188) are computed and reported in a single warning.

## References

Sharma, M., and Parton, J. (2007). Height–diameter equations for boreal
tree species in Ontario using a mixed-effects modeling approach. Forest
Ecology and Management, 249(3), 187–198.
[doi:10.1016/j.foreco.2007.05.006](https://doi.org/10.1016/j.foreco.2007.05.006)

## See also

[`hd_model_registry()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_model_registry.md),
[`get_hd_model_info()`](https://ptompalski.github.io/CanadaForestAllometry/reference/get_hd_model_info.md)

## Examples

``` r
hd_sharmaparton2007(
  DBH = c(10, 20, 30),
  species = c("PICE.MAR", "PINU.BAN", "POPU.TRE"),
  HDOM = 20, BA = 25, TPH = 2500
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   11.9
#> 2   20.6
#> 3   22.3

# Inside dplyr::mutate() with tree and plot data
trees <- data.frame(
  DBH = c(12, 25, 18),
  species = c("ABIE.BAL", "PICE.GLA", "BETU.PAP"),
  HDOM = c(16, 16, 22), BA = c(30, 30, 18), TPH = c(3000, 3000, 1200)
)
dplyr::mutate(trees, hd_sharmaparton2007(DBH, species, HDOM, BA, TPH))
#>   DBH  species HDOM BA  TPH   height
#> 1  12 ABIE.BAL   16 30 3000 11.08370
#> 2  25 PICE.GLA   16 30 3000 16.97600
#> 3  18 BETU.PAP   22 18 1200 18.25566
```
