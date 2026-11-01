# Rijal, Weiskittel and Kershaw (2012) DBH–height models for the Acadian Region

Vectorized implementation of the regional height–diameter equations of
Rijal, Weiskittel and Kershaw (2012) for 15 tree species of the North
American Acadian Region (Maine, New Brunswick, Nova Scotia, Prince
Edward Island and southern Québec). Predicts total tree height (m) from
DBH (cm), a climate-derived site index, stand crown competition factor
and the tree's basal area in larger trees.

## Usage

``` r
hd_rijal2012(
  DBH,
  species,
  CSI,
  CCF,
  BAL,
  variant = c("gnls", "nlme"),
  keep_model_id = FALSE
)
```

## Arguments

- DBH:

  Numeric vector. Tree level. Diameter at breast height, in cm. Must be
  `>= 0`.

- species:

  Character vector. Tree level. NFI species codes (e.g. `"PICE.RUB"`;
  compact `"PICERUB"` is accepted).

- CSI:

  Numeric vector. Plot level. Climate-derived site index (m): "a
  climate-derived estimate of site index (CSI) ... based on a
  non-parametric regression model that relates observed site index to
  local climate variables (Weiskittel et al., 2011b)" (source "CSI", p.
  381). Units and base age are not stated; metres are assumed (they
  reproduce the observed mean heights). Must be `> 0`.

- CCF:

  Numeric vector. Plot level. Stand crown competition factor (Krajicek
  et al. 1961), computed in the source with the maximum crown width
  equations of Russell and Weiskittel (2011) (source "CCF", pp.
  380–381). Minimum tree size not stated. Must be `>= 0`.

- BAL:

  Numeric vector. Tree level. Basal area of trees larger than the
  subject tree (source "BAL", p. 381). Units are not stated; m²/ha is
  assumed. Species basis and minimum DBH are not stated. Must be `>= 0`.

- variant:

  Coefficient set: `"gnls"` (default; Table 5) or `"nlme"` (Table 6
  fixed effects, random effects = 0).

- keep_model_id:

  Logical. If `TRUE`, add column `model_id` (`"rijal2012_gnls"` or
  `"rijal2012_nlme"`).

## Value

A tibble with column `height` (m) and, if `keep_model_id = TRUE`,
`model_id`.

## Details

**Model scope (species coverage):** American beech (`FAGU.GRA`), grey
birch (`BETU.POP`), paper birch (`BETU.PAP`), trembling aspen
(`POPU.TRE`), red maple (`ACER.RUB`), northern red oak (`QUER.RUB`),
sugar maple (`ACER.SAH`), yellow birch (`BETU.ALL`), balsam fir
(`ABIE.BAL`), black spruce (`PICE.MAR`), eastern hemlock (`TSUG.CAN`),
red spruce (`PICE.RUB`), eastern white cedar (`THUJ.OCC`), eastern white
pine (`PINU.STR`) and white spruce (`PICE.GLA`). Fitted to permanent and
research plots, mostly in mixed-species, uneven-aged stands (pp.
380–381).

**Model form** (eq. 4, p. 381): \$\$HT = 1.37 + (c_0 + CSI^{c_3}) \[1 -
\exp(-c_1 DBH)\]^{c_2 + c_4 \ln(CCF + 1) + c_5 BAL}.\$\$

**Variants** (chosen with `variant`): `"gnls"` (default) uses the
generalized nonlinear least-squares coefficients (Table 5, p. 387),
which the authors recommend "when no observations are available for
local calibration" (p. 386). `"nlme"` uses the fixed effects of the
nonlinear mixed-effects fit (Table 6, p. 387) with the source, stand and
year random effects on \\c_0\\ set to zero; without local calibration
these were more biased than GNLS (p. 386). For grey birch and northern
red oak the NLME BAL coefficient was not significant (printed "–") and
is zero.

**Height convention:** total tree height. The 1.37 m intercept (US
breast height) is used for all data, although Canadian DBH was measured
at 1.3 m; the authors judged the difference negligible (p. 381).
Predicted height is 1.37 m at DBH 0.

Rows with `NA` in any input return `NA` height. DBH outside the
per-species range of the fitting data (Table 1, p. 381) is computed and
reported in a single warning. The source gives no ranges for `CSI`,
`CCF` or `BAL`; inputs extreme enough to make the exponent non-positive
raise an error.

## References

Rijal, B., Weiskittel, A.R., and Kershaw, J.A., Jr. (2012). Development
of regional height to diameter equations for 15 tree species in the
North American Acadian Region. Forestry, 85(3), 379–389.
[doi:10.1093/forestry/cps036](https://doi.org/10.1093/forestry/cps036)

## See also

[`hd_model_registry()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_model_registry.md),
[`get_hd_model_info()`](https://ptompalski.github.io/CanadaForestAllometry/reference/get_hd_model_info.md)

## Examples

``` r
hd_rijal2012(
  DBH = c(10, 25, 40),
  species = c("PICE.RUB", "ABIE.BAL", "ACER.RUB"),
  CSI = 15, CCF = 150, BAL = c(20, 8, 2)
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   8.08
#> 2  16.1 
#> 3  18.8 

# Mixed-model fixed effects
hd_rijal2012(25, "PICE.RUB", CSI = 15, CCF = 150, BAL = 10,
             variant = "nlme", keep_model_id = TRUE)
#> # A tibble: 1 × 2
#>   height model_id      
#>    <dbl> <chr>         
#> 1   15.5 rijal2012_nlme

# Inside dplyr::mutate() with tree and plot data
trees <- data.frame(
  DBH = c(12, 25, 18),
  species = c("PICE.RUB", "TSUG.CAN", "BETU.ALL"),
  CSI = c(14, 14, 17), CCF = c(180, 180, 120), BAL = c(18, 6, 12)
)
dplyr::mutate(trees, hd_rijal2012(DBH, species, CSI, CCF, BAL))
#>   DBH  species CSI CCF BAL    height
#> 1  12 PICE.RUB  14 180  18  9.243432
#> 2  25 TSUG.CAN  14 180   6 14.164134
#> 3  18 BETU.ALL  17 120  12 12.442150
```
