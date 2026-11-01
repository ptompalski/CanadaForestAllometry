# Temesgen and von Gadow (2004) DBH–height models for interior British Columbia

Vectorized implementation of the generalized height–diameter models of
Temesgen and von Gadow (2004) for eight tree species of complex
(mixed-species, uneven-aged) stands in the Interior Douglas-fir (IDF)
biogeoclimatic zone of southern interior British Columbia. Predicts
total tree height (m) from DBH (cm) alone (Model 2) or from DBH, the
tree's basal area in larger trees and two stand-density predictors
(Model 7).

## Usage

``` r
hd_temesgen2004(
  DBH,
  species,
  BAL = NULL,
  TPH = NULL,
  BA = NULL,
  keep_model_id = FALSE
)
```

## Arguments

- DBH:

  Numeric vector. Tree level. Diameter outside bark at breast height
  (1.3 m above ground), in cm. Must be `>= 0`.

- species:

  Character vector. Tree level. NFI species codes (e.g. `"PSEU.MEN"`;
  compact `"PSEUMEN"` is accepted).

- BAL:

  Optional numeric vector. Tree level. Basal area in larger trees
  (m²/ha): basal area per hectare of the trees larger than the subject
  tree in its plot (source "BAL", p. 47). The source does not state the
  species basis or a minimum DBH (plots recorded trees with DBH \\\ge\\
  2 cm). Must be `>= 0`. Supply with `TPH` and `BA` to use Model 7.

- TPH:

  Optional numeric vector. Plot level. Number of trees per hectare
  (source "SPH", p. 47); minimum DBH not stated (plots recorded trees
  with DBH \\\ge\\ 2 cm). Must be `> 0`.

- BA:

  Optional numeric vector. Plot level. Basal area per hectare (m²/ha)
  (source "BA", p. 47); minimum DBH not stated. Must be `> 0`.

- keep_model_id:

  Logical. If `TRUE`, add column `model_id` (`"temesgen2004_m2"` or
  `"temesgen2004_m7"`).

## Value

A tibble with column `height` (m) and, if `keep_model_id = TRUE`,
`model_id`.

## Details

**Model scope (species coverage):** trembling aspen (`POPU.TRE`),
western redcedar (`THUJ.PLI`), paper birch (`BETU.PAP`), Douglas-fir
(`PSEU.MEN`), western larch (`LARI.OCC`), lodgepole pine (`PINU.CON`),
ponderosa pine (`PINU.PON`) and the interior spruce complex. The source
"spruce" combines Engelmann and white spruce (Table 1, p. 50); its
coefficients are used for `PICE.ENG`, `PICE.GLA` and the hybrid
`PICE.ENG.GLA`. Fitted to BC Ministry of Forests permanent sample plots
(49°–52°30′N) from which thinned, spaced and selectively logged plots
were excluded; the authors caution against using the models in such
stands.

**Model forms** (Models 2 and 7, pp. 46–47): \$\$H = 1.3 + a \[1 -
\exp(-b \\ DBH^{c})\],\$\$ with \\a\\, \\b\\, \\c\\ species constants
for Model 2 (Table 5, p. 51), and for Model 7 (Table 4, p. 50) \$\$a =
a_1 + a_2 BAL + a_3 TPH + a_4 BA, \quad c = a_5 + a_6 BAL.\$\$ Terms
dropped by the authors as non-significant (blank in Table 4) are zero;
e.g. Model 7 for ponderosa pine uses no stand predictor and is
practically identical to Model 2.

**Corrected exponent sign:** both equations are printed as \\1 - e^{b
D^{c}}\\. With the published (positive) \\b\\ this gives negative
heights, so the standard Weibull form of Yang et al. (1978), \\1 - e^{-b
D^{c}}\\, is used. It reproduces the observed mean heights of the
fitting data (Table 1).

**Height convention:** total tree height; DBH measured outside bark at
1.3 m, so predicted height tends to 1.3 m as DBH tends to 0.

**Variants:** if `BAL`, `TPH` and `BA` are all `NULL` (default), Model 2
(DBH only) is used. If all three are supplied, Model 7 is used for every
species, even for species where some or all of them have no effect.
Supplying only some of them is an error. A one-time message reports the
variant used. Coefficients are ordinary nonlinear least-squares fits
(SAS PROC NLIN); no calibration information is published.

Rows with `NA` in any input return `NA` height. DBH outside the
per-species range of the fitting data (Table 1) is computed and reported
in a single warning. The source gives no ranges for `BAL`, `TPH` or
`BA`; Model 7 inputs extreme enough to make \\a\\ or \\c\\ non-positive
raise an error.

## References

Temesgen, H., and von Gadow, K. (2004). Generalized height–diameter
models — an application for major tree species in complex stands of
interior British Columbia. European Journal of Forest Research, 123(1),
45–51.
[doi:10.1007/s10342-004-0020-z](https://doi.org/10.1007/s10342-004-0020-z)

## See also

[`hd_model_registry()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_model_registry.md),
[`get_hd_model_info()`](https://ptompalski.github.io/CanadaForestAllometry/reference/get_hd_model_info.md)

## Examples

``` r
# Model 2: DBH only
hd_temesgen2004(DBH = c(10, 25, 40), species = c("PSEU.MEN", "PINU.CON", "PICE.ENG"))
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   9.12
#> 2  21.8 
#> 3  27.7 

# Model 7: with basal area in larger trees, stems/ha and basal area
hd_temesgen2004(
  DBH = c(10, 25, 40),
  species = c("PSEU.MEN", "PINU.CON", "PICE.ENG"),
  BAL = c(20, 8, 2), TPH = 800, BA = 25,
  keep_model_id = TRUE
)
#> hd_temesgen2004(): `BAL`, `TPH` and `BA` supplied; using Model 7 (DBH and stand
#> variables).
#> This message is displayed once per session.
#> # A tibble: 3 × 2
#>   height model_id       
#>    <dbl> <chr>          
#> 1   9.29 temesgen2004_m7
#> 2  19.9  temesgen2004_m7
#> 3  24.7  temesgen2004_m7

# Inside dplyr::mutate() with tree and plot data
trees <- data.frame(
  DBH = c(12, 25, 18),
  species = c("PSEU.MEN", "LARI.OCC", "POPU.TRE"),
  BAL = c(18, 6, 12), TPH = c(900, 900, 600), BA = c(24, 24, 15)
)
dplyr::mutate(trees, hd_temesgen2004(DBH, species, BAL, TPH, BA))
#>   DBH  species BAL TPH BA   height
#> 1  12 PSEU.MEN  18 900 24 10.79649
#> 2  25 LARI.OCC   6 900 24 21.84368
#> 3  18 POPU.TRE  12 600 15 13.98348
```
