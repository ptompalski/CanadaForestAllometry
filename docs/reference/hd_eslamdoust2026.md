# Eslamdoust et al. (2026) DBH–height models for Ontario tree species

Vectorized implementation of the population-level individual-tree
height–diameter models M4 and M5 of Eslamdoust et al. (2026) for eight
tree species of the boreal and mixed (Great Lakes–St. Lawrence) forests
of Ontario. Predicts total tree height (m) from DBH (cm) alone (M4) or
from DBH and three stand-level predictors: dominant stand height, basal
area and stand density (M5).

## Usage

``` r
hd_eslamdoust2026(
  DBH,
  species,
  HDOM = NULL,
  BA = NULL,
  TPH = NULL,
  keep_model_id = FALSE
)
```

## Arguments

- DBH:

  Numeric vector. Tree level. Diameter at breast height (1.3 m above
  ground), in cm. Must be `>= 0`.

- species:

  Character vector. Tree level. NFI species codes (e.g. `"ACER.SAH"`;
  compact `"ACERSAH"` is accepted).

- HDOM:

  Optional numeric vector. Plot level. Stand height (m): "mean height of
  dominant and co-dominant trees" in the plot (source "SHT", p. 219).
  The source does not state whether all species or only the subject
  species are included. Must be `> 0`. Supply with `BA` and `TPH` to use
  M5.

- BA:

  Optional numeric vector. Plot level. Stand basal area (m²/ha) (source
  "BAH", p. 219 and p. 220). The source does not state a minimum DBH or
  species basis for this total. Must be `> 0`.

- TPH:

  Optional numeric vector. Plot level. Number of trees per hectare "with
  DBH\\\ge\\ 2.5cm" (source "TPH", p. 220). Must be `> 0`.

- keep_model_id:

  Logical. If `TRUE`, add column `model_id` (`"eslamdoust2026_m4"` or
  `"eslamdoust2026_m5"`).

## Value

A tibble with column `height` (m) and, if `keep_model_id = TRUE`,
`model_id`.

## Details

**Model scope (species coverage):** eastern white pine (`PINU.STR`),
eastern white cedar (`THUJ.OCC`), sugar maple (`ACER.SAH`), yellow birch
(`BETU.ALL`), American beech (`FAGU.GRA`), tamarack (`LARI.LAR`),
basswood (`TILI.AME`) and eastern hemlock (`TSUG.CAN`). Fitted to
Ontario growth-and-yield and NFI plots in natural stands (Table 1, p.
219), with stand basal area \\\ge\\ 5 m²/ha.

**Model forms** (eqs. 4 and 5, p. 220): \$\$M4: H = 1.3 + \alpha_0 \[1 -
\exp(-\alpha_1 DBH)\]^{\alpha_2};\$\$ \$\$M5: H = 1.3 + \alpha_0 \\
HDOM^{\alpha_1} \[1 - \exp(-\alpha_2 (TPH / BA)^{\alpha_3}
DBH)\]^{\alpha_4}.\$\$

**Height convention:** total tree height; DBH measured at 1.3 m, so
predicted height tends to 1.3 m as DBH tends to 0.

**Population-level prediction:** coefficients are the fixed effects of
the mixed-effects fits (Tables 4 and 5, p. 225), evaluated with the
plot-level random effect on \\\alpha_0\\ set to zero (\\u = 0\\), as in
the "fixed response" curves of Fig. 3. Plot-specific calibration with
measured heights is not implemented.

**Variants:** if `HDOM`, `BA` and `TPH` are all `NULL` (default), M4
(DBH only) is used. If all three are supplied, M5 is used for every
species. Supplying only some of them is an error. A one-time message
reports the variant used. The authors note that M4 performed as well as
or better than M5 for yellow birch and eastern hemlock (and basswood)
and that M4 "may be considered a more parsimonious and operationally
practical model in cases where stand-level variables are unavailable or
unreliable" (p. 222, p. 225).

**Tamarack and basswood (M5):** the stand-density exponent \\\alpha_3\\
is reported as 0 (not significant; Table 5), so `BA` and `TPH` do not
change their predicted heights; they are still required for M5.

**Corrected coefficient:** the tamarack M5 \\\alpha_2\\ is printed as
0.9793 in Table 5, which gives a near-flat curve (about 16 m at DBH 2.5
cm) inconsistent with Fig. 3b and the fitting data (Table 1). It is
treated as a misprint and 0.09793 is used, which reproduces the tamarack
curve in Fig. 3b. This value is inferred, not published.

The authors report increased bias for DBH \> 50 cm, particularly for
white pine, sugar maple, beech and hemlock (p. 225).

Rows with `NA` in any input return `NA` height. Inputs outside the
per-species range of the fitting data (Table 1: DBH and, for M5, `HDOM`,
`BA` and `TPH`) are computed and reported in a single warning.

## References

Eslamdoust, J., Vasiliauskas, S., Little, T., and Ashiq, M. W. (2026).
Height–diameter modeling of tree species in boreal and mixed forests
using a mixed-effects approach and stand-level variables. Forest Science
and Technology, 22(1), 217–230.
[doi:10.1080/21580103.2025.2571222](https://doi.org/10.1080/21580103.2025.2571222)

## See also

[`hd_model_registry()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_model_registry.md),
[`get_hd_model_info()`](https://ptompalski.github.io/CanadaForestAllometry/reference/get_hd_model_info.md)

## Examples

``` r
# M4: DBH only
hd_eslamdoust2026(DBH = c(10, 25, 40), species = c("ACER.SAH", "PINU.STR", "TSUG.CAN"))
#> hd_eslamdoust2026(): `HDOM`, `BA` and `TPH` not supplied; using model M4 (DBH
#> only).
#> This message is displayed once per session.
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   12.8
#> 2   19.2
#> 3   20.6

# M5: with dominant height, basal area and stems/ha
hd_eslamdoust2026(
  DBH = c(10, 25, 40),
  species = c("ACER.SAH", "PINU.STR", "TSUG.CAN"),
  HDOM = 22, BA = 30, TPH = 1200,
  keep_model_id = TRUE
)
#> hd_eslamdoust2026(): `HDOM`, `BA` and `TPH` supplied; using model M5 (DBH and
#> stand variables).
#> This message is displayed once per session.
#> # A tibble: 3 × 2
#>   height model_id         
#>    <dbl> <chr>            
#> 1   13.7 eslamdoust2026_m5
#> 2   19.6 eslamdoust2026_m5
#> 3   20.8 eslamdoust2026_m5

# Inside dplyr::mutate() with tree and plot data
trees <- data.frame(
  DBH = c(12, 25, 18),
  species = c("FAGU.GRA", "BETU.ALL", "THUJ.OCC"),
  HDOM = c(20, 20, 15), BA = c(28, 28, 40), TPH = c(1500, 1500, 3000)
)
dplyr::mutate(trees, hd_eslamdoust2026(DBH, species, HDOM, BA, TPH))
#>   DBH  species HDOM BA  TPH    height
#> 1  12 FAGU.GRA   20 28 1500 13.300521
#> 2  25 BETU.ALL   20 28 1500 20.650416
#> 3  18 THUJ.OCC   15 40 3000  9.243844
```
