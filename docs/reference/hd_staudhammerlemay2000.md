# Staudhammer and LeMay (2000) DBH–height models for coastal British Columbia

Vectorized implementation of the DBH–height models of Staudhammer and
LeMay (2000) for western redcedar (`THUJ.PLI`), red alder (`ALNU.RUB`),
Douglas-fir (`PSEU.MEN`) and western hemlock (`TSUG.HET`). Predicts
total tree height (m) from DBH (cm) alone, or from DBH and stand-density
measures.

## Usage

``` r
hd_staudhammerlemay2000(
  DBH,
  species,
  variant = c("dbh", "density"),
  TPH = NULL,
  BA = NULL,
  DMAX = NULL,
  keep_model_id = FALSE
)
```

## Arguments

- DBH:

  Numeric vector. Tree level. Diameter at breast height (1.3 m), in cm.
  Must be `>= 0`. Only trees above 2 cm DBH were recorded (p. 304).

- species:

  Character vector. Tree level. NFI species codes (e.g. `"PSEU.MEN"`;
  compact `"PSEUMEN"` is accepted). Supported: `ALNU.RUB`, `PSEU.MEN`,
  `THUJ.PLI`, `TSUG.HET`.

- variant:

  Model variant: `"dbh"` (default; eq. 2) or `"density"` (eq. 2a for red
  alder, eq. 2b for the other species).

- TPH:

  Optional numeric vector. Plot level. Stems per hectare, all species
  combined (source "TSTEMSHA", p. 304; trees with DBH \> 2 cm, live or
  dead status not stated). Must be `> 0`. Required by
  `variant = "density"` for red alder.

- BA:

  Optional numeric vector. Plot level. Basal area per hectare (m²/ha),
  all species combined (source "TBAHA", p. 304; same tree basis as
  `TPH`). Must be `> 0`. Required by `variant = "density"` for red
  alder.

- DMAX:

  Optional numeric vector. Plot level. Maximum DBH (cm) of all trees on
  the plot, all species; used to compute the relative DBH (source
  "REL_dbh" = "dbh divided by the maximum dbh for all species", p. 304).
  Must be `> 0` and `>= DBH`. Required by `variant = "density"` for
  western redcedar, Douglas-fir and western hemlock.

- keep_model_id:

  Logical. If `TRUE`, add column `model_id`
  (`"staudhammerlemay2000_dbh"` or `"staudhammerlemay2000_density"`).

## Value

A tibble with column `height` (m) and, if `keep_model_id = TRUE`,
`model_id`.

## Details

**Model scope:** fitted to the 1995 inventory of the UBC Malcolm Knapp
Research Forest (about 5000 ha, Coastal Western Hemlock zone, coastal
BC): 327 plots in 82 clusters, heights measured on a subjectively chosen
subset of 763 trees (p. 304). The source alder model ("D") combines
*Alnus rubra* and *A. tenuifolia*; it is used here for red alder
(`ALNU.RUB`) only. Alder had only 28 trees on 16 plots.

**Model form** (eq. 2, Yang et al. 1978; p. 305): \$\$H = 1.3 + E_1
\[1 - \exp(E_2 \\ DBH^{E_3})\].\$\$ The intercept is constrained to 1.3
m at DBH = 0.

**Variants** (chosen with `variant`):

- `"dbh"` (default): eq. 2, with \\E_1\\, \\E_2\\, \\E_3\\ species
  constants (Table 5, p. 308). All four species.

- `"density"`: parameter prediction (Table 5, p. 308). For red alder
  (eq. 2a), \\E_k = c\_{k0} + c\_{k1} TPH + c\_{k2} BA\\. For western
  redcedar, Douglas-fir and western hemlock (eq. 2b), \\E_k = c\_{k0} +
  c\_{k1} R + c\_{k2} R^2\\, where \\R = DBH / DMAX\\ is the tree's
  relative DBH. `TPH` and `BA` are required only when the data include
  red alder, and `DMAX` only when they include the other species.

Within a plot (fixed `DMAX`), eq. 2b heights increase with DBH up to a
relative DBH of about 0.85–0.95 and then decrease slightly for the
plot's largest trees; this follows from the published quadratic in
\\R\\. Predictors not used by the selected variant or species are
ignored with a warning. Coefficients are ordinary nonlinear
least-squares fits (SAS PROC NLIN); no calibration information is
published.

**Corrected coefficient:** for western redcedar (eq. 2b), the slope of
\\E_2\\ on \\R^2\\ is printed as 0.1551. With that value \\E_2\\ becomes
positive for \\R \> 0.23\\, giving negative heights (e.g. -47 m at DBH =
30 cm, \\R\\ = 0.5), which is inconsistent with the reported fit (RMSE
4.308 m, Table 2). The value 0.01551 (a dropped zero) is used: it gives
plausible heights and matches the DBH-only \\E_2\\ at \\R = 1\\. This
correction is an inference, not a published erratum.

**Validity:** far outside the fitting data the `"density"` parameters
can become invalid (\\E_1 \le 0\\, \\E_2 \ge 0\\ or \\E_3 \le 0\\):
Douglas-fir with \\R \< 0.062\\, redcedar with \\R \< 0.019\\, hemlock
with \\R \< 0.008\\, and red alder with `TPH` above about 7900 stems/ha
or large `BA` with low `TPH`. Such rows raise an error; use
`variant = "dbh"` for them. Inputs outside the per-species ranges of the
fitting data (Table 1, p. 304: DBH, and for `"density"` also `TPH`, `BA`
and \\R\\) are computed and reported in a single warning. Rows with `NA`
in any used input return `NA` height. The source has no worked example,
so the implementation is checked for plausibility only.

## References

Staudhammer, C., and LeMay, V. (2000). Height prediction equations using
diameter and stand density measures. The Forestry Chronicle, 76(2),
303–309. [doi:10.5558/tfc76303-2](https://doi.org/10.5558/tfc76303-2)

## See also

[`hd_model_registry()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_model_registry.md),
[`get_hd_model_info()`](https://ptompalski.github.io/CanadaForestAllometry/reference/get_hd_model_info.md)

## Examples

``` r
# DBH only (eq. 2)
hd_staudhammerlemay2000(
  DBH = c(10, 30, 60),
  species = c("PSEU.MEN", "TSUG.HET", "THUJ.PLI")
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   9.44
#> 2  25.2 
#> 3  30.5 

# Stand density: relative DBH for conifers, stems/ha and basal area for alder
hd_staudhammerlemay2000(
  DBH = c(10, 30, 60, 15),
  species = c("PSEU.MEN", "TSUG.HET", "THUJ.PLI", "ALNU.RUB"),
  variant = "density",
  TPH = 1500, BA = 50, DMAX = 80,
  keep_model_id = TRUE
)
#> # A tibble: 4 × 2
#>   height model_id                    
#>    <dbl> <chr>                       
#> 1   2.99 staudhammerlemay2000_density
#> 2  29.3  staudhammerlemay2000_density
#> 3  29.7  staudhammerlemay2000_density
#> 4  18.1  staudhammerlemay2000_density

# Inside dplyr::mutate() with tree and plot data
trees <- data.frame(
  DBH = c(12, 45, 80),
  species = c("TSUG.HET", "PSEU.MEN", "PSEU.MEN"),
  DMAX = 80
)
dplyr::mutate(
  trees,
  hd_staudhammerlemay2000(DBH, species, variant = "density", DMAX = DMAX)
)
#>   DBH  species DMAX   height
#> 1  12 TSUG.HET   80 11.25201
#> 2  45 PSEU.MEN   80 31.01389
#> 3  80 PSEU.MEN   80 41.72672
```
