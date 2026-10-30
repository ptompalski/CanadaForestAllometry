# Auger, DeBlois & Lambert (2025) DBH–height models for Québec plantations

Vectorized implementation of the DBH–height relationships of Auger,
DeBlois and Lambert (2025; Avis technique SSS-10) for black spruce and
jack pine **plantations** in Québec. Predicts total tree height (m) from
DBH (cm) alone, or from DBH and a plot-level predictor: the plot mean
DBH, or the dominant height and dominant DBH.

## Usage

``` r
hd_auger2025(
  DBH,
  species,
  variant = c("dbh", "plot_mean_dbh", "dominant"),
  mean_DBH = NULL,
  HDOM = NULL,
  dominant_DBH = NULL,
  keep_model_id = FALSE
)
```

## Arguments

- DBH:

  Numeric vector. Tree level. Diameter at breast height (1.3 m above
  ground), in cm. Must be `> 0`.

- species:

  Character vector. Tree level. NFI species codes, `"PICE.MAR"` or
  `"PINU.BAN"` (compact `"PICEMAR"` is accepted).

- variant:

  Model variant: `"dbh"` (default), `"plot_mean_dbh"` or `"dominant"`.
  See Details.

- mean_DBH:

  Numeric vector. Plot level. Mean DBH (cm) of the plot, used by
  `variant = "plot_mean_dbh"`: "le DHP moyen de tous les arbres de la
  placette" (source "DMOY", p. 6), i.e. the arithmetic (not quadratic)
  mean DBH of all trees in the plot. The source does not state whether
  natural ingress is included or a minimum DBH. Must be `>= 0`.

- HDOM:

  Numeric vector. Plot level. Dominant height (m), used by
  `variant = "dominant"`: mean height of the dominant trees of the plot
  (source "HDOM", p. 7), the dominant trees being "les 8 plus hauts
  arbres plantés faisant partie de l'étage des arbres dominants et bien
  répartis dans la placette (200 arbres/ha)" (400 m² plots, p. 2). Must
  be `> 0`.

- dominant_DBH:

  Numeric vector. Plot level. Mean DBH (cm) of the same dominant trees
  (source "DDOM", p. 7), used by `variant = "dominant"`. Must be `>= 0`.

- keep_model_id:

  Logical. If `TRUE`, add column `model_id` (`"auger2025_dbh"`,
  `"auger2025_plot_mean_dbh"` or `"auger2025_dominant"`).

## Value

A tibble with column `height` (m) and, if `keep_model_id = TRUE`,
`model_id`.

## Details

**Model scope:** black spruce (`PICE.MAR`, source "EPN") and jack pine
(`PINU.BAN`, source "PIG") in monospecific plantations of the Québec
plantation monitoring network (p. 2). The models were not fitted to
natural stands; stand origin cannot be checked by the function.

**Variants** (chosen with `variant`):

- `"dbh"` (default):

  Korf form, eq. 10 (Tableau 7, p. 11), both species: \$\$H = 1.3 +
  \alpha_1 \exp(-\alpha_2 DBH^{-\alpha_3}).\$\$ Estimates the mean
  height of the population for a given DBH; the authors note that its
  shape is unlikely to suit any particular plot (pp. 4–5).

- `"plot_mean_dbh"`:

  Requires `mean_DBH`. Jack pine, eq. 11 (Chapman–Richards) and black
  spruce, eq. 12 (Hossfeld II) (Tableau 8, p. 14): \$\$PIG: H = 1.3 +
  (\alpha\_{10} + \alpha\_{11} MD) \[1 - \exp(-(\alpha\_{20} +
  \alpha\_{21} MD) DBH)\]^{\alpha_3};\$\$ \$\$EPN: H = 1.3 +
  (\alpha\_{10} + \alpha\_{11} MD) \frac{DBH^{\alpha_3}}{(\alpha\_{20} +
  \alpha\_{21} MD) + DBH^{\alpha_3}},\$\$ where \\MD\\ is `mean_DBH`.

- `"dominant"`:

  Requires `HDOM` and `dominant_DBH`. Jack pine, eq. 13 and black
  spruce, eq. 14 (Tableau 9, p. 17), the same forms with
  \\\alpha\_{10} + \alpha\_{11} HDOM\\ as asymptote term and
  \\\alpha\_{20} + \alpha\_{22} DD\\ as rate/scale term, where \\DD\\ is
  `dominant_DBH`.

Predictors not used by the selected variant are ignored with a warning.

**Height convention:** total tree height; DBH is measured at 1.3 m and
every form tends to 1.3 m as DBH tends to 0.

**Fit:** weighted fixed-effects nonlinear regressions; a plot random
effect did not improve the fit and none was included (pp. 5–7), so the
published coefficients are used directly. Variance-function parameters
are not used.

**Korf asymptote:** the published \\\alpha_1\\ values are very large
(1,630,094 m for jack pine and 101,489 m for black spruce), so the
`"dbh"` curves keep increasing beyond the data. Extrapolation beyond the
calibration DBH range is not recommended.

Rows with `NA` in any used input return `NA` height. Inputs outside the
per-species calibration ranges (Tableau 1, p. 4: DBH and, depending on
the variant, mean DBH, dominant height and dominant DBH) are computed
and reported in a single warning.

No worked example or table of predicted heights is published; the
implementation is checked for plausibility only (see
`data-raw/Auger2025_model-spec.md`).

## References

Auger, I., DeBlois, J., and Lambert, M.-C. (2025). Relations
hauteur-diamètre, tarifs de cubage et équations de défilement pour
l'épinette noire et le pin gris en plantation au Québec. Gouvernement du
Québec, ministère des Ressources naturelles et des Forêts, Direction de
la recherche forestière. Avis technique SSS-10. 31 p.
[doi:10.82204/dcwg-5v65](https://doi.org/10.82204/dcwg-5v65)

## See also

[`hd_model_registry()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_model_registry.md),
[`get_hd_model_info()`](https://ptompalski.github.io/CanadaForestAllometry/reference/get_hd_model_info.md)

## Examples

``` r
# DBH only (Korf)
hd_auger2025(DBH = c(5, 10, 20), species = c("PICE.MAR", "PINU.BAN", "PINU.BAN"))
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   4.34
#> 2   7.77
#> 3  15.0 

# With the plot mean DBH
hd_auger2025(
  DBH = c(5, 10, 20), species = "PINU.BAN",
  variant = "plot_mean_dbh", mean_DBH = 12
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   6.31
#> 2   9.49
#> 3  13.0 

# With dominant height and dominant DBH, inside dplyr::mutate()
trees <- data.frame(
  DBH = c(6, 9, 12),
  species = "PICE.MAR",
  HDOM = 9, dominant_DBH = 11
)
dplyr::mutate(
  trees,
  hd_auger2025(DBH, species, variant = "dominant",
               HDOM = HDOM, dominant_DBH = dominant_DBH,
               keep_model_id = TRUE)
)
#>   DBH  species HDOM dominant_DBH   height           model_id
#> 1   6 PICE.MAR    9           11 6.152723 auger2025_dominant
#> 2   9 PICE.MAR    9           11 7.731378 auger2025_dominant
#> 3  12 PICE.MAR    9           11 8.704548 auger2025_dominant
```
