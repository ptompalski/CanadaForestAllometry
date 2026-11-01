# Fortin et al. (2009) DBH–height model for 20 Québec tree species

Vectorized implementation of the population-level height–diameter
relationship of Fortin, Bernier, Saucier & Labbé (2009; Mémoire de
recherche forestière no 153) for 20 commercial tree species of Québec.
Predicts total tree height (m) from DBH (cm), plot basal area, the
tree's social status, mean annual temperature, drainage, ecological
region and recent light disturbance. All species use the same
predictors. The newer
[`hd_auger2016()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_auger2016.md)
is the updated relation from the same Québec inventory (27 species).

## Usage

``` r
hd_fortin2009(
  DBH,
  species,
  BA,
  QMD,
  ecoregion,
  drainage,
  MAT,
  disturbance,
  keep_model_id = FALSE
)
```

## Arguments

- DBH:

  Numeric vector. Tree level. Diameter at breast height (1.3 m), in cm
  (source "dhp"). Must be `>= 0`.

- species:

  Character vector. Tree level. NFI species codes (e.g. `"PICE.MAR"`;
  compact `"PICEMAR"` is accepted). Supported: `ABIE.BAL`, `ACER.RUB`,
  `ACER.SAH`, `BETU.ALL`, `BETU.PAP`, `FAGU.GRA`, `FRAX.NIG`,
  `LARI.LAR`, `OSTR.VIR`, `PICE.GLA`, `PICE.MAR`, `PICE.RUB`,
  `PINU.BAN`, `PINU.STR`, `POPU.GRA`, `POPU.TRE`, `QUER.RUB`,
  `THUJ.OCC`, `TILI.AME`, `TSUG.CAN`.

- BA:

  Numeric vector. Plot level. Basal area of all trees with DBH \\\ge\\
  9.1 cm in the plot (m²/ha; source "ST"). Must be `>= 0`.

- QMD:

  Numeric vector. Plot level. Quadratic mean DBH of the plot's trees
  with DBH \\\ge\\ 9.1 cm (cm; source "dmq"), used in the social status
  index `(DBH - QMD)^2`. Must be `> 0`.

- ecoregion:

  Character vector. Plot level. Québec ecological region code (« région
  écologique », Saucier et al. 1998), e.g. `"3a"` or `"6h"`;
  case-insensitive. Mapped to the four ecological subdivisions of the
  source (see Details).

- drainage:

  Character vector. Plot level. Drainage class (source "d"): `"xeric"`,
  `"mesic"`, `"subhydric"` or `"hydric"`; French `"xérique"`,
  `"mésique"`, `"subhydrique"`, `"hydrique"` (with or without accents)
  are accepted. CFSForestTools derives it from the physical environment
  (« milieu physique ») of the ecological type: 0 = xeric, 1–3 = mesic,
  4–6 = subhydric, 7–9 = hydric.

- MAT:

  Numeric vector. Plot level. Mean annual temperature, 1971–2000 normal
  (°C; source "T", from BioSIM).

- disturbance:

  Character vector. Plot level. Light disturbance since the previous
  measurement (source "f"; partial cut or natural disturbance such as
  partial windthrow or light insect outbreak): `"anthropique"`
  (`"anthropogenic"`), `"naturelle"` (`"natural"`) or `"aucune"`
  (`"none"`).

- keep_model_id:

  Logical. If `TRUE`, add column `model_id` (`"fortin2009"`).

## Value

A tibble with column `height` (m), plus `model_id` if
`keep_model_id = TRUE`.

## Details

**Model form** (equations 6 and 7, p. 7): \$\$h = 1.3 + A \ln(DBH + 1) +
B \ln(DBH + 1)^2,\$\$ \$\$A = \beta\_{1,s} + \beta\_{2,s} BA + \beta_3
MAT + \beta\_{4,d} + \beta\_{5,e} + \beta\_{6,f} + \beta_7 ISS,\$\$
\$\$B = \beta\_{8,s} + \beta\_{9,s} BA + \beta\_{10} ISS,\$\$ \$\$ISS =
(DBH - QMD)^2,\$\$ where \\s\\ is the species, \\d\\ the drainage class,
\\e\\ the ecological subdivision and \\f\\ the disturbance class. Source
symbols map to arguments as: ST = `BA`, dmq = `QMD`, T = `MAT`, ISS («
indice de statut social ») is computed from `DBH` and `QMD`. Height is
not transformed, so no back-transformation applies.

**Population-level prediction:** the fixed effects with the plot random
effect and residual error set to zero, as in the source's validation
(eq. 9, p. 8). Plot calibration with measured heights (the source
publishes the variance components) is not implemented.

**Coefficients:** full-precision estimates from the CFSForestTools
implementation of this model (`fortin2009generalhdrelationship`,
`0_HDRelationBeta.csv`; LGPL-3). They are the same fit as Tableaux 4 and
5 (pp. 11–12) and agree with the printed values to the printed precision
(largest difference 5e-5). Predictions match CFSForestTools to within
1e-8 m.

**Ecological subdivision:** the source groups Québec ecological regions
(Saucier et al. 1998) into four subdivisions, shown only as a map
(Figure 2, p. 8). As in CFSForestTools, `ecoregion` takes the ecological
region code and is mapped as follows:

- Ouest (West): 1a, 2a, 3a, 3b, 4a, 4b, 4c, 5a, 5b, 5c, 5d, 6a–6g.

- Centre: 2b, 2c, 3c, 3d, 4d, 4e, 4f, 5e, 5f.

- Nord-Est (Northeast): 6h–6l, 6n–6s, 7a, 7b, 7c.

- Sud-Est (Southeast): 4g, 4h, 5g, 5h, 5i, 5j, 6m.

**Figure 5:** the effect plots on p. 13 are reproduced within about 0.05
m, except panel (e) (social status index). For a tree 10 cm larger or
smaller than `QMD`, the published coefficients give about 13.2 m while
the figure shows about 12.7 m. This function follows the coefficients
(as CFSForestTools does).

**Applicability:** fitted to merchantable trees (DBH \\\ge\\ 9.1 cm) of
the Québec permanent sample plots, 1970–2006. Trees with DBH \< 9.1 cm
are computed but reported as outside the range, and their predicted
height may be below 1.3 m. Inputs outside the ranges of the fitting data
(Tableau 2: per-species DBH; Tableau 1: `BA` 0.2–66.9 m²/ha, `MAT` −4.7
to 6.8 °C) are computed and reported in a single warning. Rows with `NA`
in any input return `NA` height.

## References

Fortin, M., Bernier, S., Saucier, J.-P., & Labbé, F. (2009). Une
relation hauteur-diamètre tenant compte de l'influence de la station et
du climat pour 20 espèces commerciales du Québec. Gouvernement du
Québec, ministère des Ressources naturelles et de la Faune, Direction de
la recherche forestière. Mémoire de recherche forestière no 153. 22 p.

## See also

[`hd_auger2016()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_auger2016.md),
[`hd_model_registry()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_model_registry.md),
[`get_hd_model_info()`](https://ptompalski.github.io/CanadaForestAllometry/reference/get_hd_model_info.md)

## Examples

``` r
# Reference tree of Figure 5 (balsam fir, Centre subdivision): about 13.4 m
hd_fortin2009(
  DBH = 20, species = "ABIE.BAL", BA = 20, QMD = 20, ecoregion = "4d",
  drainage = "mesic", MAT = 0, disturbance = "none"
)
#> # A tibble: 1 × 1
#>   height
#>    <dbl>
#> 1   13.4

# Inside dplyr::mutate()
trees <- data.frame(
  DBH = c(14, 22, 30),
  species = c("PICE.MAR", "ABIE.BAL", "BETU.ALL"),
  BA = 22, QMD = 18, ecoregion = c("5a", "5a", "3b"),
  drainage = c("subhydrique", "mesique", "mesique"), MAT = c(0.5, 0.5, 3),
  disturbance = "aucune"
)
dplyr::mutate(
  trees,
  hd_fortin2009(DBH, species, BA, QMD, ecoregion, drainage, MAT, disturbance)
)
#>   DBH  species BA QMD ecoregion    drainage MAT disturbance   height
#> 1  14 PICE.MAR 22  18        5a subhydrique 0.5      aucune 12.17414
#> 2  22 ABIE.BAL 22  18        5a     mesique 0.5      aucune 15.47118
#> 3  30 BETU.ALL 22  18        3b     mesique 3.0      aucune 17.88935
```
