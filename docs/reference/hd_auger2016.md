# Auger (2016) DBH–height model for 27 Québec tree species

Vectorized implementation of the population-level height–diameter
relationship of Auger (2016; Note de recherche forestière no 146) for 27
commercial tree species of Québec. Predicts total tree height (m) from
DBH (cm), plot merchantable basal area and, depending on the species,
the tree's relative DBH, climate normals, elevation and Québec
ecological classification codes.

## Usage

``` r
hd_auger2016(
  DBH,
  species,
  BA,
  QMD = NULL,
  bioclim_subdomain = NULL,
  physical_env = NULL,
  potential_veg = NULL,
  MAT = NULL,
  MAP = NULL,
  elevation = NULL,
  disturbance = NULL,
  keep_model_id = FALSE
)
```

## Arguments

- DBH:

  Numeric vector. Tree level. Diameter at breast height (1.3 m), in cm.
  Must be `>= 0`.

- species:

  Character vector. Tree level. NFI species codes (e.g. `"PICE.MAR"`;
  compact `"PICEMAR"` is accepted). Supported: `ABIE.BAL`, `ACER.RUB`,
  `ACER.SAC`, `ACER.SAH`, `BETU.ALL`, `BETU.PAP`, `BETU.POP`,
  `FAGU.GRA`, `FRAX.AME`, `FRAX.NIG`, `LARI.LAR`, `OSTR.VIR`,
  `PICE.GLA`, `PICE.MAR`, `PICE.RUB`, `PINU.BAN`, `PINU.RES`,
  `PINU.STR`, `POPU.BAL`, `POPU.GRA`, `POPU.TRE`, `PRUN.SER`,
  `QUER.RUB`, `THUJ.OCC`, `TILI.AME`, `TSUG.CAN`, `ULMU.AME`.

- BA:

  Numeric vector. Plot level. Merchantable basal area of the plot
  (m²/ha; source "ST", « surface terrière marchande dans la placette »,
  trees with DBH \> 9 cm, all species). Must be `>= 0`. Required for all
  species.

- QMD:

  Optional numeric vector. Plot level. Quadratic mean DBH (cm) of the
  plot's merchantable trees. Used to compute the relative DBH (RDHP =
  `DBH / QMD`). Must be `> 0`. Required for 21 species.

- bioclim_subdomain:

  Optional character vector. Plot level. Bioclimatic subdomain (source
  "SDOM"; Saucier et al. 1998): `"1"`, `"2est"`, `"2ouest"`, `"3est"`,
  `"3ouest"`, `"4est"`, `"4ouest"`, `"5est"`, `"5ouest"`, `"6est"`,
  `"6ouest"`. Required for 11 species.

- physical_env:

  Optional character or integer vector. Plot level. Physical environment
  class (source "MP", « milieu physique », MRN 2013), `0` to `9`.
  Required for 10 species.

- potential_veg:

  Optional character vector. Plot level. Potential vegetation code
  (source "VP"; e.g. `"RS2"`, `"FE3"`; three characters). Required for
  15 species.

- MAT:

  Optional numeric vector. Plot level. Mean annual temperature,
  1981–2010 normal (°C; source "TMOY"). Required for 7 species.

- MAP:

  Optional numeric vector. Plot level. Mean (total) annual
  precipitation, 1981–2010 normal (mm; source "PTOT"). Must be `>= 0`.
  Required for 10 species.

- elevation:

  Optional numeric vector. Plot level. Elevation above sea level (m;
  source "ALT", altitude). Required for 4 species.

- disturbance:

  Optional character vector. Plot level. Occurrence of a light
  disturbance (« perturbation légère »): `"anthropique"`
  (`"anthropogenic"`), `"naturelle"` (`"natural"`) or `"aucune"`
  (`"none"`). Required for 5 species.

- keep_model_id:

  Logical. If `TRUE`, add column `model_id` (`"auger2016"`).

## Value

A tibble with column `height` (m), plus `model_id` if
`keep_model_id = TRUE`.

## Details

**Model form** (equation 1, p. 4): \$\$h = 1.3 + A \ln(DBH + 1) + B
\ln(DBH + 1)^2,\$\$ \$\$A = a + a\_{ST} BA + a\_{RDHP} \frac{DBH}{QMD} +
a\_{ALT} ALT + a\_{PTOT} PTOT + a\_{TMOY} TMOY + a\_{SDOM} + a\_{MP} +
a\_{VP} + a\_{PERT},\$\$ where \\B\\ is a species constant and every
covariate effect interacts with \\\ln(DBH + 1)\\. Source symbols map to
arguments as: ALT = `elevation`, PTOT = `MAP`, TMOY = `MAT`, SDOM =
`bioclim_subdomain`, MP = `physical_env`, VP = `potential_veg`, PERT =
`disturbance`. Height is not transformed, so no back-transformation
applies. Grey birch (`BETU.POP`) has no \\\ln(DBH + 1)^2\\ term in the
source (Annexe 1) and is linear in \\\ln(DBH + 1)\\.

**Species-specific predictors** (Tableau 5, pp. 10–11): `BA` is used for
all species; the other predictors only for the species whose selected
model contains them. A predictor is required only when a row's species
uses it; otherwise it may be `NULL` or `NA` and is ignored. Use
`get_hd_model_info("auger2016")` for definitions, and see the source for
the per-species predictor sets. For example, white spruce (`PICE.GLA`)
uses `BA`, `QMD`, `MAT`, `bioclim_subdomain` and `potential_veg`; silver
maple, American elm and grey birch use `BA` only.

**Population-level prediction:** the published fixed effects with the
plot random effect and residual set to zero, i.e. the marginal
prediction used by the source (p. 7). Plot calibration with measured
heights is not implemented.

**Class codes:** classes with few observations were grouped by the
author, and the group estimate is repeated for each member class
(Annexes 2–4). A class with no estimate for a species (absent from its
fitting data, e.g. physical environment 7 for red oak) gives an `NA`
height and one warning per call. Codes are matched case-insensitively.

**Coefficient precision:** coefficients are used as printed (4
decimals). The altitude and precipitation slopes therefore carry about
one significant digit; rounding can shift predictions by up to about
0.15 m.

**Applicability:** fitted to merchantable trees (DBH \\\ge\\ 9.1 cm) of
the Québec permanent and temporary inventory plots. The author advises
against use for trees with DBH \< 9 cm (p. 19); such trees are computed
but reported as outside the range, and their predicted height may be
below 1.3 m. Inputs outside the per-species ranges of the fitting data
(Tableau 1: DBH and relative DBH; Tableau 2: `BA`, `elevation`, `MAT`,
`MAP`) are computed and reported in a single warning. Rows with `NA` in
a used input return `NA` height.

**External implementation:** the `generalhdrelation2014` module of
CFSForestTools reproduces the published coefficients for 10 species
(`PRUN.SER`, `ACER.SAC`, `FRAX.AME`, `FRAX.NIG`, `LARI.LAR`, `ULMU.AME`,
`OSTR.VIR`, `PINU.STR`, `PINU.RES`, `TILI.AME`; within 0.02 m) but uses
different parameter estimates and predictor sets for the other 17
species (differences up to about 2.6 m). This function follows the
publication.

## References

Auger, I. (2016). Une nouvelle relation hauteur-diamètre tenant compte
de l'influence de la station et du climat pour 27 essences commerciales
du Québec. Gouvernement du Québec, ministère des Forêts, de la Faune et
des Parcs, Direction de la recherche forestière. Note de recherche
forestière no 146. 31 p.

## See also

[`hd_model_registry()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_model_registry.md),
[`get_hd_model_info()`](https://ptompalski.github.io/CanadaForestAllometry/reference/get_hd_model_info.md)

## Examples

``` r
# Worked example (Annexe 1): white spruce, 12.0 m
hd_auger2016(
  DBH = 20, species = "PICE.GLA", BA = 20, QMD = 15,
  bioclim_subdomain = "1", potential_veg = "FC1", MAT = 2.3
)
#> # A tibble: 1 × 1
#>   height
#>    <dbl>
#> 1   12.0

# Species needing only basal area
hd_auger2016(DBH = c(15, 30), species = c("ACER.SAC", "ULMU.AME"), BA = 25)
#> # A tibble: 2 × 1
#>   height
#>    <dbl>
#> 1   15.8
#> 2   20.4

# Inside dplyr::mutate(): supply every predictor used by the species present
trees <- data.frame(
  DBH = c(14, 22, 18),
  species = c("PICE.MAR", "ABIE.BAL", "ACER.SAH"),
  BA = 22, QMD = c(15, 15, 20),
  bioclim_subdomain = c("5ouest", "5ouest", "3ouest"), physical_env = 2,
  potential_veg = c("RS2", "RS2", "FE3"), MAT = c(1, 1, 3.5),
  MAP = 1000, elevation = 300, disturbance = "none"
)
dplyr::mutate(
  trees,
  hd_auger2016(
    DBH, species, BA, QMD, bioclim_subdomain, physical_env, potential_veg,
    MAT, MAP, elevation, disturbance
  )
)
#>   DBH  species BA QMD bioclim_subdomain physical_env potential_veg MAT  MAP
#> 1  14 PICE.MAR 22  15            5ouest            2           RS2 1.0 1000
#> 2  22 ABIE.BAL 22  15            5ouest            2           RS2 1.0 1000
#> 3  18 ACER.SAH 22  20            3ouest            2           FE3 3.5 1000
#>   elevation disturbance   height
#> 1       300        none 12.30616
#> 2       300        none 16.15196
#> 3       300        none 16.46363
```
