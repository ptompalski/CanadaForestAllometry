# Predict tree height: choose best available DBH–height model or run all candidates

`hd()` is a convenience wrapper that predicts total tree height (m) from
DBH by selecting, per tree, one (or more) of the DBH–height (`hd_*()`)
models listed in
[`hd_model_registry()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_model_registry.md).
It is the DBH–height counterpart of
[`vol()`](https://ptompalski.github.io/CanadaForestAllometry/reference/vol.md).

## Usage

``` r
hd(
  DBH,
  species,
  jurisdiction,
  ...,
  bec_zone = NA_character_,
  bc_region = NA_character_,
  stand_origin = "natural",
  pick_best = TRUE,
  keep_model_id = TRUE
)
```

## Arguments

- DBH:

  Numeric vector. Diameter at breast height (cm).

- species:

  Character vector. NFI species codes (e.g. `"PICE.MAR"`).

- jurisdiction:

  Character vector. Province/territory code (e.g. `"ON"`).

- ...:

  Named predictors, using the `hd_*()` argument names (e.g. `BA`,
  `HDOM`, `TPH`, `natural_subregion`, `ecoregion`). Each must have
  length 1 or `length(DBH)`; `NA` marks the predictor as unavailable for
  that tree. See
  [`hd_model_registry()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_model_registry.md)
  for the predictors each model uses.

- bec_zone:

  Optional character vector. BC biogeoclimatic zone code (e.g. `"CWH"`,
  `"IDF"`), used to infer `bc_region`: `CWH`, `CDF`, `MH` and `CMA` are
  coastal; `BG`, `BWBS`, `ESSF`, `ICH`, `IDF`, `IMA`, `MS`, `PP`,
  `SBPS`, `SBS`, `SWB` and `BAFA` are interior. Ignored outside BC.

- bc_region:

  Optional character vector, `"coastal"` or `"interior"`. Alternative to
  `bec_zone`; if both are given they must agree. Ignored outside BC.

- stand_origin:

  Character vector, `"natural"` (default) or `"plantation"`. `NA` is
  treated as `"natural"`.

- pick_best:

  Logical. If `TRUE` (default), use the single best model per tree. If
  `FALSE`, run all eligible models and return a list (one tibble per
  tree), suitable as a list-column.

- keep_model_id:

  Logical. If `TRUE` (default), include a `model_id` column with the
  registry model ID used.

## Value

If `pick_best = TRUE`, a tibble with one row per tree and column
`height` (m), plus `model_id` if requested. Trees for which no model is
eligible or succeeds get `NA` (with a warning). Warnings are also issued
when a plantation tree uses a natural-stand model, or when a coastal- or
interior-only BC model is used without `bec_zone`/`bc_region`.

If `pick_best = FALSE`, a list with one tibble per tree containing the
outputs of every successful candidate model (`height`, and optionally
`model_id`), ordered from most to least preferred.

## Model selection logic

Selection is registry-driven and occurs per input tree:

1.  Geographic applicability: the model's `province_scope` must include
    `jurisdiction`.

2.  Species coverage: the model must have coefficients for `species`
    (exact match; no genus-level fallback).

3.  Stand origin: plantation-only models are used only when
    `stand_origin = "plantation"`. For plantation trees, models fitted
    to plantations (or to both origins) are preferred; natural-stand
    models are used only as a fallback, with a warning.

4.  British Columbia coast vs interior: models fitted to coastal or
    interior BC only (see `bc_region` in
    [`hd_model_registry()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_model_registry.md))
    are restricted to trees in that region when `bec_zone` or
    `bc_region` is given. When neither is given, they remain eligible
    and a warning is issued if one is used.

5.  Required predictors: every required predictor of the model (see
    [`get_hd_model_info()`](https://ptompalski.github.io/CanadaForestAllometry/reference/get_hd_model_info.md))
    must be supplied in `...` and non-missing for the tree. Optional
    predictors (e.g. a region code) are passed on when non-missing.

6.  Ranking: among eligible models the highest `rank` is used. If the
    model fails for a tree (e.g. a species-specific predictor is
    missing, or a region code is not recognised), the next-ranked model
    is tried.

## Important limitations and recommended use

- Predictor definitions differ between sources. For example, basal area
  (`BA`) may be all-species or species-specific and computed with
  different minimum DBH thresholds; dominant height (`HDOM`) is defined
  differently by each source. `hd()` passes the same value to every
  model, so check the definitions with
  [`get_hd_model_info()`](https://ptompalski.github.io/CanadaForestAllometry/reference/get_hd_model_info.md)
  before supplying stand-level predictors.

- The ranking is a pragmatic default (more stand information, newer and
  more local models are preferred). If you require a specific model,
  call the `hd_*()` function directly.

- Run with `pick_best = FALSE` during exploratory analyses to inspect
  which models are eligible and how sensitive results are to model
  choice.

- Models' out-of-range warnings are passed through unchanged; their
  informational messages (e.g. which internal variant was used) are
  suppressed, since `model_id` reports the model used.

## See also

[`hd_model_registry()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_model_registry.md),
[`find_hd_models()`](https://ptompalski.github.io/CanadaForestAllometry/reference/find_hd_models.md),
[`get_hd_model_info()`](https://ptompalski.github.io/CanadaForestAllometry/reference/get_hd_model_info.md)

## Examples

``` r
## DBH only: best available DBH-only model
hd(DBH = c(12, 25), species = "PICE.MAR", jurisdiction = c("ON", "AB"))
#> # A tibble: 2 × 2
#>   height model_id           
#>    <dbl> <chr>              
#> 1   10.9 sharmazhang2004_eq5
#> 2   19.2 huang2016          

## Stand predictors unlock stand-level models where available
hd(
  DBH = 20, species = "PICE.MAR", jurisdiction = "ON",
  HDOM = 18, BA = 25, TPH = 1200
)
#> # A tibble: 1 × 2
#>   height model_id          
#>    <dbl> <chr>             
#> 1   16.5 sharma2016_natural

## BC: the BEC zone (or bc_region) selects coastal vs interior models
hd(DBH = c(30, 30), species = "PSEU.MEN", jurisdiction = "BC", bec_zone = c("CWH", "IDF"))
#> # A tibble: 2 × 2
#>   height model_id                
#>    <dbl> <chr>                   
#> 1   22.7 staudhammerlemay2000_dbh
#> 2   22.0 temesgen2004_m2         

## Compare all eligible models
hd(
  DBH = 20, species = "PICE.MAR", jurisdiction = "ON",
  HDOM = 18, BA = 25, TPH = 1200, pick_best = FALSE
)[[1]]
#> # A tibble: 4 × 2
#>   height model_id           
#>    <dbl> <chr>              
#> 1   16.5 sharma2016_natural 
#> 2   16.5 sharmaparton2007   
#> 3   20.4 sharmazhang2004_eq2
#> 4   16.1 sharmazhang2004_eq5

## Inside dplyr::mutate()
trees <- tibble::tibble(
  DBH = c(15, 22, 30),
  species = c("PINU.BAN", "PICE.GLA", "PICE.MAR"),
  jurisdiction = c("ON", "AB", "AB"),
  natural_subregion = c(NA, "CM", "LF")
)
trees |>
  dplyr::mutate(hd(DBH, species, jurisdiction,
    natural_subregion = natural_subregion
  ))
#> # A tibble: 3 × 6
#>     DBH species  jurisdiction natural_subregion height model_id         
#>   <dbl> <chr>    <chr>        <chr>              <dbl> <chr>            
#> 1    15 PINU.BAN ON           NA                  14.5 zhang2002_overall
#> 2    22 PICE.GLA AB           CM                  21.3 huang2016        
#> 3    30 PICE.MAR AB           LF                  21.1 huang2016        
```
