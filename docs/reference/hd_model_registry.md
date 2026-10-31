# DBH–height model registry

Returns a registry (metadata table) of the DBH–height (`hd_*()`) models
available in CanadaForestAllometry, one row per model variant. Use it
with
[`get_hd_model_info()`](https://ptompalski.github.io/CanadaForestAllometry/reference/get_hd_model_info.md)
and
[`find_hd_models()`](https://ptompalski.github.io/CanadaForestAllometry/reference/find_hd_models.md)
to discover which model suits your data, then call the corresponding
`hd_*()` function directly, or let
[`hd()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd.md)
select a model automatically (using `rank`).

## Usage

``` r
hd_model_registry()
```

## Value

A tibble with one row per model variant and columns:

- `model_id`:

  Stable model identifier (with variant suffix).

- `engine`:

  Name of the `hd_*()` function implementing the model.

- `rank`:

  Preference used by
  [`hd()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd.md)
  when several models are eligible (higher = preferred). Only meaningful
  among models covering the same jurisdiction and species.

- `fixed_args`:

  List-column of arguments selecting the variant.

- `reference`:

  BibTeX key of the source publication.

- `scope`:

  `"provincial"` or `"regional"`.

- `province_scope`:

  List-column of jurisdiction codes.

- `stand_origin`:

  `"natural"`, `"plantation"` or `"both"`.

- `bc_region`:

  For British Columbia models fitted to only part of the province:
  `"coastal"` or `"interior"`; `NA` otherwise. Used by
  [`hd()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd.md).

- `equation_family`:

  Model form (e.g. `"chapman_richards"`).

- `required_predictors`, `optional_predictors`:

  List-columns of argument names (excluding `species`).

- `region_arg`:

  Name of the model-specific region argument, or `NA`.

- `prediction_types`:

  List-column; currently `"population"`.

- `calibration_published`:

  Whether the source publishes the variance components needed for local
  (plot) calibration.

- `params_key`:

  Internal parameter table name.

- `species_manual`:

  List-column of species, used only when coefficients are not stored in
  a parameter table.

- `description`:

  One-line description.

## Details

All models predict total tree height (m) from DBH (cm) at the population
level (random effects set to zero for mixed-effects sources).

## See also

[`get_hd_model_info()`](https://ptompalski.github.io/CanadaForestAllometry/reference/get_hd_model_info.md),
[`find_hd_models()`](https://ptompalski.github.io/CanadaForestAllometry/reference/find_hd_models.md),
[`hd()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd.md)

## Examples

``` r
hd_model_registry()
#> # A tibble: 27 × 18
#>    model_id             engine  rank fixed_args   reference scope province_scope
#>    <chr>                <chr>  <dbl> <list>       <chr>     <chr> <list>        
#>  1 huang1994            hd_hu…    30 <list [0]>   @Huang19… prov… <chr [1]>     
#>  2 huang2016            hd_hu…    70 <list [0]>   @Huang20… prov… <chr [1]>     
#>  3 huang2016b           hd_hu…    90 <list [0]>   @Huang20… prov… <chr [1]>     
#>  4 sharmaparton2007     hd_sh…    80 <list [0]>   @SharmaP… regi… <chr [1]>     
#>  5 sharma2016_natural   hd_sh…    85 <named list> @Sharma2… prov… <chr [1]>     
#>  6 sharma2016_plantati… hd_sh…    85 <named list> @Sharma2… prov… <chr [1]>     
#>  7 sharmazhang2004_eq2  hd_sh…    75 <named list> @SharmaZ… prov… <chr [1]>     
#>  8 sharmazhang2004_eq5  hd_sh…    45 <named list> @SharmaZ… prov… <chr [1]>     
#>  9 eslamdoust2026_m4    hd_es…    55 <list [0]>   @Eslamdo… prov… <chr [1]>     
#> 10 eslamdoust2026_m5    hd_es…    90 <list [0]>   @Eslamdo… prov… <chr [1]>     
#> # ℹ 17 more rows
#> # ℹ 11 more variables: stand_origin <chr>, bc_region <chr>,
#> #   equation_family <chr>, required_predictors <list>,
#> #   optional_predictors <list>, region_arg <chr>, prediction_types <list>,
#> #   calibration_published <lgl>, params_key <chr>, species_manual <list>,
#> #   description <chr>
```
