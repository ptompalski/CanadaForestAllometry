# DBH–height model registry

Returns a registry (metadata table) of the DBH–height (`hd_*()`) models
available in CanadaForestAllometry, one row per model variant. Use it
with
[`get_hd_model_info()`](https://ptompalski.github.io/CanadaForestAllometry/reference/get_hd_model_info.md)
and
[`find_hd_models()`](https://ptompalski.github.io/CanadaForestAllometry/reference/find_hd_models.md)
to discover which model suits your data, then call the corresponding
`hd_*()` function directly. There is no automatic model selection.

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
[`find_hd_models()`](https://ptompalski.github.io/CanadaForestAllometry/reference/find_hd_models.md)

## Examples

``` r
hd_model_registry()
#> # A tibble: 8 × 16
#>   model_id       engine fixed_args   reference scope province_scope stand_origin
#>   <chr>          <chr>  <list>       <chr>     <chr> <list>         <chr>       
#> 1 huang2016      hd_hu… <list [0]>   @Huang20… prov… <chr [1]>      natural     
#> 2 sharmaparton2… hd_sh… <list [0]>   @SharmaP… regi… <chr [1]>      both        
#> 3 eslamdoust202… hd_es… <list [0]>   @Eslamdo… prov… <chr [1]>      natural     
#> 4 eslamdoust202… hd_es… <list [0]>   @Eslamdo… prov… <chr [1]>      natural     
#> 5 auger2016      hd_au… <list [0]>   @Auger20… prov… <chr [1]>      natural     
#> 6 auger2025_dbh  hd_au… <named list> @Auger20… prov… <chr [1]>      plantation  
#> 7 auger2025_plo… hd_au… <named list> @Auger20… prov… <chr [1]>      plantation  
#> 8 auger2025_dom… hd_au… <named list> @Auger20… prov… <chr [1]>      plantation  
#> # ℹ 9 more variables: equation_family <chr>, required_predictors <list>,
#> #   optional_predictors <list>, region_arg <chr>, prediction_types <list>,
#> #   calibration_published <lgl>, params_key <chr>, species_manual <list>,
#> #   description <chr>
```
