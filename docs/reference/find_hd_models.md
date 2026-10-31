# Find DBH–height models usable with your data

Filters
[`hd_model_registry()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_model_registry.md)
by jurisdiction and species, and reports which required predictors each
candidate model needs that you do not have. Models with missing
predictors are kept (as near-misses) so you can see what extra data
would make them usable. No model is selected automatically; see
[`hd()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd.md)
for automatic selection.

## Usage

``` r
find_hd_models(jurisdiction = NULL, species = NULL, available = "DBH")
```

## Arguments

- jurisdiction:

  Optional character vector of province/territory codes (e.g. `"ON"`).
  Models covering any of them are kept. `NULL` (default) applies no
  filter.

- species:

  Optional character vector of NFI species codes (e.g. `"PICE.MAR"`).
  Models covering any of them are kept. `NULL` (default) applies no
  filter.

- available:

  Character vector of predictor argument names you can supply (e.g.
  `c("DBH", "BA", "TPH")`). Names follow the `hd_*()` argument names,
  which are case-sensitive.

## Value

The matching rows of
[`hd_model_registry()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_model_registry.md)
with species coverage (`species`, `n_species`, `species_text`), plus
`missing_predictors` (list-column of required predictors not in
`available`) and `usable` (logical). Usable models are listed first.

## See also

[`hd_model_registry()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_model_registry.md),
[`get_hd_model_info()`](https://ptompalski.github.io/CanadaForestAllometry/reference/get_hd_model_info.md)

## Examples

``` r
find_hd_models(jurisdiction = "ON", species = "PICE.MAR",
               available = c("DBH", "BA", "TPH"))
#> # A tibble: 5 × 23
#>   model_id engine  rank fixed_args   reference scope province_scope stand_origin
#>   <chr>    <chr>  <dbl> <list>       <chr>     <chr> <list>         <chr>       
#> 1 sharmaz… hd_sh…    75 <named list> @SharmaZ… prov… <chr [1]>      both        
#> 2 sharmaz… hd_sh…    45 <named list> @SharmaZ… prov… <chr [1]>      both        
#> 3 sharmap… hd_sh…    80 <list [0]>   @SharmaP… regi… <chr [1]>      both        
#> 4 sharma2… hd_sh…    85 <named list> @Sharma2… prov… <chr [1]>      natural     
#> 5 sharma2… hd_sh…    85 <named list> @Sharma2… prov… <chr [1]>      plantation  
#> # ℹ 15 more variables: bc_region <chr>, equation_family <chr>,
#> #   required_predictors <list>, optional_predictors <list>, region_arg <chr>,
#> #   prediction_types <list>, calibration_published <lgl>, params_key <chr>,
#> #   species_manual <list>, description <chr>, species <list>, n_species <int>,
#> #   species_text <chr>, missing_predictors <list>, usable <lgl>
```
