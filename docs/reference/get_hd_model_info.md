# Get information about a DBH–height model

Returns the registry metadata for one `hd_*()` model variant together
with its predictor table (argument, level, units, whether required, and
the source's definition of the predictor).

## Usage

``` r
get_hd_model_info(model_id)
```

## Arguments

- model_id:

  Single model identifier, as in `hd_model_registry()$model_id`.

## Value

A named list with the registry fields of the model (list-columns
unwrapped), its `species` coverage, and `predictors`, a tibble with
columns `arg`, `level`, `units`, `required`, `definition` and
`source_locator`.

## See also

[`hd_model_registry()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_model_registry.md),
[`find_hd_models()`](https://ptompalski.github.io/CanadaForestAllometry/reference/find_hd_models.md)

## Examples

``` r
ids <- hd_model_registry()$model_id
if (length(ids) > 0) get_hd_model_info(ids[[1]])
#> $model_id
#> [1] "huang2016"
#> 
#> $engine
#> [1] "hd_huang2016"
#> 
#> $fixed_args
#> list()
#> 
#> $reference
#> [1] "@Huang2016hd"
#> 
#> $scope
#> [1] "provincial"
#> 
#> $province_scope
#> [1] "AB"
#> 
#> $stand_origin
#> [1] "natural"
#> 
#> $equation_family
#> [1] "chapman_richards; log_logistic"
#> 
#> $required_predictors
#> [1] "DBH"
#> 
#> $optional_predictors
#> [1] "natural_subregion"
#> 
#> $region_arg
#> [1] "natural_subregion"
#> 
#> $prediction_types
#> [1] "population"
#> 
#> $calibration_published
#> [1] TRUE
#> 
#> $params_key
#> [1] "parameters_hd_Huang2016"
#> 
#> $species_manual
#> NULL
#> 
#> $description
#> [1] "Alberta T/609 population DBH-height models (base fits, Table 4); provincial or natural-subregion-group variants."
#> 
#> $species
#>  [1] "ABIE.BAL" "ABIE.LAS" "BETU.PAP" "LARI.LAR" "LARI.LYA" "LARI.OCC"
#>  [7] "PICE.ENG" "PICE.GLA" "PICE.MAR" "PINU.ALB" "PINU.BAN" "PINU.CON"
#> [13] "PINU.FLE" "POPU.BAL" "POPU.TRE" "PSEU.MEN"
#> 
#> $predictors
#> # A tibble: 2 × 6
#>   arg               level units               required definition source_locator
#>   <chr>             <chr> <chr>               <lgl>    <chr>      <chr>         
#> 1 DBH               tree  cm                  TRUE     Tree diam… T/609 p. 6 (e…
#> 2 natural_subregion plot  code (1-21 or e.g.… FALSE    Alberta 2… T/609 Table 2…
#> 
```
