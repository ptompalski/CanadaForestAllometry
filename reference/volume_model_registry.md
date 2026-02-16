# Volume model registry

Returns a registry (metadata table) describing tree volume models
available in CanadaForestAllometry. The registry is used by wrapper
functions to (1) discover models, (2) select the best applicable model
for a given tree, and (3) select the correct model function.

## Usage

``` r
volume_model_registry()
```

## Value

A tibble with one row per model and metadata fields used for model
selection.
