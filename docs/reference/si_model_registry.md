# Site-index model registry

Returns a registry (metadata table) describing site-index models
available in CanadaForestAllometry. The registry supports model
discovery and can be used by higher-level wrappers to select candidate
models by jurisdiction/species and required inputs.

## Usage

``` r
si_model_registry()
```

## Value

A tibble with one row per site-index model variant and metadata fields
used for model selection.
